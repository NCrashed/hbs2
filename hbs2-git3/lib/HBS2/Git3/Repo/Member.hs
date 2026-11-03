{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module HBS2.Git3.Repo.Member (
    MemberKeyError (..),
    MemberAddMode (..),
    MemberAddResult (..),
    parseMemberKey,
    renderMemberKey,
    membersToAdd,
    memberAddDryRun,
    addRepoMembers,
    listRepoMembers,
) where

import HBS2.Git3.Prelude
import HBS2.Git3.Repo.Tools (updateGroupKey)
import HBS2.Git3.State

import HBS2.KeyMan.Keys.Direct
import HBS2.Net.Auth.GroupKeySymm

import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List qualified as List

-- Raw signing and encryption public keys have the same untagged wire shape.
-- Requiring the prefix makes an accidental paste of a signing key fail at the
-- command boundary instead of silently adding unusable key material.
data MemberKeyError
  = MemberKeyNeedsEncryptionPrefix
  | MemberEncryptionKeyMalformed String
  deriving stock (Eq, Typeable)

instance Show MemberKeyError where
  show MemberKeyNeedsEncryptionPrefix =
    "repository member keys must be explicitly typed as enc:<base58-key>"
  show (MemberEncryptionKeyMalformed raw) =
    "malformed repository encryption member key: " <> raw

instance Exception MemberKeyError

data MemberAddMode = ApplyMemberAdd | DryRunMemberAdd
    deriving stock (Eq, Show)

memberAddDryRun :: MemberAddMode -> Bool
memberAddDryRun ApplyMemberAdd = False
memberAddDryRun DryRunMemberAdd = True

data MemberAddResult = MemberAddResult
    { memberPreviousGroupKey :: HashRef
    , memberCurrentGroupKey :: Maybe HashRef
    , memberAdded :: [PubKey 'Encrypt 'HBS2Basic]
    , memberAll :: [PubKey 'Encrypt 'HBS2Basic]
    , memberDryRun :: Bool
    }
    deriving stock (Eq, Show)

parseMemberKey :: String -> Either MemberKeyError (PubKey 'Encrypt 'HBS2Basic)
parseMemberKey raw = case List.stripPrefix "enc:" raw of
    Nothing -> Left MemberKeyNeedsEncryptionPrefix
    Just payload ->
        maybe
            (Left $ MemberEncryptionKeyMalformed raw)
            Right
            (fromStringMay payload)

renderMemberKey :: PubKey 'Encrypt 'HBS2Basic -> String
renderMemberKey = ("enc:" <>) . show . pretty . AsBase58

membersToAdd ::
    HashSet (PubKey 'Encrypt 'HBS2Basic) ->
    [PubKey 'Encrypt 'HBS2Basic] ->
    HashSet (PubKey 'Encrypt 'HBS2Basic)
membersToAdd current requested = HS.fromList requested `HS.difference` current

sortedMembers ::
    HashSet (PubKey 'Encrypt 'HBS2Basic) ->
    [PubKey 'Encrypt 'HBS2Basic]
sortedMembers = List.sortOn renderMemberKey . HS.toList

listRepoMembers ::
    (HBS2GitPerks m) =>
    GitRepoKey ->
    Git3 m (HashRef, [PubKey 'Encrypt 'HBS2Basic])
listRepoMembers repo = do
    setGitRepoKey repo
    waitRepo Nothing repo
    (gkh, gk) <- getGK >>= orThrowUser "repository is not encrypted"
    pure (gkh, sortedMembers $ HM.keysSet $ recipients gk)

addRepoMembers ::
    (HBS2GitPerks m) =>
    MemberAddMode ->
    GitRepoKey ->
    [PubKey 'Encrypt 'HBS2Basic] ->
    Git3 m MemberAddResult
addRepoMembers mode repo requested = do
    setGitRepoKey repo
    waitRepo Nothing repo
    (oldHash, oldGroupKey) <- getGK >>= orThrowUser "repository is not encrypted"

    let current = HM.keysSet (recipients oldGroupKey)
    let addedSet = membersToAdd current requested
    let allSet = current <> addedSet
    let result currentHash =
            MemberAddResult
                { memberPreviousGroupKey = oldHash
                , memberCurrentGroupKey = currentHash
                , memberAdded = sortedMembers addedSet
                , memberAll = sortedMembers allSet
                , memberDryRun = memberAddDryRun mode
                }

    if HS.null addedSet
        then pure $ result (Just oldHash)
        else do
            secret <-
                liftIO (runKeymanClientRO $ extractGroupKeySecret oldGroupKey)
                    >>= orThrow (GitRepoGroupKeySecretNotFound oldHash)
            case mode of
                DryRunMemberAdd -> pure $ result Nothing
                ApplyMemberAdd -> do
                    sto <- getStorage
                    newGroupKey <- generateGroupKey @'HBS2Basic (Just secret) (HS.toList allSet)
                    newHash <- writeAsMerkle sto (serialise newGroupKey) <&> HashRef
                    updateGroupKey repo newHash
                    pure $ result (Just newHash)
