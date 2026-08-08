-- | The stable render contract (PEP-22): a thread, as JSON.
--
-- WHAT THIS IS FOR. PEP-22 calls it "the deliverable that keeps the web layer a
-- pure view": a versioned, read-only, serialized projection of the fold that a
-- renderer consumes without touching hbs2, crypto or the event log. Until this
-- existed there was no machine-readable output at all -- @grep -ri json
-- hbs2-hub@ found nothing -- so a web UI had nothing to read and every reader
-- would have had to parse the columns meant for a person.
--
-- DERIVED AND NEVER A SOURCE OF TRUTH. Everything here comes out of
-- 'ThreadState', which comes out of the fold, which comes out of canon. Two
-- clones produce the same bytes from the same log. There is no cache to
-- invalidate and nothing to keep in step.
--
-- NO @verified@ FIELD, which is PEP-22's own reasoning and worth repeating
-- where somebody might add one: everything in the contract has already passed
-- the PEP-19 admission check. A bad signature or an unauthorized canon key is
-- dropped by the fold and never materialized, so presence IS verification.
-- Auditing what was dropped is @hub verify@'s job.
--
-- READ-ONLY AND CARRIES NO CAPABILITY. The one thing here that looks like a key
-- is @part_secret@, and it is not one: PEP-18/19 publish it INTO canon on
-- purpose, so that any clone can fetch and decrypt an attachment the owner
-- accepted. It opens what canon already published and authorizes nothing.
--
-- A REDACTED ITEM CARRIES NO TEXT. PEP-22 says redacted items render as
-- withheld rather than removed, and this withholds them HERE rather than
-- marking them and trusting the renderer: the point of a redaction is that the
-- text stops being shown, and a contract that ships the body with a boolean
-- beside it has published it to every renderer that forgets to read the
-- boolean. The flag stays, so a renderer can say that something was withdrawn.
module HBS2.Hub.Render
  ( contractVersion
  , PRDiff(..)
  , DiffAvailability(..)
  , threadContract
  , renderContract
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Fold (ThreadState(..),Comment(..),PRState(..),tsTitle)

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef)
import HBS2.Prelude.Plated (Pretty(..))

import Data.Aeson (Value(..),object,(.=),encode)
import Data.Aeson.Encode.Pretty (encodePretty',defConfig,Config(..),Indent(..)
                                ,keyOrder)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

-- | The version a renderer checks before it reads anything else.
--
-- Bumped when a field changes meaning or goes away, never for an addition: a
-- renderer that ignores what it does not know keeps working, which is the whole
-- point of putting a number here.
contractVersion :: Int
contractVersion = 1

-- | Whether the objects a pull request proposes can be shown.
--
-- Three states because the objects may or may not be present, and the two ways
-- of being absent call for different offers from a renderer: one is recoverable
-- at a cost and the other is not.
data DiffAvailability
  = DiffAvailable        -- ^ the objects are in this repository
  | DiffReconstructable  -- ^ gone, but rebuildable from the bundle attachment
  | DiffUnavailable      -- ^ neither present nor rebuildable
  deriving stock (Eq,Show)

-- | The precomputed diff, so a static renderer needs no git.
--
-- Supplied by the CALLER and not computed here, which is what keeps this module
-- pure and testable with no repository: running git belongs to the layer that
-- already has one. A 'Nothing' diff is a caller that did not look, and is
-- rendered as 'DiffUnavailable' with no text -- honest, since nothing was
-- established either way.
data PRDiff = PRDiff
  { pdAvailability :: DiffAvailability
  , pdTruncated    :: Bool
  , pdText         :: Text
  }
  deriving stock (Eq,Show)

-- | One thread, as the contract.
--
-- Pure. Give it what the fold materialized and, for a pull request, whatever
-- the caller could work out about the objects.
threadContract :: ThreadState -> Maybe PRDiff -> Value
threadContract t mdiff = object $
  [ "contract"    .= contractVersion
  , "kind"        .= kindOf (tsKind t)
  , "number"      .= tsNumber t
  , "thread_id"   .= b58 (tsId t)
  , "title"       .= withheld (tsRedacted t) (tsTitle t)
  , "status"      .= attr "status" "open"
  , "labels"      .= multi "labels"
  , "assignees"   .= multi "assignees"
  , "author"      .= key58 (tsAuthor t)
  , "canon_by"    .= key58 (tsCanonBy t)
  , "created_at"  .= tsCreated t
  , "declared_at" .= tsAuthorTs t
  , "updated_at"  .= tsUpdated t
  , "redacted"    .= tsRedacted t
  , "body"        .= hide (tsBody t)
  , "body_part"   .= fmap b58 (hide (tsBodyPart t))
  , "part_secret" .= fmap secret58 (hide (tsPartSecret t))
    -- What the AUTHOR asked for, never applied. Kept separate from `labels`
    -- for the reason PEP-19 makes applying one an owner-signed event: merging
    -- the two would let a stranger label their own issue.
  , "labels_requested" .= tsLabelsRequested t
  , "origin"      .= fmap b58 (tsOrigin t)
  , "comments"    .= fmap commentContract (tsComments t)
  ]
  <> maybe [] (\pr -> ["pr" .= prContract pr mdiff]) (tsPR t)
  where
    attr k d = fromMaybe d (HM.lookup k (tsAttrs t))
    multi k  = maybe [] decodeLabels (HM.lookup k (tsAttrs t))

    -- The title of a redacted thread is withheld like its body: a title is
    -- text the same stranger wrote, and it is the part a list view shows.
    withheld True  _ = Null
    withheld False x = String x

    hide :: Maybe a -> Maybe a
    hide x = if tsRedacted t then Nothing else x

commentContract :: Comment -> Value
commentContract c = object
  [ "event_id"    .= b58 (cId c)
  , "author"      .= key58 (cAuthor c)
  , "canon_by"    .= key58 (cCanonBy c)
    -- The name is historical and PEP-22 says so: this is folded-ts, the same
    -- trusted clock as `created_at`, and what orders the list.
  , "ts"          .= cFoldedTs c
  , "declared_at" .= cAuthorTs c
  , "reply_to"    .= fmap b58 (cReplyTo c)
  , "body"        .= hide (cBody c)
  , "body_part"   .= fmap b58 (hide (cBodyPart c))
  , "part_secret" .= fmap secret58 (hide (cPartSecret c))
  , "origin"      .= fmap b58 (cOrigin c)
  , "redacted"    .= cRedacted c
  ]
  where
    hide :: Maybe a -> Maybe a
    hide x = if cRedacted c then Nothing else x

prContract :: PRState -> Maybe PRDiff -> Value
prContract pr mdiff = object
  [ "onto"        .= prOnto co
  , "base"        .= prBase co
  , "tip"         .= prSourceTip co
  , "source"      .= prSource co
  , "source_ref"  .= prSourceRef co
  , "bundle_part" .= fmap (b58 . ptPart) (prBundle co)
  , "part_secret" .= fmap secret58 (psPartSecret pr)
  , "merge_commit" .= fmap fst (psMerge pr)
  , "merged_into"  .= fmap snd (psMerge pr)
    -- Who supplied THESE coordinates, which is not always the thread's author:
    -- PEP-19 lets a maintainer revise a pull request, so a thread attributed to
    -- one person can point at a tip another one chose.
  , "coords_author"   .= key58 (psAuthor pr)
  , "coords_canon_by" .= key58 (psCanonBy pr)
  , "diff"        .= diffContract mdiff
  ]
  where co = psCoords pr

diffContract :: Maybe PRDiff -> Value
diffContract Nothing = object
  [ "format" .= ("unified" :: Text)
  , "availability" .= ("unavailable" :: Text)
  , "truncated" .= False
  , "text" .= ("" :: Text)
  ]
diffContract (Just d) = object
  [ "format" .= ("unified" :: Text)
  , "availability" .= case pdAvailability d of
      DiffAvailable       -> "available" :: Text
      DiffReconstructable -> "reconstructable"
      DiffUnavailable     -> "unavailable"
  , "truncated" .= pdTruncated d
  , "text" .= pdText d
  ]

kindOf :: HubKind -> Text
kindOf = \case
  HubIssue -> "issue"
  HubPR    -> "pr"

b58 :: Pretty a => a -> Text
b58 = Text.pack . show . pretty

key58 :: HubKey -> Text
key58 = Text.pack . show . pretty . AsBase58

secret58 :: PartSecret -> Text
secret58 = Text.pack . show . pretty . AsBase58 . partSecretBytes

-- | The contract as bytes, in a field order a person can also read.
--
-- Pretty-printed and key-ordered on purpose. This is a wire format, so a stable
-- byte layout means two clones of one canon produce identical files and a diff
-- of two exports is a diff of what changed rather than of how the encoder felt.
-- Aeson's default object order is the hash order of the keys, which is neither
-- stable across versions nor readable.
--
-- Control characters need no special handling HERE and that is a property of
-- JSON, not luck: a title with an ESC in it is emitted as @\\u001b@, so the
-- bytes survive exactly and no terminal ever sees the escape. PEP-22's warning
-- about sanitizing applies to the renderers that print this, not to this.
renderContract :: Value -> ByteString
renderContract = encodePretty' cfg
  where
    cfg = defConfig
      { confIndent = Spaces 2
      , confCompare = keyOrder
          [ "contract", "kind", "number", "thread_id", "title", "status"
          , "labels", "assignees", "author", "canon_by"
          , "created_at", "declared_at", "updated_at", "redacted"
          , "body", "body_part", "part_secret", "labels_requested"
          , "origin", "pr", "comments"
          , "event_id", "ts", "reply_to"
          , "onto", "base", "tip", "source", "source_ref"
          , "bundle_part", "merge_commit", "merged_into"
          , "coords_author", "coords_canon_by", "diff"
          , "format", "availability", "truncated", "text"
          ]
      , confTrailingNewline = True
      }

-- Kept so a caller that only wants compact bytes has them without importing
-- aeson itself.
_compact :: Value -> ByteString
_compact = encode
