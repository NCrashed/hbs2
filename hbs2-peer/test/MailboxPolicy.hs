{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What a policy file that this build does not understand means.
--
-- It used to mean nothing at all. The clause loop ended in a @_ -> pure ()@
-- catch-all and the function returned @Just@ regardless, so one misspelled word
-- was dropped in silence and @loadPolicyContent@ reported a policy it had
-- successfully read. The mailbox then ran on whatever its DEFAULT action said,
-- which is not what the file asked for and not what anybody was told. The
-- @orThrowUser "invalid policy"@ guards in hbs2-cli were unreachable for the
-- same reason.
--
-- A policy is the thing that decides who may put a message in a mailbox, so
-- "some of this file was ignored" is not a state it may be in quietly. The whole
-- file is refused now, and the callers fall back to 'defaultBasicPolicy', which
-- is deny/deny.
module MailboxPolicy (mailboxPolicyTests) where

import HBS2.Peer.Proto.Mailbox.Policy.Basic
import HBS2.Net.Auth.Schema (CryptoScheme(..))

import Data.Config.Suckless
import Data.List (isInfixOf)
import Prettyprinter (pretty)

import Test.Tasty
import Test.Tasty.HUnit

type S = 'HBS2Basic

-- A policy as somebody would write it into a file, read back through the
-- policy's own renderer. Rendered rather than compared as a value only because
-- 'BasicPolicy' has Eq and no Show, and a test that cannot print what it got is
-- a test nobody can act on.
parsed :: String -> IO (Maybe String)
parsed s = case parseTop s of
  Left e   -> assertFailure ("the fixture itself does not parse: " <> show e)
  Right sy -> fmap (show . pretty) <$> parseBasicPolicy @S sy

rendered :: BasicPolicy S -> Maybe String
rendered = Just . show . pretty

mailboxPolicyTests :: TestTree
mailboxPolicyTests = testGroup "mailbox policy: a file it cannot read is not a policy"
  [ testCase "reads the clauses it knows" $ do
      p <- parsed "(peer allow all)\n(sender deny all)\n"
      p @?= rendered (BasicPolicy Allow Deny mempty mempty 0)

  , testCase "refuses a clause it does not know instead of dropping it" $ do
      -- THE ONE THAT MATTERS. One letter, and the whole rule used to vanish
      -- while the file still reported as a valid policy.
      parsed "(peer alow all)\n"                    >>= (@?= Nothing)
      parsed "(peer allow all)\n(sender)\n"         >>= (@?= Nothing)
      parsed "(nonsense)\n"                         >>= (@?= Nothing)
      -- and a good clause beside a bad one does not rescue the file
      parsed "(peer allow all)\n(sendr deny all)\n" >>= (@?= Nothing)

  , testCase "an empty file is a policy, and it is the closed one" $ do
      -- Nothing to misread, so this is a policy rather than a refusal -- and
      -- what it says is deny/deny, which is the same answer the callers reach
      -- when a file IS refused. The two paths agree.
      parsed "" >>= (@?= rendered (defaultBasicPolicy @S))

  , testCase "layout is not part of the format" $ do
      -- parseTop makes a list per LINE, so several clauses on one line arrive as
      -- one form containing them, and a file may have been re-flowed by
      -- anything. Without flattening, the refusal above would have rejected a
      -- working policy written on a single line.
      one   <- parsed "(peer allow all) (sender deny all)"
      many' <- parsed "(peer allow all)\n(sender deny all)\n"
      one @?= many'
      one @?= rendered (BasicPolicy Allow Deny mempty mempty 0)

  , testCase "(pow D) is read, and written only when it charges" $ do
      parsed "(peer allow all)\n(sender deny all)\n(pow 20)\n"
        >>= (@?= rendered (BasicPolicy Allow Deny mempty mempty 20))

      -- What it renders to matters as much as what it reads. This text is
      -- hashed and versioned, so a clause emitted for a policy that charges
      -- nothing would rewrite every existing policy file on the first upgrade
      -- and bump the version of every mailbox that has one.
      assertBool "a policy that charges says how much"
        ("(pow 20)" `isInfixOf` shown (BasicPolicy Allow Deny mempty mempty 20))
      assertBool "a policy that charges nothing says nothing"
        (not ("pow" `isInfixOf` shown (BasicPolicy Allow Deny mempty mempty 0)))

  , testCase "a difficulty a stamp cannot carry is refused, not clamped" $ do
      -- Refused rather than clamped, because a clamped 4096 is a mailbox
      -- charging a difficulty nobody typed. Unreachable is a typo either way.
      parsed "(pow 256)\n" >>= (@?= Nothing)
      parsed "(pow -1)\n"  >>= (@?= Nothing)
      parsed "(pow)\n"     >>= (@?= Nothing)
  ]
  where
    shown :: BasicPolicy S -> String
    shown = show . pretty
