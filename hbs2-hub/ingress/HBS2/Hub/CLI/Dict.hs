-- | What this tool answers to (PEP-22).
--
-- The dictionary's SURFACE, split out of @app\/Main.hs@ so that a test can ask
-- about it. Main is not a dependency of the test suite -- an executable cannot
-- be -- so everything it held was checked by running the binary and by nothing
-- else, and two of the things it holds are decisions rather than plumbing:
-- which verbs exist, and which inherited builtins are kept OUT.
--
-- What stays in Main is what closes over the finished dictionary: the help
-- entries, which take the dictionary they are listing.
module HBS2.Hub.CLI.Dict
  ( hubEntries
  , ours
  , peerFulNames
  ) where

import HBS2.Hub.CLI.Accept
import HBS2.Hub.CLI.Ban
import HBS2.Hub.CLI.Comment
import HBS2.Hub.CLI.Compact
import HBS2.Hub.CLI.Compose
import HBS2.Hub.CLI.Inbox
import HBS2.Hub.CLI.Maintainer
import HBS2.Hub.CLI.Own
import HBS2.Hub.CLI.Policy
import HBS2.Hub.CLI.Pr
import HBS2.Hub.CLI.Publish
import HBS2.Hub.CLI.Read
import HBS2.Hub.CLI.Reject
import HBS2.Hub.CLI.Sent
import HBS2.Hub.CLI.Show
import HBS2.Hub.CLI.Status
import HBS2.Hub.CLI.Sync
import HBS2.Hub.CLI.Updates
import HBS2.Hub.CLI.Verify
import HBS2.Hub.CLI.Whoami

import HBS2.CLI.Prelude
import HBS2.CLI.Run

import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import Data.Text qualified as Text

-- | Every verb this tool defines.
--
-- The order is the order they were added and means nothing: 'makeDict' is a
-- left-biased union over distinct names.
hubEntries :: forall c m . ( IsContext c
                           , MonadUnliftIO m
                           , HasStorage m
                           , HasClientAPI MailboxAPI UNIX m
                           , HasClientAPI LWWRefAPI UNIX m
                           , Exception (BadFormException c)
                           ) => MakeDictM c m ()
hubEntries = do
  inboxEntries
  acceptEntries
  commentEntries
  compactEntries
  composeEntries
  sentEntries
  statusEntries
  syncEntries
  updatesEntries
  verifyEntries
  whoamiEntries
  readEntries
  showEntries
  prEntries
  publishEntries
  maintainerEntries
  rejectEntries
  ownEntries
  policyEntries
  banEntries

-- | AN ALLOWLIST, and it is the whole of what this tool answers to.
--
-- The dictionary inherits hbs2-cli's 'internalEntries', which inherits
-- suckless-conf's, which binds about 154 verbs nothing here documents -- @rm@,
-- @mv@, @cp@, @cd@, @setenv@ and the whole @run:proc:*@ family. So
-- @hbs2-hub rm victim.txt@ deleted the file and exited 0, and @help rm@
-- answered for it while @--help@, which lists the @hub:@ names, did not. A tool
-- that files issues does not need to delete files, and surface it does not need
-- is surface a typo can land in.
--
-- NOT BY DROPPING A MODULE, which is where the obvious fix goes wrong: those
-- primitives are in @Script.Internal@, next to the evaluator, and not in
-- @Script.File@ as their names suggest -- so removing the import that looks
-- responsible removes something else and leaves @rm@ bound. And a blocklist is
-- the wrong shape whichever module it names: the upstream set grows, and a verb
-- added there would arrive here unannounced.
--
-- So the surface is stated positively: the verbs this tool defines, plus the
-- four names that are about the tool rather than about a forge.
ours :: Id -> Bool
ours (Id t) =
  "hub:" `Text.isPrefixOf` t || t `elem` ["help", "--help", "--version", "--run"]

-- | The verbs that NEED an @hbs2-peer@, which is the shorter list and the one
-- whose omissions are audible.
--
-- It used to be the other way round: the peer-FREE verbs were named, 26 of them
-- against these 15. That put the hand-maintained list on the side that keeps
-- growing, and on the side whose mistake is silent. Absent from the peer-free
-- list, or misspelled in it (the same thing, since a name the dictionary does
-- not hold is examined by nothing), a verb went through @recover@ and paid a
-- @hbs2-peer poke@ with no timeout: measured at 1.55 s against a live peer and
-- 6.0 s against a stub, and against a WEDGED peer it hung -- which is the peer
-- an operator has when they reach for the verbs that were supposed not to need
-- one.
--
-- Named this way round the two mistakes swap places, and both become loud:
--
--   * a new peer-free verb needs no edit here at all;
--   * a new peer-ful verb left out of this list is caught by the
--     PeerNotConnectedException handler at its dispatch, which says which verb
--     and says it is a build bug;
--   * a name that is not a verb is caught by "HBS2.Hub.DictSpec", which is
--     where it moved from a @die@ in 'main': a wrong list is a wrong build, and
--     a build error is cheaper than a runtime one.
--
-- Derived from the constraints instead? Not from these modules: the constraint
-- sits on the module, and @prEntries@ carries @HasStorage@ for `pr new` while
-- also holding `pr merge` and `pr checkout`, which reach only git, the keyman
-- and canon. Splitting @Pr@ by peer-need would split it on an axis it is not
-- organised by.
peerFulNames :: [Id]
peerFulNames =
  [ "hub:inbox", "hub:inbox:show", "hub:inbox:accept", "hub:inbox:honour"
  , "hub:inbox:reject"
  , "hub:issue:new", "hub:issue:comment"
  , "hub:pr:new", "hub:pr:revise", "hub:pr:comment"
  , "hub:updates"
  , "hub:policy:show", "hub:policy:pow", "hub:policy:default"
  , "hub:block", "hub:unblock"
  , "hub:whoami"
  -- It reads the mailbox: see the mailbox line in its report.
  , "hub:status"
  ]
