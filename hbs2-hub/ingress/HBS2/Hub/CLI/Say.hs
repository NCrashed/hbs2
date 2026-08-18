-- | The two writers every verb in this tool leaves through (PEP-22).
--
-- WHY THIS IS ITS OWN MODULE. Both of these lived in "HBS2.Hub.CLI.Common",
-- which imports "HBS2.Hub.CLI.Verify" for the refusal report -- so the one
-- module that could not have them was the one that prints @hub verify@'s
-- refusals, and it wrote to stderr by hand. So did two others. Each hand-written
-- one is a copy that has to remember both of the rules below, and each of them
-- had forgotten one.
--
-- The rules, and they are the whole content of this module:
--
-- * STDOUT IS FLUSHED FIRST, or advice on stderr overtakes the report on stdout
--   it is about. The streams are buffered differently the moment either is not a
--   terminal, which is why this is invisible until somebody reads a CI log.
--
-- * A CLOSED HANDLE DOES NOT TAKE THE EXIT CODE WITH IT. @hub inbox K 2>&1 |
--   head@ closes both, and an unguarded write then leaves through the RTS with
--   1 -- the code PEP-22 reserves for usage errors, which is the one thing the
--   caller must not be told.
module HBS2.Hub.CLI.Say
  ( saying
  , refuse
  ) where

import HBS2.CLI.Prelude

import System.Exit (exitWith,ExitCode(..))
import System.IO.Error (isResourceVanishedError)

-- | Write to stderr, or do not, but do not take the exit code with you.
saying :: Doc AnsiStyle -> IO ()
saying d = quietly (hFlush stdout) >> quietly (hPutDoc stderr d)
  where
    -- Guarded SEPARATELY, so a closed stdout does not swallow the message: the
    -- two handles are closed by different things, and `hub inbox K | head`
    -- closes only the first.
    quietly = handleJust (\e -> if isResourceVanishedError e then Just () else Nothing)
                         (\_ -> pure ())

-- | Say why, on stderr, and leave with the code that says which why.
refuse :: String -> Int -> IO a
refuse msg n = do
  saying ("hbs2-hub:" <+> pretty msg <> line)
  exitWith (ExitFailure n)
