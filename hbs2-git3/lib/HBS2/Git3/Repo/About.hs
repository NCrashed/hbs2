-- | What a repository says about itself: a name and a one-paragraph
-- description, in its manifest (PEP-25).
--
-- A repository is otherwise known by its key alone, forty-four characters of
-- base58, which is what every page about it showed. These are the two clauses
-- a page needs to call it something:
--
-- > (name "hbs2-hub-browser")
-- > (description "hbs2-hub issues and pull requests on GitHub pages")
--
-- Words the owner wrote, signed with the manifest; a reader shows them as text.
module HBS2.Git3.Repo.About
  ( About(..)
  , aboutOf
  , setAbout
  , writesBackAsAbout
  , maxNameChars
  , maxDescriptionChars
  ) where

import HBS2.Git3.Prelude
import HBS2.Git3.Repo.Mailbox (renderManifest)

import Data.Config.Suckless.Script

import Data.Char (isControl)
import Data.Text qualified as Text

data About = About
  { aboutName        :: Maybe Text
  , aboutDescription :: Maybe Text
  }
  deriving stock (Eq,Show)

maxNameChars, maxDescriptionChars :: Int
maxNameChars = 100
maxDescriptionChars = 1000

-- | The name and the description a manifest declares, the first of each.
aboutOf :: [Syntax C] -> About
aboutOf mf = About (field "name") (field "description")
  where
    field f = headMay [ Text.pack s | ListVal [SymbolVal f', StringLike s] <- mf, f' == f ]

-- | Set one of the two fields, or take it out with 'Nothing'.
--
-- Replaced where it is, so a manifest someone has read keeps its order, and
-- appended when new. Refused rather than trimmed: an empty value is not one
-- this format can write (see "HBS2.Git3.Repo.Mailbox"), each is one line with
-- no quote or backslash in it, and neither carries a control character.
setAbout :: Id -> Maybe Text -> [Syntax C] -> Either Text [Syntax C]
setAbout field mv mf = case mv of
  Nothing -> Right without
  Just v -> do
    let t = Text.strip v
        limit = if field == "name" then maxNameChars else maxDescriptionChars
    when (Text.null t) $ Left (idText <> " is empty; --clear-" <> idText <> " takes it out")
    when (Text.length t > limit) $ Left (idText <> " is longer than " <> Text.pack (show limit) <> " characters")
    -- ONE LINE, AND NO QUOTE OR BACKSLASH, because the manifest's renderer
    -- writes a string between quotes as it is: a quote inside ends it early, a
    -- newline comes back a space, and the manifest no longer parses.
    -- 'writesBackAsAbout' would refuse that before anything is published; this
    -- says why, in words, first.
    when (Text.any (`elem` ("\n\r" :: String)) t) $ Left (idText <> " is one line")
    when (Text.any (`elem` ("\"\\" :: String)) t) $ Left (idText <> " may not contain a quote or a backslash")
    when (Text.any isControl t) $ Left (idText <> " has a control character in it")
    let c = mkForm @C (Text.unpack idText) [mkStr t]
    pure $ if any isIt mf then [ if isIt x then c else x | x <- dropLater mf ] else mf <> [c]
  where
    idText = let Id i = field in i
    isIt = \case
      ListVal (SymbolVal f : _) -> f == field
      _ -> False
    without = [ x | x <- mf, not (isIt x) ]
    -- Two clauses for one field would make 'aboutOf' answer from the first;
    -- keep the first in place and drop the rest.
    dropLater xs = go False xs
      where
        go _ [] = []
        go seen (x:rest)
          | isIt x && seen = go seen rest
          | isIt x = x : go True rest
          | otherwise = x : go seen rest

-- | Does the manifest, rendered as it will be stored and parsed back, say
-- exactly this? Checked on the bytes, for the reason 'renderManifest' gives.
writesBackAsAbout :: About -> [Syntax C] -> Bool
writesBackAsAbout want mf = case parseTop (renderManifest mf) of
  Left _ -> False
  Right back -> aboutOf back == want
