-- | The canon file format (PEP-19 "Event schema", "Tree layout").
--
-- Canon is a git tree of one file per event, and this module is the whole of
-- what turns an 'Event' into those bytes and back. It is deliberately separate
-- from the fold: the fold decides what canon MEANS, this decides what canon IS.
--
-- Two clauses in an event file are authoritative, the base58 of the two signed
-- boxes. Everything else in the file is a projection regenerated from them,
-- never read back and never trusted: a reader that believed @(seq 5)@ over the
-- canon box would be trusting an unsigned line of text.
--
-- The version clause is reported and never obeyed. It vetoes nothing: not the
-- tree, which would hand a veto to anyone who can write one unsigned line, and
-- not the file either. The two boxes are self-describing CBOR and a version
-- this build does not know does not make them unreadable; what a newer schema
-- can actually do is put content inside a box that this build cannot decode,
-- and the fold answers that on its own, by dropping the event and keeping its
-- stamp. Refusing here instead would throw the stamp away one floor higher up,
-- which is the same mistake the tree-wide veto was.
--
-- Both bounds a reader needs are checked before the parser runs, because
-- reading is what a hostile file attacks. See 'maxEventBytes'.
module HBS2.Hub.Canon
  ( CanonError(..)
  , renderEvent
  , parseEvent
  , renderMeta
  , parseMeta
  , renderNumberIndex
  , parseNumberIndex
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Letter (contentSyntax)

import HBS2.Base58 (AsBase58(..),fromBase58)
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Prelude.Plated (Pretty(..),fromStringMay)

import Data.Config.Suckless
import Data.Coerce (coerce)
import Prettyprinter (defaultLayoutOptions,layoutPageWidth,layoutPretty,PageWidth(Unbounded))
import Prettyprinter.Render.Text (renderStrict)

import Codec.Serialise (Serialise,serialise)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word32,Word64)

-- | Why a canon file could not be read.
--
-- 'FileTooNew' is not a failure of the file, and the difference matters to the
-- caller: everything else here means somebody wrote something malformed, while
-- that one means this build is behind and an upgrade will read it.
data CanonError =
    NotAnEvent Text        -- ^ the text does not parse as s-expressions at all
  | MissingClause Text     -- ^ a clause the schema requires is absent
  | BadClause Text         -- ^ present, but not the shape the schema pins
    -- | Over a bound this reader will not spend work on, naming what: the file,
    -- its clause count, or one token. Not a judgement about the content, which
    -- is exactly the point: it is refused before anything reads it.
  | TooLarge Text
  deriving stock (Eq,Show)

-- | What a reader will look at before deciding the file is an attack.
--
-- An event file is two base58 boxes, a version, and a projection of at most a
-- couple of dozen clauses; the one part that can be legitimately large is an
-- inline body, bounded elsewhere at 32 KiB. These are all far above anything a
-- writer here produces and far below what costs a reader anything.
maxEventBytes, maxClauses, maxTokenBytes :: Int
maxEventBytes = 128 * 1024
maxClauses    = 128
maxTokenBytes = 4 * 1024

bounded :: Int -> Int -> Text -> Either CanonError ()
bounded got limit what
  | got > limit = Left (TooLarge what)
  | otherwise   = Right ()

-- | Write one event file.
--
-- The two boxes are written first, so that a reader that stops early still has
-- everything that matters, and a human opening the file sees the authoritative
-- part before the readable one.
--
-- The projection is written only when the boxes can be opened. A newer schema
-- is exactly the case where this build cannot describe the content, and
-- guessing would be worse than silence: compaction re-renders events it read
-- from canon, so it must be able to write a file whose content it does not
-- understand without inventing clauses for it.
renderEvent :: Event -> Text
renderEvent e = Text.unlines (fmap render clauses)
  where

    -- The op clause comes from 'contentSyntax', which every projection of an
    -- author content emits: writing one here as well put two of them in every
    -- file, and 'only' refuses a clause that appears twice.
    clauses =
      [ mkForm "hub-event" [mkInt hubEventVersion]
      , mkForm "author-box" [b58 (boxBytes (evAuthorBox e))]
      , mkForm "canon-box"  [b58 (boxBytes (evCanonBox e))]
      ]
      <> concat [ mkForm "author" [b58key k] : contentSyntax ac | Just (k,ac) <- [author] ]
      <> concat [ canonClauses k cc | Just (k,cc) <- [canonOf] ]

    author  = ok (unboxChecked (evAuthorBox e))
    canonOf = ok (unboxChecked (evCanonBox e))
    ok = either (const Nothing) Just

    canonClauses k cc =
      [ mkForm "seq" [mkInt (ccSeq cc)] ]
      <> [ mkForm "number" [mkInt n] | Just n <- [ccNumber cc] ]
      <> [ mkForm "origin" [href o] | Just o <- [ccOrigin cc] ]
      <> [ mkForm "folded-ts" [mkInt (ccFoldedTs cc)]
         , mkForm "canon-by" [b58key k]
         ]
      <> [ mkForm "part-secret" [b58 (partSecretBytes s)] | Just s <- [ccPartSecret cc] ]

    b58 bs = mkSym @C (show (pretty (AsBase58 bs)))
    b58key k = mkSym @C (show (pretty (AsBase58 k)))
    href h = mkSym @C (show (pretty h))

    boxBytes :: Serialise a => a -> LBS.ByteString
    boxBytes = serialise

-- | Read one event file back.
--
-- Returns the file's declared version alongside the event, because a caller
-- that folds a tree has to report the versions it met even when it could read
-- them (PEP-22), and a caller that WRITES has to know it is not silently
-- downgrading a file it did not author.
--
-- Only the two box clauses are read. The projection is ignored entirely: it is
-- unsigned, so believing any of it would mean believing whoever last wrote the
-- file about what the boxes say.
parseEvent :: Text -> Either CanonError (Word32, Event)
parseEvent txt = do
  cs <- clausesOf txt
  version <- word32 =<< only "hub-event" cs
  abox <- box "author-box" =<< only "author-box" cs
  cbox <- box "canon-box" =<< only "canon-box" cs
  pure (version, Event abox cbox)
  where
    box :: Serialise a => Text -> Syntax C -> Either CanonError a
    box name s = do
      sym <- symbol name s
      -- Before the decode, not after. Base58 is Integer arithmetic and
      -- quadratic in the length of the token, so a hundred kilobytes in one
      -- clause exhausts the heap of whoever reads the file, and a reader that
      -- verifies signatures afterwards never gets that far. A box is a few
      -- hundred bytes; anything past 'maxTokenBytes' is not a late one.
      _ <- bounded (Text.length sym) maxTokenBytes name
      raw <- maybe (Left (BadClause name)) Right
               (fromBase58 (Text.encodeUtf8 sym))
      either (const (Left (BadClause name))) Right (decodeChecked raw)

-- | The tree's @version@ file: the consensus version of the whole canon.
--
-- This one DOES govern the tree, unlike a file's own version: it names the
-- admission rules that produced everything in it, and a reader that folded
-- under different rules would produce a view that quietly disagrees with every
-- up-to-date clone. It is unsigned like everything else in the tree, so it
-- protects against an honest version skew and not against whoever can write to
-- the tree; the fold's answer to that one is that only the reflog key can
-- publish (PEP-19).
renderMeta :: Text
renderMeta = render (mkForm  "hub-meta" [mkInt hubMetaVersion]) <> "\n"

parseMeta :: Text -> Either CanonError Word32
parseMeta txt = do
  cs <- clausesOf txt
  word32 =<< only "hub-meta" cs

-- | The number index (PEP-19): a convenience map from human @#N@ to thread.
--
-- Regenerable from the @open@ events, which compaction never drops, and never
-- trusted over them: it is written for a reader that wants @#42@ without
-- folding, and read only as a hint.
renderNumberIndex :: [(Word64, ThreadId)] -> Text
renderNumberIndex ns = Text.unlines
  [ render (mkForm  "number" [mkInt n, mkSym  (show (pretty t))])
  | (n,t) <- ns ]

parseNumberIndex :: Text -> Either CanonError [(Word64, ThreadId)]
parseNumberIndex txt = do
  cs <- clausesOf txt
  traverse entry [ c | c@(List _ (SymbolVal "number" : _)) <- cs ]
  where
    entry = \case
      List _ [SymbolVal "number", LitIntVal n, SymbolVal t]
        | n >= 0 -> case fromStringMay (Text.unpack (idText t)) of
            Just h  -> Right (fromIntegral n, HashRef h)
            Nothing -> Left (BadClause "number")
      _ -> Left (BadClause "number")

-- The clauses of a file, however the writer laid them out.
--
-- One clause per line reads back as one top-level form each; several on one
-- line read back as a single form containing them. Both are accepted, because
-- the layout is not part of the format and a file may have been re-flowed by
-- anything.
clausesOf :: Text -> Either CanonError [Syntax C]
clausesOf txt = do
  -- Both bounds are checked before the parser runs, and both are counted in
  -- one pass over the text, because the parser is what they are protecting.
  -- Reading a canon file is the first thing anyone does with a tree and the
  -- last thing they can refuse to do, so the cost of a file has to be bounded
  -- by something cheaper than parsing it. It is superlinear in the number of
  -- forms, so a file of forty thousand empty clauses costs a minute of CPU,
  -- and one file that anybody with write access can drop into a tree would
  -- then stop every fold and every verify in every clone.
  _ <- bounded (Text.length txt) maxEventBytes "file"
  _ <- bounded (Text.count "(" txt) maxClauses "clauses"
  case parseTop txt of
    Left e     -> Left (NotAnEvent (Text.pack (show e)))
    Right tops -> Right (concatMap flatten tops)
  where
    flatten s = case s of
      List _ (List{} : _) -> [ x | x@List{} <- listOf s ]
      _                   -> [s]
    listOf = \case
      List _ xs -> xs
      _         -> []

-- Exactly one clause of this name, because two would mean two answers and no
-- rule for picking between them.
only :: Text -> [Syntax C] -> Either CanonError (Syntax C)
only name cs = case [ c | c@(List _ (SymbolVal n : _)) <- cs, idText n == name ] of
  [c] -> Right c
  []  -> Left (MissingClause name)
  _   -> Left (BadClause name)

-- One clause per line, and never wrapped.
--
-- Two reasons, and neither is taste. Several forms on ONE line are read back as
-- one nested form, so the line breaks are part of the grammar here. And the
-- default layout wraps at a page width, which would make the bytes of a canon
-- file depend on a printer's idea of a terminal: an event would render
-- differently under a different default, and canon is content-addressed.
render :: Syntax C -> Text
render = renderStrict . layoutPretty opts . pretty
  where
    opts = defaultLayoutOptions { layoutPageWidth = Unbounded }

-- Symbols carry an Id, which is a Text in a newtype; the format is written in
-- terms of the Text.
idText :: Id -> Text
idText = coerce

symbol :: Text -> Syntax C -> Either CanonError Text
symbol name = \case
  List _ [SymbolVal _, SymbolVal v] -> Right (idText v)
  _                                 -> Left (BadClause name)

word32 :: Syntax C -> Either CanonError Word32
word32 = \case
  List _ [SymbolVal name, LitIntVal n]
    | n >= 0, n <= fromIntegral (maxBound :: Word32) -> Right (fromIntegral n)
    | otherwise -> Left (BadClause (idText name))
  s -> Left (BadClause (Text.pack (show (pretty s))))
