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
-- The version clause is per FILE, and a file this build cannot read is one
-- file. It does not veto the tree: the clause is unsigned text that anyone who
-- can write a file can write, so a single line saying @(hub-event 4294967295)@
-- would otherwise make a repository unreadable for every clone, permanently
-- and for free. The caller drops such a file and reports it, exactly as the
-- fold drops an event whose author box it cannot decode.
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
  | FileTooNew Word32      -- ^ @(hub-event N)@ newer than this build speaks
  deriving stock (Eq,Show)

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

    clauses =
      [ mkForm "hub-event" [mkInt hubEventVersion] ]
      <> [ mkForm "op" [mkSym @C (Text.unpack (opName ac))] | Just (_,ac) <- [author] ]
      <> [ mkForm "author-box" [b58 (boxBytes (evAuthorBox e))]
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

    opName = \case
      AOpen{} -> "open"; AComment{} -> "comment"; ARevise{} -> "revise"
      ASet{} -> "set"; AClose{} -> "close"; AReopen{} -> "reopen"
      AMerge{} -> "merge"; ARedact{} -> "redact"
      ADelegate{} -> "delegate"; ARevoke{} -> "revoke"

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
  if version > hubEventVersion
    then Left (FileTooNew version)
    else do
      abox <- box "author-box" =<< only "author-box" cs
      cbox <- box "canon-box" =<< only "canon-box" cs
      pure (version, Event abox cbox)
  where
    box :: Serialise a => Text -> Syntax C -> Either CanonError a
    box name s = do
      sym <- symbol name s
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
clausesOf txt =
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
