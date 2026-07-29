-- | The canon file format (PEP-19 "Event schema", "Tree layout").
--
-- Canon is a git tree of one file per event, and this module is the whole of
-- what turns an 'Event' into those bytes and back. It is deliberately separate
-- from the fold: the fold decides what canon MEANS, this decides what canon IS.
--
-- Two clauses in an event file are authoritative, the base64 of the two signed
-- boxes (see 'encodedBytes' for why that encoding and not the base58 the rest
-- of this project uses). Everything else in the file is a projection
-- regenerated from them, never read back and never trusted: a reader that believed @(seq 5)@ over the
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
  , maxEventBytes
  , maxClauses
  , maxTokenBytes
  , renderEvent
  , parseEvent
  , renderMeta
  , parseMeta
  , renderNumberIndex
  , parseNumberIndex
  , CanonRead(..)
  , readEventLog
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Letter (contentSyntax,maxBoxBytes)

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Prelude.Plated (Pretty(..),fromStringMay,(<+>))

import Data.Config.Suckless
import Data.Coerce (coerce)
import Prettyprinter (defaultLayoutOptions,layoutPageWidth,layoutPretty,PageWidth(Unbounded))
import Prettyprinter.Render.Text (renderStrict)

import Codec.Serialise (Serialise,serialise)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as B8
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

-- | What a reader says about a file it would not read, on a terminal.
instance Pretty CanonError where
  pretty = \case
    NotAnEvent e    -> "not an s-expression:" <+> pretty (safeText e)
    MissingClause c -> "missing clause" <+> pretty c
    BadClause c     -> "malformed clause" <+> pretty (safeText c)
    TooLarge what   -> "over the bound this reader will read:" <+> pretty what

-- | What a reader will look at before deciding the file is an attack.
--
-- DERIVED from the bounds the bridge mints under ('maxBoxBytes'), and this is
-- the second half of that rule rather than a separate opinion. A reader bound
-- chosen on its own is a reader that refuses what its own writer produced: at
-- one point the bridge would mint an event with a 32 KiB body, write the file,
-- and answer 'TooLarge' when asked to read it back, on the same build.
--
-- A token is one encoded box; a file is two of those plus the projection, which
-- repeats the same text and can double under escaping, plus room for the rest
-- of the clauses.
maxTokenBytes, maxEventBytes, maxClauses :: Int
maxTokenBytes = encodedBytes maxBoxBytes
maxEventBytes = 2 * maxTokenBytes + 2 * maxBoxBytes + 8192
maxClauses    = 128

-- | Base64, and the choice is load-bearing rather than a taste in alphabets.
--
-- Base58 is what this project uses for keys and hashes, where it earns its
-- keep: those are short and get read aloud and copied by hand. A box is
-- kilobytes, and base58 is a base conversion over a big integer, so decoding it
-- is quadratic in the length: measured on this code, 4 KiB takes 12 ms, 16 KiB
-- takes 213 ms, and 64 KiB takes two and a half seconds. Every fold of every
-- clone pays that for every event. Base64 is linear and denser, and the
-- alphabet does not matter here because nobody reads a box.
--
-- The number this costs is one third on top of the bytes; the number it saves
-- is the difference between reading a repository and not.
encodedBytes :: Int -> Int
encodedBytes n = 4 * ((n + 2) `div` 3)

bounded :: Int -> Int -> Text -> Either CanonError ()
bounded got limit what
  | got > limit = Left (TooLarge what)
  | otherwise   = Right ()

-- | The size of a text in UTF-8 bytes, which is the unit every bound here is
-- derived in.
--
-- Counting characters instead let a multi-byte file reach four times the bound
-- it was measured against, and the parameter was called a byte limit. Folded
-- rather than encoded, because measuring by encoding allocates the very
-- megabytes the bound exists to refuse.
utf8Length :: Text -> Int
utf8Length = Text.foldl' (\n c -> n + width c) 0
  where
    width c | c < '\x80'    = 1
            | c < '\x800'   = 2
            | c < '\x10000' = 3
            | otherwise     = 4

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
      , mkForm "author-box" [b64 (boxBytes (evAuthorBox e))]
      , mkForm "canon-box"  [b64 (boxBytes (evCanonBox e))]
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
           -- Named apart from the author box's own (target), which an open also
           -- carries: they answer different questions and a file shows both.
         , mkForm "canon-target" [b58key (ccTarget cc)]
         ]
      <> [ mkForm "part-secret" [mkSym @C (show (pretty (AsBase58 (partSecretBytes s))))]
         | Just s <- [ccPartSecret cc] ]

    -- The two boxes are base64 and everything else base58: see "encodedBytes"
    -- for why they are not the same encoding.
    b64 bs = mkSym @C (B8.unpack (B64.encode (LBS.toStrict bs)))
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
  version <- word32 "hub-event" =<< only "hub-event" cs
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
      _ <- bounded (utf8Length sym) maxTokenBytes name
      raw <- either (const (Left (BadClause name))) Right
               (B64.decode (Text.encodeUtf8 sym))
      either (const (Left (BadClause name))) Right (decodeChecked raw)

-- | What reading a whole canon tree produced.
--
-- Three lists rather than a list of events, because two of them are what
-- @hub verify@ exists to print (PEP-22) and what a writer has to look at before
-- it appends: a file this build could not read at all, named by its path since
-- that is the only identity an unreadable file has, and the version every file
-- declared, including the ones that read fine.
data CanonRead = CanonRead
  { crEvents   :: [Event]
  , crBad      :: [(FilePath, CanonError)]
  , crVersions :: [(FilePath, Word32)]
  }
  deriving stock (Eq,Show)

-- | Read every file of a canon tree.
--
-- The seam between the tree and the fold, and it is here so that there is one
-- of it: @hub sync@ and @hub verify@ both need "parse these files, keep what
-- parsed, report what did not", and two copies of that would disagree about
-- which files count on the day one of them grew a special case. The IO that
-- lists the tree stays outside; this is the whole of the decision.
--
-- Order is the caller's: hand the paths in lexical order and the events arrive
-- in the fold's own order for the @seq@ prefix, which costs the fold nothing
-- (it sorts anyway) and makes a truncated read a prefix rather than a sample.
readEventLog :: [(FilePath, Text)] -> CanonRead
readEventLog = foldr one (CanonRead [] [] [])
  where
    one (path, txt) acc = case parseEvent txt of
      Left err -> acc { crBad = (path, err) : crBad acc }
      Right (v, e) -> acc { crEvents = e : crEvents acc
                          , crVersions = (path, v) : crVersions acc
                          }

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
  word32 "hub-meta" =<< only "hub-meta" cs

-- | The number index (PEP-19): a convenience map from human @#N@ to thread.
--
-- Regenerable from the @open@ events, which compaction never drops, and never
-- trusted over them: it is written for a reader that wants @#42@ without
-- folding, and read only as a hint.
renderNumberIndex :: [(Word64, ThreadId)] -> Text
renderNumberIndex ns = Text.unlines
  [ render (mkForm  "number" [mkInt n, mkSym  (show (pretty t))])
  | (n,t) <- ns ]

-- One line at a time, with bounds of its own, and neither is the event file's.
--
-- The index has an entry per thread, so an event file's clause bound would cap
-- a repository at 128 issues; and the parser is superlinear in the number of
-- forms, so handing it a whole index at once is the cost this reader is
-- otherwise so careful about. A line is a form, the file is a list of lines,
-- and the work is linear in the file either way.
parseNumberIndex :: Text -> Either CanonError [(Word64, ThreadId)]
parseNumberIndex txt = do
  -- Before Text.lines, which materializes the whole file: a bound checked after
  -- the split is a bound on nothing.
  _ <- bounded (utf8Length txt) maxIndexBytes "index-file"
  let ls = [ l | l <- Text.lines txt, not (Text.null (Text.strip l)) ]
  _ <- bounded (length ls) maxIndexEntries "index"
  traverse line ls
  where
    line l = do
      _ <- bounded (utf8Length l) maxIndexLineBytes "index-line"
      cs <- clausesWith maxIndexLineBytes 4 l
      entry =<< only "number" cs

    entry = \case
      List _ [SymbolVal "number", LitIntVal n, SymbolVal t]
        -- Both ends of the range, not just the sign. Checking only n >= 0 and
        -- then narrowing let two lines of an index both claim to be number one,
        -- silently, on a file that is meant to answer which thread is which.
        | n >= 0, n <= fromIntegral (maxBound :: Word64) ->
            case fromStringMay (Text.unpack (idText t)) of
              Just h  -> Right (fromIntegral n, HashRef h)
              Nothing -> Left (BadClause "number")
      _ -> Left (BadClause "number")

-- | A number and a thread id per line, so the whole of a line's bound is one
-- integer and one base58 hash with room to spare.
--
-- The file bound is what actually decides this, and it is small on purpose. An
-- index exists to answer "#42" without folding canon, so reading one has to
-- cost less than folding: at roughly forty microseconds a line, a bound of a
-- million entries permitted half a minute of parsing and, with the line bound,
-- a quarter of a gigabyte of file. The threat model is the event file's, since
-- @hub verify@ reads trees other people wrote.
--
-- Refusing an index is not refusing the repository: it is regenerable from the
-- open events and never trusted over them, so a caller that meets 'TooLarge'
-- here proceeds as though there were no index at all.
maxIndexBytes, maxIndexLineBytes, maxIndexEntries :: Int
maxIndexBytes     = 2 * 1024 * 1024
maxIndexLineBytes = 256
maxIndexEntries   = maxIndexBytes `div` 48

-- The clauses of a file, however the writer laid them out.
--
-- One clause per line reads back as one top-level form each; several on one
-- line read back as a single form containing them. Both are accepted, because
-- the layout is not part of the format and a file may have been re-flowed by
-- anything.
clausesOf :: Text -> Either CanonError [Syntax C]
clausesOf = clausesWith maxEventBytes maxClauses

clausesWith :: Int -> Int -> Text -> Either CanonError [Syntax C]
clausesWith byteLimit formLimit txt = do
  -- Both bounds are checked before the parser runs, and both are counted in
  -- one pass over the text, because the parser is what they are protecting.
  -- Reading a canon file is the first thing anyone does with a tree and the
  -- last thing they can refuse to do, so the cost of a file has to be bounded
  -- by something cheaper than parsing it. It is superlinear in the number of
  -- forms, so a file of forty thousand empty clauses costs a minute of CPU,
  -- and one file that anybody with write access can drop into a tree would
  -- then stop every fold and every verify in every clone.
  _ <- bounded (utf8Length txt) byteLimit "file"
  _ <- bounded (countForms txt) formLimit "clauses"
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

-- How many forms a text opens, counting only the parentheses the parser will
-- see as parentheses.
--
-- Counting every '(' in the file was a bound on the CONTRIBUTOR rather than on
-- the attacker: a body is a string literal, code in a body has brackets, and
-- 115 of them anywhere in an issue made the file unreadable for good. The
-- escape rule is the one the writer emits and the reader honours, so a quote
-- inside a string does not end it.
countForms :: Text -> Int
countForms = seen . Text.foldl' step (0 :: Int, Plain)
  where
    seen (n,_) = n

    step (n, st) c = case st of
      InComment | c == '\n'   -> (n + 1, Plain)
                | otherwise   -> (n, InComment)
      InEscape                -> (n, InString)
      InString  | c == '\\'   -> (n, InEscape)
                | c == '"'    -> (n, Plain)
                | otherwise   -> (n, InString)
      Plain     | c == '"'    -> (n, InString)
                -- A comment is tracked for one reason: a quote inside one
                -- would otherwise put this counter into a string it is not in,
                -- and everything after it would go uncounted.
                | c == ';'    -> (n, InComment)
                | opens c     -> (n + 1, Plain)
                | otherwise   -> (n, Plain)

    -- Every character that opens a form, and a newline is one of them.
    --
    -- The parser makes a list per bracket pair, a list per quote, quasiquote or
    -- unquote, and a list per non-empty LINE, and the cost is superlinear in
    -- how many lists it makes. Each spelling of the same attack was cheaper
    -- than the last: counting only '(' let "[]" through at four minutes a file,
    -- and counting brackets alone let bare atoms one per line through at over a
    -- minute for eighty kilobytes, growing sixfold per doubling. A newline is
    -- the cheapest of the three, two bytes an item and no punctuation at all.
    --
    -- Counting newlines as well as brackets double-counts an ordinary event
    -- file, which is what a bound can afford to do: a real one is a dozen or so
    -- lines against a limit of a hundred and twenty-eight.
    opens c = c `elem` ("([{'`,\n" :: String)

-- Where a scan of the text is, for 'countForms'.
data Scan = Plain | InString | InEscape | InComment

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

-- The clause name is passed in rather than taken from what was parsed: the
-- fallback used to print the whole malformed form into the error, which is a
-- stranger's bytes on their way to a terminal, and the caller already knows the
-- name it asked for.
word32 :: Text -> Syntax C -> Either CanonError Word32
word32 name = \case
  List _ [SymbolVal _, LitIntVal n]
    | n >= 0, n <= fromIntegral (maxBound :: Word32) -> Right (fromIntegral n)
  _ -> Left (BadClause name)
