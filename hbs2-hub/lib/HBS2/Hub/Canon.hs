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
-- All three bounds a reader needs (bytes, clauses, escapes) are checked before
-- the parser runs, because reading is what a hostile file attacks. See
-- 'maxEventBytes' and 'maxEscapes'.
module HBS2.Hub.Canon
  ( CanonError(..)
  , maxEventBytes
  , maxClauses
  , maxEscapes
  , maxTokenBytes
    -- | The bounded reader itself, for the files that are NOT canon.
    --
    -- A manifest and a policy are S-expressions this node fetched from
    -- somewhere else, so they are the same threat as an event file and were
    -- being handed straight to 'parseTop' with no bound at all. The limits are
    -- the caller's, since a manifest is not an event and neither is an index;
    -- the RULE -- count bytes, top-level forms and escapes in one pass, before
    -- the parser sees anything -- is this module's, and there should be one of
    -- it. The reasoning it encodes, including the measurements, is on
    -- 'scanText' below and is not worth deriving twice.
  , clausesWith
    -- | Measuring and cutting text in the unit every bound here is derived in.
    --
    -- Exported because the reader is not the only place that budgets bytes:
    -- @hub issue|pr show --json@ bounds the diff it embeds, and was doing it
    -- with 'Text.length' against a constant named for bytes, which is the same
    -- four-times overrun this module records having fixed once already.
  , utf8Length
  , takeBytes
  , renderEvent
  , renderEventAt
  , parseEvent
  , MetaVersions(..)
  , metaAt
  , renderMeta
  , parseMeta
  , renderNumberIndex
  , parseNumberIndex
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
import Data.Char (isSpace)
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe)
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
    -- Ord so a caller can sort a report by path and reason. The order means
    -- nothing beyond the constructor order; it is not a severity.
  deriving stock (Eq,Ord,Show)

-- | What a reader says about a file it would not read, on a terminal.
instance Pretty CanonError where
  pretty = \case
    NotAnEvent e    -> "not an s-expression:" <+> pretty (safeText e)
    MissingClause c -> "missing clause" <+> pretty (safeText c)
    BadClause c     -> "malformed clause" <+> pretty (safeText c)
    TooLarge what   -> "over the bound this reader will read:" <+> pretty (safeText what)

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

-- | The most escape sequences a canon file may carry.
--
-- The third bound, and the one the other two do not imply. Un-escaping a string
-- literal costs the parser time superlinear in how many escapes the file holds:
-- measured on a file shaped like a real event (two tokens at the bound, a
-- projected body) it is 0.06 s at 512 escapes, 0.19 s at 1024, 0.76 s at 2048
-- and 3.0 s at 4096, and at the byte bound, where every other byte of the body
-- is a backslash, eighty seconds. That last one is an HONEST file: a 32 KiB
-- body is inside every limit the letter layer sets, and a body of newlines is
-- what a code block is. So this is not only a bound on an attacker; it is the
-- reason 'capEscapes' truncates the projection the writer emits, rather than a
-- reader refusing what its own writer produced.
--
-- 1024 rather than something rounder because that is where the measured cost
-- crosses two tenths of a second, and a fold pays it once per event file.
maxEscapes :: Int
maxEscapes = 1024

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
utf8Length = Text.foldl' (\n c -> n + utf8Width c) 0

-- | What one character weighs in UTF-8.
utf8Width :: Char -> Int
utf8Width c | c < '\x80'    = 1
            | c < '\x800'   = 2
            | c < '\x10000' = 3
            | otherwise     = 4

-- | The longest prefix of a text that fits in a byte budget.
--
-- Cuts BETWEEN code points, never inside one, because the caller is truncating
-- for a byte bound and what comes out still has to be text. 'Text.take' is the
-- wrong tool for the same reason 'Text.length' is the wrong measure: both count
-- characters, and every bound in this package is derived in bytes.
takeBytes :: Int -> Text -> Text
takeBytes n = Text.pack . go n . Text.unpack
  where
    go _ [] = []
    go k (c:cs) | w > k     = []
                | otherwise = c : go (k - w) cs
      where w = utf8Width c

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
renderEvent = renderEventAt Nothing

-- | The same, writing a version the caller already knows this file had.
--
-- WHAT COMPACTION MUST NOT INVENT. Every retained file is re-rendered through
-- here, and with the version taken from this build's constant that erased the
-- only record a tree keeps of having been written under two rule sets: a file
-- declaring @(hub-event 1)@ came back declaring 2, so a reader could no longer
-- tell an old event from a new one, and the one verb that rewrites files was
-- the one that destroyed the evidence. The clause has no consumer yet, which is
-- exactly why it must survive until it does: it cannot be recovered afterwards.
--
-- 'Nothing' is a freshly minted event, which is written under this build's
-- rules and says so.
renderEventAt :: Maybe Word32 -> Event -> Text
renderEventAt ver e = Text.unlines (fmap render clauses)
  where

    -- The op clause comes from 'contentSyntax', which every projection of an
    -- author content emits: writing one here as well put two of them in every
    -- file, and 'only' refuses a clause that appears twice.
    clauses =
      [ mkForm "hub-event" [mkInt (fromMaybe hubEventVersion ver)]
      , mkForm "author-box" [b64 (boxBytes (evAuthorBox e))]
      , mkForm "canon-box"  [b64 (boxBytes (evCanonBox e))]
      ]
      -- Only the projection is capped, and only the projection can be: the two
      -- boxes above are base64 and carry no escape at all, and they are the
      -- authoritative copy of everything the cap may cut.
      <> capEscapes maxEscapes
           (  concat [ mkForm "author" [b58key k] : contentSyntax ac | Just (k,ac) <- [author] ]
           <> concat [ canonClauses k cc | Just (k,cc) <- [canonOf] ] )

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

    -- THROUGH THE SAME GUARDS THE REPORTS USE. base58 is Integer base
    -- conversion and therefore quadratic, and neither a HashRef nor a HubKey
    -- is bounded to its scheme's width by the type -- so a field that is not
    -- a hash is rendered as its size rather than converted. The haddock on
    -- 'hashDoc' predicted this module by name: "every renderer that prints one
    -- of these needs the same guard, and the second one to need it printed
    -- pretty instead". This is the third, and it is the one  compact@
    -- reaches over canon somebody else published.
    b58key k = mkSym @C (show (keyDoc k))
    href h = mkSym @C (show (hashDoc h))

    boxBytes :: Serialise a => a -> LBS.ByteString
    boxBytes = serialise

-- Spend an escape budget across a projection, truncating the strings that
-- overrun it.
--
-- Not a refusal, because there is nothing to refuse: the content is inside a
-- signed box that this same file carries in full, the letter layer already
-- accepted it, and the projection exists so that @git show@ is worth running.
-- What it costs a reader is the parser's, and the parser is the thing the
-- budget protects (see 'maxEscapes').
--
-- Truncation is at an escape boundary, in the ALREADY-escaped text: the strings
-- here come from 'sexpStr', which escapes and then hands the printer something
-- it wraps in quotes and nothing more, so cutting mid-sequence would leave a
-- trailing backslash that swallows the closing quote. The marker is added after
-- the cut and contains no escape of its own.
capEscapes :: Int -> [Syntax C] -> [Syntax C]
capEscapes budget0 = snd . mapAccumL go budget0
  where
    go budget = \case
      List a xs -> let (b', ys) = mapAccumL go budget xs in (b', List a ys)
      s@(LitStrVal t)
        | escapesIn t <= budget -> (budget - escapesIn t, s)
        | otherwise             -> (0, mkStr @C (cut budget t <> truncationMark))
      s -> (budget, s)

    escapesIn = Text.foldl' (\n c -> if c == '\\' then n + 1 else n) (0 :: Int)

    -- Walk to where the budget runs out. The state is (escapes left, is this
    -- character the second half of an escape sequence).
    --
    -- ONE PASS, by 'Text.uncons' rather than by index. This walked with
    -- 'Text.index' and 'Text.compareLength', both O(i) on text's UTF-8
    -- representation, so the walk was O(k^2) in the offset the budget runs out
    -- at -- and that offset is not bounded by the escape count, since the loop
    -- only stops at a backslash. A body of 32 KiB (exactly 'maxInlineBody')
    -- made of ordinary text followed by eleven hundred backslashes is a
    -- stranger's letter that costs a second of the triage loop's CPU, inside
    -- 'renderEvent', once per accept. Measured on this project's text-2.0.2:
    -- 0.97 s for that shape against 0.006 s for the same escapes at the front,
    -- and the same second again for every event a compaction re-renders.
    cut n t = Text.take (walk 0 n False t) t
      where
        walk !i !left !inEsc s = case Text.uncons s of
          Nothing -> i
          Just (c, rest)
            | inEsc      -> walk (i + 1) left False rest
            | c /= '\\'  -> walk (i + 1) left False rest
            | left <= 0  -> i
            | otherwise  -> walk (i + 1) (left - 1) True rest

-- What a truncated projection says about itself, in a form that costs nothing
-- to parse and cannot be mistaken for the content: no escape, no quote, and the
-- authoritative copy is in the box a few lines above it.
truncationMark :: Text
truncationMark = " [projection truncated: see the author box]"

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
--
-- 'Nothing' for the version when the clause is missing or is not a number.
-- The version is reported and never obeyed, so it has no vote on whether the
-- file is readable: refusing on it let anybody who can write into a clone hide
-- a signed event by editing one unsigned line, and the event would go missing
-- rather than come back wrong, which is the harder failure to notice.
parseEvent :: Text -> Either CanonError (Maybe Word32, Event)
parseEvent txt = do
  cs <- clausesOf txt
  -- Forced here, not returned as a thunk. Unforced it closed over cs, so the
  -- whole parse of the file stayed alive inside the Maybe: the caller keeps one
  -- of these per file and forces them only when the report is printed, which made
  -- the peak the sum of every file's parse instead of the largest single file.
  let !version = either (const Nothing) Just (word32 "hub-event" =<< only "hub-event" cs)
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


-- | The tree's @version@ file: the consensus version of the whole canon.
--
-- This one DOES govern the tree, unlike a file's own version: it names the
-- admission rules that produced everything in it, and a reader that folded
-- under different rules would produce a view that quietly disagrees with every
-- up-to-date clone. It is unsigned like everything else in the tree, so it
-- protects against an honest version skew and not against whoever can write to
-- the tree; the fold's answer to that one is that only the reflog key can
-- publish (PEP-19).
-- | What the @version@ file says.
--
-- TWO NUMBERS, and the second is the one that decides whether a reader may
-- proceed. @hub-meta@ is what the tree was written under; @hub-min@ is the
-- lowest reader that still produces a SOUND view of it, which is not the same
-- question and was not asked. See 'hubMetaMin'.
data MetaVersions = MetaVersions
  { mvRules :: Word32   -- ^ @(hub-meta N)@
  , mvMin   :: Word32   -- ^ @(hub-min M)@; equal to N when the file omits it
  }
  deriving stock (Eq,Show)

renderMeta :: MetaVersions -> Text
renderMeta mv =
  render (mkForm "hub-meta" [mkInt (mvRules mv)]) <> "\n"
    <> render (mkForm "hub-min" [mkInt (mvMin mv)]) <> "\n"

-- | The version this build writes for a tree needing rules @n@.
--
-- A helper rather than two arguments at each call site: they are both 'Word32'
-- and swapping them would say the opposite of what was meant.
metaAt :: Word32 -> MetaVersions
metaAt n = MetaVersions n (min hubMetaMin n)

parseMeta :: Text -> Either CanonError MetaVersions
parseMeta txt = do
  cs <- clausesOf txt
  n <- word32 "hub-meta" =<< only "hub-meta" cs
  -- ABSENT MEANS N, which is what every tree written before this clause
  -- existed says: without it a reader has no claim to lean on and the honest
  -- floor is the rules version itself. 'only' tolerates a clause it does not
  -- know, so a tree carrying it stays readable to a build that does not.
  m <- case only "hub-min" cs of
         Left _  -> pure n
         Right c -> word32 "hub-min" c
  pure (MetaVersions n m)

-- | The number index (PEP-19): a convenience map from human @#N@ to thread.
--
-- Regenerable from the @open@ events, which compaction never drops, and never
-- trusted over them: it is written for a reader that wants @#42@ without
-- folding, and read only as a hint.
-- Bounded by the same three numbers its reader is, because a file written past
-- them is a file this build refuses to read back: the index is a cache, and one
-- nobody reads is worse than a shorter one, which is still correct for every
-- number it does hold. A line over the line bound is dropped rather than
-- truncated, since half a thread-id would resolve to nothing or, worse, to
-- something else.
--
-- Entries stay in the caller's order, so a truncated index is a prefix of the
-- full one rather than a sample of it.
renderNumberIndex :: [(Word64, ThreadId)] -> Text
renderNumberIndex ns = Text.unlines (go 0 maxIndexEntries lines')
  where
    lines' = [ l | (n,t) <- ns
             , let l = render (mkForm "number" [mkInt n, mkSym (show (pretty t))])
             , utf8Length l <= maxIndexLineBytes ]

    go _ _ [] = []
    go used left (l:rest)
      | left <= 0            = []
      -- +1 for the newline 'Text.unlines' adds, which the reader counts too.
      | used + w > maxIndexBytes = []
      | otherwise            = l : go (used + w) (left - 1) rest
      where w = utf8Length l + 1

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
  -- All three bounds are checked before the parser runs, and all three are
  -- counted in one pass over the text, because the parser is what they are
  -- protecting. Reading a canon file is the first thing anyone does with a tree
  -- and the last thing they can refuse to do, so the cost of a file has to be
  -- bounded by something cheaper than parsing it. It is superlinear in the
  -- number of forms, so a file of forty thousand empty clauses costs a minute
  -- of CPU, and one file that anybody with write access can drop into a tree
  -- would then stop every fold and every verify in every clone.
  _ <- bounded (utf8Length txt) byteLimit "file"
  let (forms, escapes) = scanText txt
  _ <- bounded forms formLimit "clauses"
  _ <- bounded escapes maxEscapes "escapes"
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

-- How many items a text opens at the top level, and how many escape sequences
-- it contains, counting only the punctuation the parser will see as punctuation.
--
-- Counting every '(' in the file was a bound on the CONTRIBUTOR rather than on
-- the attacker: a body is a string literal, code in a body has brackets, and
-- 115 of them anywhere in an issue made the file unreadable for good. The
-- escape rule is the one the writer emits and the reader honours, so a quote
-- inside a string does not end it.
--
-- The escapes are counted in the same pass because the same state machine
-- already has to know where they are, and because they are the other half of
-- the cost: see 'maxEscapes'.
--
-- WHAT IS COUNTED IS AN ITEM AT THE TOP LEVEL, and the previous version of this
-- counted punctuation instead. That is the whole of the correction, and the hole
-- it closes was reachable through @hub verify@ on any repository, measured end
-- to end on this build with a @version@ file of bare atoms separated by spaces
-- and holding not one newline:
--
-- >  honest file:   45 ms
-- >   32 KB:      2225 ms
-- >   64 KB:      8899 ms
-- >  128 KB:     36581 ms
--
-- Four times the cost for twice the input, and 128 KB is well inside
-- 'maxEventBytes'. That file reported ONE form and passed every bound, because
-- an atom opens no bracket and ends at no newline. The cost is 'parseTop'
-- accumulating top-level items with a left-nested append, so it is quadratic in
-- how many items the top level has and in nothing else: the same 65536 atoms
-- moved inside one enclosing form parse in 317 ms, which is why depth is tracked
-- here and why an atom is counted at depth zero only.
--
-- A string literal at the top level is an item by the same argument and is
-- counted the same way, since @\"a\" \"b\" \"c\"@ needs no bracket either.
--
-- Nothing a writer emits is counted differently than before: 'renderEvent' puts
-- one bracketed clause on each line, so the top level of a real file holds no
-- bare atom and no bare string, and the count stays two per line against a limit
-- of a hundred and twenty-eight.
scanText :: Text -> (Int, Int)
scanText = seen . Text.foldl' step (0 :: Int, 0 :: Int, Plain, 0 :: Int, False)
  where
    seen (n,e,_,_,_) = (n,e)

    -- The last two components are how deep in brackets the scan is, and whether
    -- it is in the middle of a bare token. Both exist to answer one question --
    -- is this character the START of an item at the TOP level -- which cannot be
    -- answered by looking at the character alone.
    step (n, e, st, d, tok) c = case st of
      InComment | c == '\n'   -> (n + 1, e, Plain, d, False)
                | otherwise   -> (n, e, InComment, d, tok)
      InEscape                -> (n, e, InString, d, tok)
      InString  | c == '\\'   -> (n, e + 1, InEscape, d, tok)
                | c == '"'    -> (n, e, Plain, d, False)
                | otherwise   -> (n, e, InString, d, tok)
      -- Counted where it opens and not where it closes, so the item is counted
      -- once; the closing quote comes back through the InString branch above.
      Plain     | c == '"'    -> (item, e, InString, d, False)
                -- A comment is tracked for one reason: a quote inside one
                -- would otherwise put this counter into a string it is not in,
                -- and everything after it would go uncounted.
                | c == ';'    -> (n, e, InComment, d, False)
                | closes c    -> (n, e, Plain, max 0 (d - 1), False)
                | nests c     -> (n + 1, e, Plain, d + 1, False)
                | marks c     -> (n + 1, e, Plain, d, False)
                | isSpace c   -> (n, e, Plain, d, False)
                | otherwise   -> (item, e, Plain, d, True)
      where
        -- One count for the whole run of characters that makes up a bare token,
        -- and none at all for a token inside a form.
        item | d > 0     = n
             | tok       = n
             | otherwise = n + 1

    -- The brackets, which are the only punctuation that nests.
    nests  c = c `elem` ("([{" :: String)
    closes c = c `elem` (")]}" :: String)

    -- A quote, quasiquote or unquote makes a list of what follows it, and a
    -- non-empty LINE makes a list of what is on it. Neither nests, so neither
    -- moves the depth; both are still counted wherever they appear, which is
    -- what they were counted as before and is left alone.
    marks  c = c `elem` ("'`,\n" :: String)

-- Where a scan of the text is, for 'scanText'.
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
