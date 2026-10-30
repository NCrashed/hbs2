module BlockSizeCacheState where

-- | What a peer's cached answer says about a block at the current announcement
-- revision.  Positive answers are stable: a peer that had a block still has a
-- useful size for it.  Negative answers are only evidence for the revision in
-- which they were observed; a later announcement means the peer may have
-- acquired the block since then.
data BlockSizeCacheResult
  = BlockSizeKnown Integer
  | BlockSizeAbsent
  | BlockSizeUnknown
  deriving stock (Eq, Show)

lookupBlockSize
  :: Eq revision
  => revision
  -> Maybe (revision, Maybe Integer)
  -> BlockSizeCacheResult
lookupBlockSize current = \case
  Just (_, Just size) -> BlockSizeKnown size
  Just (cached, Nothing)
    | cached == current -> BlockSizeAbsent
  _ -> BlockSizeUnknown
