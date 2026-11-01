-- | Bounded analysis-result cache keyed by @(file path, source content)@.
--
-- Per-buffer analysis is already memoised inside 'Session.Documents', but
-- workspace-wide requests re-read and re-analyse sibling files on every
-- call. This cache lets the second hover / workspace-symbol / definition
-- request reuse the previous result whenever the content has not changed,
-- collapsing what was an O(workspace) cost into O(changed files).
--
-- Entries carry a monotonically increasing sequence number so the cache
-- can be capped at 'analysisCacheCapacity' with simple LRU eviction. A
-- 'didChange' that changes the document text shifts the key, so stale
-- entries naturally fall out of the working set even before eviction.
module AnalysisCache
  ( AnalysisCache,
    AnalysisKey,
    accessAnalysisCache,
    analysisCacheCapacity,
    analysisCacheSize,
    emptyAnalysisCache,
    insertAnalysisCache,
    lookupAnalysisCache,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Driver (Analysis)

-- | Cache key: full path + the exact source text the analysis was produced
-- from. Two different files with identical content do not share entries
-- because tynix resolves imports relative to the source path.
type AnalysisKey = (FilePath, Text)

-- | Opaque cache value. Construct with 'emptyAnalysisCache' and mutate via
-- 'insertAnalysisCache'.
data AnalysisCache = AnalysisCache
  { analysisCacheEntries :: !(Map AnalysisKey (Int, Either String Analysis)),
    analysisCacheNext :: !Int
  }
  deriving (Show)

-- | Maximum number of entries kept in the cache. Once exceeded, least
-- recently used entries (lowest sequence numbers) are evicted.
analysisCacheCapacity :: Int
analysisCacheCapacity = 256

-- | Initial empty cache.
emptyAnalysisCache :: AnalysisCache
emptyAnalysisCache = AnalysisCache Map.empty 0

-- | Current number of cached entries.
analysisCacheSize :: AnalysisCache -> Int
analysisCacheSize = Map.size . analysisCacheEntries

-- | Look up a cached analysis result for the given key.
lookupAnalysisCache ::
  AnalysisKey -> AnalysisCache -> Maybe (Either String Analysis)
lookupAnalysisCache key cache = snd <$> Map.lookup key (analysisCacheEntries cache)

-- | Look up and mark an entry as recently used.
accessAnalysisCache ::
  AnalysisKey -> AnalysisCache -> (Maybe (Either String Analysis), AnalysisCache)
accessAnalysisCache key cache =
  case Map.lookup key (analysisCacheEntries cache) of
    Nothing -> (Nothing, cache)
    Just (_, value) ->
      let nextSeq = analysisCacheNext cache
          entries' = Map.insert key (nextSeq, value) (analysisCacheEntries cache)
       in ( Just value,
            cache{analysisCacheEntries = entries', analysisCacheNext = nextSeq + 1}
          )

-- | Insert or replace an analysis result, evicting the least recently used entries if
-- the cache would exceed 'analysisCacheCapacity'.
insertAnalysisCache ::
  AnalysisKey -> Either String Analysis -> AnalysisCache -> AnalysisCache
insertAnalysisCache key value cache =
  let nextSeq = analysisCacheNext cache
      added = Map.insert key (nextSeq, value) (analysisCacheEntries cache)
   in AnalysisCache (evictToCapacity added) (nextSeq + 1)

-- | Drop least-recently-used entries until the cache fits.
--
-- An insert can only push the cache one entry over, so this normally deletes a
-- single key. Deleting is the point: rebuilding the whole map from a sorted
-- list re-compares every key, and the keys hold full document text — on a
-- 256-entry cache that turned each keystroke into hundreds of text
-- comparisons. Finding the oldest entry is one linear scan over the sequence
-- numbers, and removing it is a single logarithmic delete.
evictToCapacity ::
  Map AnalysisKey (Int, Either String Analysis) ->
  Map AnalysisKey (Int, Either String Analysis)
evictToCapacity entries
  | Map.size entries <= analysisCacheCapacity = entries
  | otherwise =
      case oldestKey entries of
        Nothing -> entries
        Just key -> evictToCapacity (Map.delete key entries)

-- | The key with the lowest sequence number, i.e. the least recently used.
oldestKey :: Map AnalysisKey (Int, value) -> Maybe AnalysisKey
oldestKey = fmap fst . Map.foldrWithKey step Nothing
  where
    step key (seqNo, _) acc =
      case acc of
        Just (_, bestSeq) | bestSeq <= seqNo -> acc
        _ -> Just (key, seqNo)
