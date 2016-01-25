{-# LANGUAGE RecordWildCards #-}

module HPack.Cache
(Cache, newCache, addElem)
where

import qualified Data.Map as M
import qualified Data.List as L

-- | Keep a cache of elements of a maximum size. When the maximum is
-- exceeded, indicate which elements should be deleted.
data Cache a
    = Cache
        { maxSize :: Int
            -- ^ Maximum number of elements that should be in the
            -- cache at any one time
        , priority :: Integer
            -- ^ Priority of the last inserted element
            -- (this grows unbounded)
        , elements :: M.Map a Integer
            -- ^ Elements of the cache. |elements| < maxSize
        }

newCache :: Int -> Cache a
newCache maxSize = Cache maxSize 0 M.empty

-- | Add an element to the cache. If maxSize is exceeded,
addElem :: Ord a => a -> Cache a -> (Cache a, [a])
addElem item Cache{..} =
    if M.size elems < maxSize
        then (mkCache elems, [])
        else (mkCache (M.fromList keepElems), map fst dropElems)
    where
        elems = M.insert item priority elements
        mkCache = Cache maxSize (priority - 1)
        sorted = L.sortOn snd (M.assocs elems)
        (keepElems, dropElems) = splitAt (maxSize `quot` 3 * 2) sorted
