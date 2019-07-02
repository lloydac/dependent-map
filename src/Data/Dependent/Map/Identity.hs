{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RankNTypes       #-}

-- | The exports from "Data.Dependent.Map" specialized to 'Identity'.
module Data.Dependent.Map.Identity
    ( DMap
    , DSum
    , pattern (:=>)
    , Some(..)
    , GEq(..)
    , GCompare(..)
    , GOrdering(..)

    -- * Operators
    , (!), (\\)

    -- * Query
    , null
    , size
    , member
    , notMember
    , lookup
    , findWithDefault

    -- * Construction
    , empty
    , singleton

    -- ** Insertion
    , insert
    , insertWith
    , insertWith'
    , insertWithKey
    , insertWithKey'
    , insertLookupWithKey
    , insertLookupWithKey'

    -- ** Delete\/Update
    , delete
    , adjust
    , adjustWithKey
    , adjustWithKey'
    , update
    , updateWithKey
    , updateLookupWithKey
    , alter
    , alterF

    -- * Combine

    -- ** Union
    , union
    , unionWithKey
    , unions
    , unionsWithKey

    -- ** Difference
    , difference
    , differenceWithKey

    -- ** Intersection
    , intersection
    , intersectionWithKey

    -- * Traversal
    -- ** Map
    , map
    , mapWithKey
    , traverseWithKey
    , mapAccumLWithKey
    , mapAccumRWithKey
    , mapKeysWith
    , mapKeysMonotonic

    -- ** Fold
    , foldrWithKey
    , foldlWithKey

    -- * Conversion
    , keys
    , assocs

    -- ** Lists
    , toList
    , fromList
    , fromListWithKey

    -- ** Ordered lists
    , toAscList
    , toDescList
    , fromAscList
    , fromAscListWithKey
    , fromDistinctAscList

    -- * Filter
    , filter
    , filterWithKey
    , partitionWithKey

    , mapMaybe
    , mapMaybeWithKey
    , mapEitherWithKey

    , split
    , splitLookup

    -- * Submap
    , isSubmapOf, isSubmapOfBy
    , isProperSubmapOf, isProperSubmapOfBy

    -- * Indexed
    , lookupIndex
    , findIndex
    , elemAt
    , updateAt
    , deleteAt

    -- * Min\/Max
    , findMin
    , findMax
    , lookupMin
    , lookupMax
    , deleteMin
    , deleteMax
    , deleteFindMin
    , deleteFindMax
    , updateMinWithKey
    , updateMaxWithKey
    , minViewWithKey
    , maxViewWithKey
    ) where

import Prelude hiding (null, lookup, map)

import Control.Applicative (liftA2)
import Control.Arrow (first, second)
import Data.Coerce (coerce)
import Data.Functor.Identity
import Data.Function ((&))

import Data.Dependent.Sum.Identity (DSum, pattern (:=>), EqTag)
import Data.GADT.Compare (GCompare(..), GEq(..), GOrdering(..))
import Data.Some (Some(..))

import qualified Data.Dependent.Map as DMap

-- | 'DMap.DMap' type specialized to 'Identity'.
type DMap k = DMap.DMap k Identity

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 !,\\ --

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
--
-- > fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
-- > fromList [(5,'a'), (3,'b')] ! 5 == 'a'

(!) :: GCompare k => DMap k -> k v -> v
(!) m k = runIdentity ((DMap.!) m k)

-- | Same as 'difference'.
(\\) :: GCompare k => DMap k -> DMap k -> DMap k
(\\) = (DMap.\\)

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}

-- | /O(1)/. The empty map.
--
-- > empty      == fromList []
-- > size empty == 0
empty :: DMap k
empty = DMap.empty

-- | /O(1)/. A map with a single element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1
singleton :: k v -> v -> DMap k
singleton k = DMap.singleton k . Identity

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(1)/. Is the map empty?
null :: DMap k -> Bool
null = DMap.null

-- | /O(1)/. The number of elements in the map.
size :: DMap k -> Int
size = DMap.size

-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
member :: GCompare k => k a -> DMap k -> Bool
member = DMap.member

-- | /O(log n)/. Is the key not a member of the map? See also 'member'.
notMember :: GCompare k => k v -> DMap k -> Bool
notMember = DMap.notMember

-- | /O(log n)/. Lookup the value at a key in the map.
--
-- The function will return the corresponding value as @('Just' value)@,
-- or 'Nothing' if the key isn't in the map.
lookup :: GCompare k => k v -> DMap k -> Maybe v
lookup k = fmap runIdentity . DMap.lookup k

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
-- Consider using 'lookup' when elements may not be present.
-- find :: GCompare k => k v -> DMap k -> v
-- find k m = runIdentity (DMap.find k m)

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
findWithDefault :: GCompare k => v -> k v -> DMap k -> v
findWithDefault def k m = runIdentity (DMap.findWithDefault (Identity def) k m)

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}

-- | /O(log n)/. Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
insert :: GCompare k => k v -> v -> DMap k -> DMap k
insert k = DMap.insert k . Identity

-- | /O(log n)/. Insert a new key and value in the map if the key
-- is not already present. If the key is already present, @insertR@
-- does nothing.
-- insertR :: GCompare k => k v -> v -> DMap k -> DMap k
-- insertR k = DMap.insertR k . Identity

-- | /O(log n)/. Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@
-- will insert the entry @key :=> value@ into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the entry @key :=> f new_value old_value@.
insertWith :: GCompare k => (v -> v -> v) -> k v -> v -> DMap k -> DMap k
insertWith f k = DMap.insertWith (liftA2 f) k . Identity

-- | Same as 'insertWith', but the combining function is applied strictly.
-- This is often the most desirable behavior.
insertWith' :: GCompare k => (v -> v -> v) -> k v -> v -> DMap k -> DMap k
insertWith' f k = DMap.insertWith' (liftA2 f) k . Identity

-- | /O(log n)/. Insert with a function, combining key, new value and old value.
-- @'insertWithKey' f key value mp@
-- will insert the entry @key :=> value@ into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the entry @key :=> f key new_value old_value@.
-- Note that the key passed to f is the same key passed to 'insertWithKey'.
insertWithKey :: GCompare k => (k v -> v -> v -> v) -> k v -> v -> DMap k -> DMap k
insertWithKey f k = DMap.insertWithKey (liftA2 . f) k . Identity

-- | Same as 'insertWithKey', but the combining function is applied strictly.
insertWithKey' :: GCompare k => (k v -> v -> v -> v) -> k v -> v -> DMap k -> DMap k
insertWithKey' f k = DMap.insertWithKey' (liftA2 . f) k . Identity

-- | /O(log n)/. Combines insert operation with old value retrieval.
-- The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
insertLookupWithKey :: GCompare k => (k v -> v -> v -> v) -> k v -> v -> DMap k -> (Maybe v, DMap k)
insertLookupWithKey f k v = first (fmap runIdentity) . DMap.insertLookupWithKey (liftA2 . f) k (Identity v)

-- | /O(log n)/. A strict version of 'insertLookupWithKey'.
insertLookupWithKey' :: GCompare k => (k v -> v -> v -> v) -> k v -> v -> DMap k -> (Maybe v, DMap k)
insertLookupWithKey' f k v = first (fmap runIdentity) . DMap.insertLookupWithKey' (liftA2 . f) k (Identity v)

{--------------------------------------------------------------------
  Deletion
--------------------------------------------------------------------}

-- | /O(log n)/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
delete :: GCompare k => k v -> DMap k -> DMap k
delete = DMap.delete

-- | /O(log n)/. Update a value at a specific key with the result of the provided function.
-- When the key is not
-- a member of the map, the original map is returned.
adjust :: GCompare k => (v -> v) -> k v -> DMap k -> DMap k
adjust f = DMap.adjust (fmap f)

-- | /O(log n)/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
adjustWithKey :: GCompare k => (k v -> v -> v) -> k v -> DMap k -> DMap k
adjustWithKey f = DMap.adjustWithKey (fmap . f)

-- | /O(log n)/. A strict version of 'adjustWithKey'.
adjustWithKey' :: GCompare k => (k v -> v -> v) -> k v -> DMap k -> DMap k
adjustWithKey' f = DMap.adjustWithKey' (fmap . f)

-- | /O(log n)/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
update :: GCompare k => (v -> Maybe v) -> k v -> DMap k -> DMap k
update f = DMap.update (sequence . fmap f)

-- | /O(log n)/. The expression (@'updateWithKey' f k map@) updates the
-- value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
-- the element is deleted. If it is (@'Just' y@), the key @k@ is bound
-- to the new value @y@.
updateWithKey :: GCompare k => (k v -> v -> Maybe v) -> k v -> DMap k -> DMap k
updateWithKey f = DMap.updateWithKey (\k -> sequence . fmap (f k))

-- | /O(log n)/. Lookup and update. See also 'updateWithKey'.
-- The function returns changed value, if it is updated.
-- Returns the original key value if the map entry is deleted.
updateLookupWithKey :: GCompare k => (k v -> v -> Maybe v) -> k v -> DMap k -> (Maybe v, DMap k)
updateLookupWithKey f k = first (fmap runIdentity) . DMap.updateLookupWithKey (\k' -> sequence . fmap (f k')) k

-- | /O(log n)/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'Map'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
alter :: GCompare k => (Maybe v -> Maybe v) -> k v -> DMap k -> DMap k
alter f = DMap.alter (sequence . fmap f . sequence)

-- | Works the same as 'alter' except the new value is return in some 'Functor' @f@.
-- In short : @(\v' -> alter (const v') k dm) <$> f (lookup k dm)@
alterF :: (GCompare k, Functor f) => k v -> (Maybe v -> f (Maybe v)) -> DMap k -> f (DMap k)
alterF k f = DMap.alterF k (fmap (fmap Identity) . runIdentity . fmap f . sequence)

{--------------------------------------------------------------------
  Indexing
--------------------------------------------------------------------}

-- | /O(log n)/. Return the /index/ of a key. The index is a number from
-- /0/ up to, but not including, the 'size' of the map. Calls 'error' when
-- the key is not a 'member' of the map.
findIndex :: GCompare k => k v -> DMap k -> Int
findIndex = DMap.findIndex

-- | /O(log n)/. Lookup the /index/ of a key. The index is a number from
-- /0/ up to, but not including, the 'size' of the map.
lookupIndex :: GCompare k => k v -> DMap k -> Maybe Int
lookupIndex = DMap.lookupIndex

-- | /O(log n)/. Retrieve an element by /index/. Calls 'error' when an
-- invalid index is used.
elemAt :: Int -> DMap k -> DSum k
elemAt = DMap.elemAt

-- | /O(log n)/. Update the element at /index/. Does nothing when an
-- invalid index is used.
updateAt :: (forall v. k v -> v -> Maybe v) -> Int -> DMap k -> DMap k
updateAt f = DMap.updateAt (\k -> sequence . fmap (f k))

-- | /O(log n)/. Delete the element at /index/.
-- Defined as (@'deleteAt' i map = 'updateAt' (\k x -> 'Nothing') i map@).
deleteAt :: Int -> DMap k -> DMap k
deleteAt = DMap.deleteAt

{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}

-- | /O(log n)/. The minimal key of the map. Calls 'error' is the map is empty.
findMin :: DMap k -> DSum k
findMin = DMap.findMin

lookupMin :: DMap k -> Maybe (DSum k)
lookupMin = DMap.lookupMin

-- | /O(log n)/. The maximal key of the map. Calls 'error' is the map is empty.
findMax :: DMap k -> DSum k
findMax = DMap.findMax

lookupMax :: DMap k -> Maybe (DSum k)
lookupMax = DMap.lookupMax

-- | /O(log n)/. Delete the minimal key. Returns an empty map if the map is empty.
deleteMin :: DMap k -> DMap k
deleteMin = DMap.deleteMin

-- | /O(log n)/. Delete the maximal key. Returns an empty map if the map is empty.
deleteMax :: DMap k -> DMap k
deleteMax = DMap.deleteMax

-- | /O(log n)/. Delete and find the maximal element.
--
-- > deleteFindMax (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((10,"c"), fromList [(3,"b"), (5,"a")])
-- > deleteFindMax empty                                      Error: can not return the maximal element of an empty map
deleteFindMax :: DMap k -> (DSum k, DMap k)
deleteFindMax = DMap.deleteFindMax

-- | /O(log n)/. Delete and find the minimal element.
--
-- > deleteFindMin (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((3,"b"), fromList[(5,"a"), (10,"c")])
-- > deleteFindMin                                            Error: can not return the minimal element of an empty map
deleteFindMin :: DMap k -> (DSum k, DMap k)
deleteFindMin = DMap.deleteFindMin

-- | /O(log n)/. Retrieves the minimal (key :=> value) entry of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
minViewWithKey :: DMap k -> Maybe (DSum k, DMap k)
minViewWithKey = DMap.minViewWithKey

-- | /O(log n)/. Retrieves the maximal (key :=> value) entry of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
maxViewWithKey :: DMap k -> Maybe (DSum k, DMap k)
maxViewWithKey = DMap.maxViewWithKey

-- | /O(log n)/. Update the value at the minimal key.
updateMinWithKey :: (forall v. k v -> v -> Maybe v) -> DMap k -> DMap k
updateMinWithKey f = DMap.updateMinWithKey (\k -> sequence . fmap (f k))

-- | /O(log n)/. Update the value at the maximal key.
updateMaxWithKey :: (forall v. k v -> v -> Maybe v) -> DMap k -> DMap k
updateMaxWithKey f = DMap.updateMaxWithKey (\k -> sequence . fmap (f k))

{--------------------------------------------------------------------
  Union.
--------------------------------------------------------------------}

-- | The union of a list of maps:
--   (@'unions' == 'Prelude.foldl' 'union' 'empty'@).
unions :: GCompare k => [DMap k] -> DMap k
unions = DMap.unions

-- | The union of a list of maps, with a combining operation:
--   (@'unionsWithKey' f == 'Prelude.foldl' ('unionWithKey' f) 'empty'@).
unionsWithKey :: GCompare k => (forall v. k v -> v -> v -> v) -> [DMap k] -> DMap k
unionsWithKey f = DMap.unionsWithKey (liftA2 . f)

-- | /O(m*log(n\/m + 1)), m <= n/.
-- The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and @t2@.
-- It prefers @t1@ when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
union :: GCompare k => DMap k -> DMap k -> DMap k
union = DMap.union

-- | /O(n+m)/.
-- Union with a combining function.
unionWithKey :: GCompare k => (forall v. k v -> v -> v -> v) -> DMap k -> DMap k -> DMap k
unionWithKey f = DMap.unionWithKey (liftA2 . f)

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}

-- | /O(m * log (n\/m + 1)), m <= n/. Difference of two maps.
-- Return elements of the first map not existing in the second map.
difference :: GCompare k => DMap k -> DMap k -> DMap k
difference = DMap.difference

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
differenceWithKey :: GCompare k => (forall v. k v -> v -> v -> Maybe v) -> DMap k -> DMap k -> DMap k
differenceWithKey f = DMap.differenceWithKey (\k v -> sequence . liftA2 (f k) v)

{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}

-- | /O(m * log (n\/m + 1), m <= n/. Intersection of two maps.
-- Return data in the first map for the keys existing in both maps.
-- (@'intersection' m1 m2 == 'intersectionWith' 'const' m1 m2@).
intersection :: GCompare k => DMap k -> DMap k -> DMap k
intersection = DMap.intersection

-- | /O(m * log (n\/m + 1), m <= n/. Intersection with a combining function.
intersectionWithKey :: GCompare k => (forall v. k v -> v -> v -> v) -> DMap k -> DMap k -> DMap k
intersectionWithKey f = DMap.intersectionWithKey (liftA2 . f)

{--------------------------------------------------------------------
  Submap
--------------------------------------------------------------------}
-- | /O(n+m)/.
-- This function is defined as (@'isSubmapOf' = 'isSubmapOfBy' 'eqTagged')@).
isSubmapOf :: (GCompare k, EqTag k Identity) => DMap k -> DMap k -> Bool
isSubmapOf = DMap.isSubmapOf

{- | /O(n+m)/.
 The expression (@'isSubmapOfBy' f t1 t2@) returns 'True' if
 all keys in @t1@ are in tree @t2@, and when @f@ returns 'True' when
 applied to their respective keys and values.
-}
isSubmapOfBy :: GCompare k => (forall v. k v -> k v -> v -> v -> Bool) -> DMap k -> DMap k -> Bool
isSubmapOfBy f = DMap.isSubmapOfBy (\k1 k2 (Identity v1) (Identity v2) -> f k1 k2 v1 v2)

-- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' 'eqTagged'@).
isProperSubmapOf :: (GCompare k, EqTag k Identity) => DMap k -> DMap k -> Bool
isProperSubmapOf = DMap.isProperSubmapOf

{- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
 The expression (@'isProperSubmapOfBy' f m1 m2@) returns 'True' when
 @m1@ and @m2@ are not equal,
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective keys and values.
-}
isProperSubmapOfBy :: GCompare k => (forall v. k v -> k v -> v -> v -> Bool) -> DMap k -> DMap k -> Bool
isProperSubmapOfBy f = DMap.isProperSubmapOfBy (\k1 k2 (Identity v1) (Identity v2) -> f k1 k2 v1 v2)

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}

-- | /O(n)/. Filter all keys\/values that satisfy the predicate.
filterWithKey :: GCompare k => (forall v. k v -> v -> Bool) -> DMap k -> DMap k
filterWithKey f = DMap.filterWithKey (\k (Identity v) -> f k v)

-- | /O(n)/. Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
partitionWithKey :: GCompare k => (forall v. k v -> v -> Bool) -> DMap k -> (DMap k, DMap k)
partitionWithKey f = DMap.partitionWithKey (\k (Identity v) -> f k v)

-- | /O(n)/. Map values and collect the 'Just' results.
mapMaybe :: GCompare k => (forall v. v -> Maybe v) -> DMap k -> DMap k
mapMaybe f = DMap.mapMaybe (sequence . fmap f)

-- | /O(n)/. Map keys\/values and collect the 'Just' results.
mapMaybeWithKey :: GCompare k => (forall v. k v -> v -> Maybe v) -> DMap k -> DMap k
mapMaybeWithKey f = DMap.mapMaybeWithKey (\k -> sequence . fmap (f k))

-- | /O(n)/. Map keys\/values and separate the 'Left' and 'Right' results.
mapEitherWithKey :: GCompare k => (forall v. k v -> v -> Either v v) -> DMap k -> (DMap k, DMap k)
mapEitherWithKey f = DMap.mapEitherWithKey (\k -> coerce . f k . runIdentity)

{--------------------------------------------------------------------
  Mapping
--------------------------------------------------------------------}

-- | /O(n)/. Map a function over all values in the map.
map :: (forall v. v -> v) -> DMap k -> DMap k
map f = DMap.map (fmap f)

-- | /O(n)/. Map a function over all values in the map.
mapWithKey :: (forall v. k v -> v -> v) -> DMap k -> DMap k
mapWithKey f = DMap.mapWithKey (fmap . f)

-- | /O(n)/.
-- @'traverseWithKey' f m == 'fromList' <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value.
traverseWithKey :: Applicative f => (forall v. k v -> v -> f v) -> DMap k -> f (DMap k)
traverseWithKey f = DMap.traverseWithKey (\k -> sequenceA . fmap (f k))

-- | /O(n)/. The function 'mapAccumLWithKey' threads an accumulating
-- argument throught the map in ascending order of keys.
mapAccumLWithKey :: (forall v. a -> k v -> v -> (a, v)) -> a -> DMap k -> (a, DMap k)
mapAccumLWithKey f = DMap.mapAccumLWithKey (\a k (Identity v) -> f a k v & second Identity)

-- | /O(n)/. The function 'mapAccumRWithKey' threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (forall v. a -> k v -> v -> (a, v)) -> a -> DMap k -> (a, DMap k)
mapAccumRWithKey f = DMap.mapAccumRWithKey (\a k (Identity v) -> f a k v & second Identity)

-- | /O(n*log n)/.
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.
mapKeysWith :: GCompare k2 => (forall v. k2 v -> v -> v -> v) -> (forall v. k1 v -> k2 v) -> DMap k1 -> DMap k2
mapKeysWith c = DMap.mapKeysWith (liftA2 . c)

-- | /O(n)/.
-- @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
-- is strictly monotonic.
-- That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
-- /The precondition is not checked./
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapKeysMonotonic f s == mapKeys f s
-- >     where ls = keys s
--
-- This means that @f@ maps distinct original keys to distinct resulting keys.
-- This function has better performance than 'mapKeys'.
mapKeysMonotonic :: (forall v. k1 v -> k2 v) -> DMap k1 -> DMap k2
mapKeysMonotonic = DMap.mapKeysMonotonic

{--------------------------------------------------------------------
  Folds
--------------------------------------------------------------------}

-- | /O(n)/. Post-order fold.  The function will be applied from the lowest
-- value to the highest.
foldrWithKey :: (forall v. k v -> v -> b -> b) -> b -> DMap k -> b
foldrWithKey f = DMap.foldrWithKey (\k (Identity v) b -> f k v b)

-- | /O(n)/. Pre-order fold.  The function will be applied from the highest
-- value to the lowest.
foldlWithKey :: (forall v. b -> k v -> v -> b) -> b -> DMap k -> b
foldlWithKey f = DMap.foldlWithKey (\b k (Identity v) -> f b k v)

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}

-- | /O(n)/. Return all keys of the map in ascending order.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []

keys :: DMap k -> [Some k]
keys = DMap.keys

-- | /O(n)/. Return all key\/value pairs in the map in ascending key order.
assocs :: DMap k -> [DSum k]
assocs = DMap.assocs

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}

-- | /O(n*log n)/. Build a map from a list of key\/value pairs. See also 'fromAscList'.
-- If the list contains more than one value for the same key, the last value
-- for the key is retained.
fromList :: GCompare k => [DSum k] -> DMap k
fromList = DMap.fromList

-- | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWithKey'.
fromListWithKey :: GCompare k => (forall v. k v -> v -> v -> v) -> [DSum k] -> DMap k
fromListWithKey f = DMap.fromListWithKey (liftA2 . f)

-- | /O(n)/. Convert to a list of key\/value pairs.
toList :: DMap k -> [DSum k]
toList = DMap.toList

-- | /O(n)/. Convert to an ascending list.
toAscList :: DMap k -> [DSum k]
toAscList = DMap.toAscList

-- | /O(n)/. Convert to a descending list.
toDescList :: DMap k -> [DSum k]
toDescList = DMap.toDescList

-- | /O(n)/. Build a map from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: GEq k => [DSum k] -> DMap k
fromAscList = DMap.fromAscList

-- | /O(n)/. Build a map from an ascending list in linear time with a
-- combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
fromAscListWithKey :: GEq k => (forall v. k v -> v -> v -> v) -> [DSum k] -> DMap k
fromAscListWithKey f = DMap.fromAscListWithKey (liftA2 . f)


-- | /O(n)/. Build a map from an ascending list of distinct elements in linear time.
-- /The precondition is not checked./
fromDistinctAscList :: [DSum k] -> DMap k
fromDistinctAscList = DMap.fromDistinctAscList

{--------------------------------------------------------------------
  Split
--------------------------------------------------------------------}

-- | /O(log n)/. The expression (@'split' k map@) is a pair @(map1,map2)@ where
-- the keys in @map1@ are smaller than @k@ and the keys in @map2@ larger than @k@.
-- Any key equal to @k@ is found in neither @map1@ nor @map2@.
split :: GCompare k => k v -> DMap k -> (DMap k, DMap k)
split = DMap.split

-- | /O(log n)/. The expression (@'splitLookup' k map@) splits a map just
-- like 'split' but also returns @'lookup' k map@.
splitLookup :: GCompare k => k v -> DMap k -> (DMap k, Maybe v, DMap k)
splitLookup k m =
  case DMap.splitLookup k m of
    (m1, v, m2) -> (m1, runIdentity <$> v, m2)
