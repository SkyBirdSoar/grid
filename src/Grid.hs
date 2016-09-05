module Grid
( module Unit
, Grid
, elem
, fromList
, insert
, insertMany
, delete
, deleteAll
, trim
, toList
, length
) where

import Unit

import qualified Data.IntMap.Strict as Map ( IntMap
                                           , fromList
                                           , toList
                                           , lookup
                                           , alter
                                           , delete
                                           , filter
                                           , map
                                           , foldl'
                                           )
import qualified Data.IntSet as Set        ( IntSet
                                           , fromList
                                           , toList
                                           , member
                                           , insert
                                           , delete
                                           , null
                                           , size
                                           )

import           Prelude hiding (elem, length)
import qualified Prelude (length)

newtype Grid = Grid (Map.IntMap Set.IntSet)
               deriving (Eq, Show)

fromList :: [(Unit, [Unit])] -> Grid
fromList xs = trim $ Grid (Map.fromList (map convert xs))
              where
                convert ((Unit u), us) = (fromInteger u, Set.fromList (map (fromInteger . unpack) us))
                unpack (Unit u) = u

elem :: (Unit, Unit) -> Grid -> Bool
elem (Unit x, Unit y) (Grid m) = case Map.lookup (fromInteger x) m of
                                   Nothing -> False
                                   Just s  -> Set.member (fromInteger y) s

insert :: (Unit, Unit) -> Grid -> Grid
insert (Unit x, Unit y) (Grid m) = Grid $ Map.alter f (fromInteger x) m
                                   where
                                     f Nothing  = Just $ Set.fromList [fromInteger y]
                                     f (Just s) = Just $ Set.insert (fromInteger y) s
                                  
insertMany :: [(Unit, Unit)] -> Grid -> Grid
insertMany list (Grid m) = Grid $ res list m
                           where
                             res [] m = m
                             res ((Unit x, Unit y):xs) m = res xs (Map.alter f (fromInteger x) m)
                                                           where
                                                             f Nothing  = Just $ Set.fromList [fromInteger y]
                                                             f (Just s) = Just $ Set.insert (fromInteger y) s

delete :: (Unit, Unit) -> Grid -> Grid
delete (Unit x, Unit y) (Grid m) = Grid $ Map.alter f (fromInteger x) m
                                   where
                                     f Nothing  = Nothing
                                     f (Just s) = Just $ Set.delete (fromInteger y) s
                                  
deleteAll :: Unit -> Grid -> Grid
deleteAll (Unit x) (Grid m) = Grid $ Map.delete (fromInteger x) m

trim :: Grid -> Grid
trim (Grid m) = Grid $ Map.filter (not . Set.null) m

toList :: Grid -> [(Unit, Unit)]
toList (Grid m) = concatMap f $ Map.toList $ Map.map (Set.toList) m
                  where
                    f (x,ys) = [ (fromIntegral x, fromIntegral y) | y <- ys ]
                    
length :: Grid -> Int
length (Grid m) = Map.foldl' (\acc s -> acc + Set.size s) 0 m