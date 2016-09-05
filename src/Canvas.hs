{-# LANGUAGE BangPatterns #-}

module Canvas
( module Tile
, module Unit
, fill
) where

import Tile
import CheckedGrid (CheckedGrid)
import Grid        (Grid)
import Unit
import qualified CheckedGrid
import qualified Grid

import Data.List
import System.Random
import qualified Data.IntMap.Strict as Map
import Debug.Trace

data Canvas = Canvas [((Unit, Unit), Tile)]
              deriving (Eq)

fill :: [TileSpec] -> [LetterSpec] -> Char -> Unit -> Unit -> StdGen -> Either String (Maybe Canvas)
fill ts ls defaultL height width stdgen 
  = case mkTileFac ts of
      Left  s  -> Left s
      Right tf -> case mkLetterFac ls of
                    Left  s  -> Left s
                    Right lf -> Right $ _fill tf lf defaultL width height stdgen -- This wrapper exists to flip these two values as there is currently a bug
                    
_fill :: TileFactory -> LetterFactory -> Char -> Unit -> Unit -> StdGen -> Maybe Canvas
_fill tf lf defaultL height width stdgen
  = let res = go stdgen (Grid.fromList []) (CheckedGrid.toList checkedgrid) []
    in  fmap Canvas (fmap reverse res)
    
    where
      checkedgrid = CheckedGrid.createCheckedGrid height width
      
      placeable :: [(Unit, Unit)] -> Grid -> Bool
      placeable cs used = (not $ any (`Grid.elem` used) cs) && all (`CheckedGrid.elem` checkedgrid) cs
      
      maxCount = let (Unit h) = height
                     (Unit w) = width
                 in  fromIntegral (h * w)
      
      go :: StdGen -> Grid -> [(Unit, Unit)] -> [((Unit, Unit), Tile)] -> Maybe [((Unit, Unit), Tile)]
      go _   _     []     result  = Just result
      go gen !used (x:xs) !result
        | x `Grid.elem` used = go gen used xs result
        | otherwise          = let (g, gg) = split gen
                                   res     = try g 0
                               in  res >>= \(t,tC) -> go gg (Grid.insertMany tC used) xs ((x,t):result)
        where
          try :: StdGen -> Int -> Maybe (Tile, [(Unit, Unit)])
          try g count
            | count >= maxCount = Nothing
            | otherwise         = let t  = getTile defaultL tf lf g
                                      tC = tileCoords x t
                                  in  if   placeable tC used
                                      then Just (t, tC)
                                      else try (fst $ split g) (count + 1)
 
tileCoords :: (Unit, Unit) -> Tile -> [(Unit, Unit)]
tileCoords (x, y) t = [ (x + z, y + z2) | z <- [0..(getSize t) - 1], z2 <- [0..(getSize t) - 1] ]
      
instance Show Canvas where
  show (Canvas list) = mapToString $ translateToMap list Map.empty
                       where
                         translateToMap :: [((Unit, Unit), Tile)] -> Map.IntMap (Map.IntMap Char) -> Map.IntMap (Map.IntMap Char)
                         translateToMap []         result = result
                         translateToMap ((c,t):xs) result 
                           = let tC = tileCoords c t
                                 ls = map (\(Unit a, Unit b) -> (fromIntegral a, fromIntegral b, getLetter t)) tC
                             in  translateToMap xs (foldl' ins result ls)
                             where
                               ins m (a, b, l) = Map.alter ins2 a m
                                                 where
                                                   ins2 Nothing   = Just $ Map.fromList [(b,l)]
                                                   ins2 (Just m2) = Just $ Map.insert b l m2
                                                   
                         mapToString :: Map.IntMap (Map.IntMap Char) -> String
                         mapToString m = intercalate "\n" $ Map.elems (Map.map (reverse . (Map.foldr (:) "")) m)