{-# LANGUAGE BangPatterns #-}

module Canvas
( module Tile
, module Unit
, Canvas
, mkCanvas
) where

import           Tile
import           Unit

import           Control.Monad
import qualified Data.IntMap.Strict as Map
import           Data.List
import           System.Random

data Canvas = Canvas TileFactory LetterFactory Char Int Int StdGen
              
mkCanvas :: [TileSpec] -> [LetterSpec] -> Char -> Unit -> Unit -> StdGen -> Either String Canvas
mkCanvas ts ls defaultL height width stdgen 
  = case mkTileFac ts of
      Left  s  -> Left s
      Right tf -> case mkLetterFac ls of
                    Left  s  -> Left s
                    Right lf -> Right $ Canvas tf lf defaultL (unpack height) (unpack width) stdgen

{-# INLINE unpack #-}
unpack :: Unit -> Int
unpack (Unit s) = fromInteger s

tileCoords :: (Int, Int) -> Tile -> [(Int, Int)]
tileCoords (x, y) t = [ (x + z, y + z2) | z <- [0..(unpack $ getSize t) - 1], z2 <- [0..(unpack $ getSize t) - 1] ]

type MyMap = Map.IntMap (Map.IntMap (Maybe Char))

instance Show Canvas where
  show (Canvas tf lf defaultL height width stdgen)
    = mapToString (populateMapWithTiles stdgen m _m)
      where
        m :: MyMap
        m = ymap
            where
              ymap = Map.fromList [ (y,    xmap) | y <- [0..height - 1] ]
              xmap = Map.fromList [ (x, Nothing) | x <- [0..width  - 1] ]
              
        _m :: [(Int, Int)]
        _m = [ (x, y) | x <- [0..width - 1], y <- [0..height - 1] ]
        
        {-# INLINE letterExist #-}
        letterExist :: (Int, Int) -> MyMap -> Bool
        letterExist (x, y) ymap = case join (Map.lookup y ymap >>= Map.lookup x >>= return) of
                                    Nothing -> False
                                    Just _  -> True
                                                                
        placeable :: [(Int, Int)] -> MyMap -> Bool
        placeable xs m = all (\x -> withinBounds x && not (x `letterExist` m)) xs
                         where
                           withinBounds (x, y) = ((y < height) && (x < width))
        
        updateMap :: Char -> MyMap -> [(Int, Int)] -> MyMap
        updateMap l = foldr forY
                      where
                        forY (x, y) = Map.adjust forX y
                                      where
                                        forX = Map.adjust (const (Just l)) x

        populateMapWithTiles :: StdGen -> MyMap -> [(Int, Int)] -> MyMap
        populateMapWithTiles gen result
          = snd . foldl' f (gen, result)
            where
              f :: (StdGen, MyMap) -> (Int, Int) -> (StdGen, MyMap)
              f (gen, result) x 
                = if   letterExist x result
                  then (gen, result)
                  else let (g, gg) = split gen
                           t       = getTile defaultL tf lf gen
                           tC      = tileCoords x t
                           tL      = getLetter t
                       in  if   placeable tC result
                           then (g,  (updateMap tL result tC))
                           else f (gg, result) x
        {-}
        populateMapWithTiles :: [(Int, Int)] -> StdGen -> MyMap -> MyMap
        populateMapWithTiles []     _   result = result
        populateMapWithTiles (x:xs) gen result
          | letterExist x result = populateMapWithTiles xs gen result
          | otherwise            = let (g, gg) = split gen
                                       t       = getTile defaultL tf lf gen
                                       tC      = tileCoords x t
                                       tL      = getLetter t
                                   in  if   placeable tC result
                                       then populateMapWithTiles xs g (updateMap tL result tC)
                                       else populateMapWithTiles (x:xs) gg result
        -}                               
        mapToString :: MyMap -> String
        mapToString m = intercalate "\n" $ map (Map.foldr f "") (Map.elems m)
                        where
                          f (Just l) acc = l : acc
