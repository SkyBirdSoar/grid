module Canvas
( module Tile
, Canvas
, mkCanvas
) where

import           Tile

import qualified Data.IntMap.Strict as Map
import           Data.List
import           System.Random

data Canvas = Canvas TileFactory LetterFactory Char Int Int StdGen
              
mkCanvas :: [TileSpec] -> [LetterSpec] -> Char -> Int -> Int -> StdGen -> Either String Canvas
mkCanvas ts ls defaultL height width stdgen 
  = case mkTileFac ts of
      Left  s  -> Left s
      Right tf -> case mkLetterFac ls of
                    Left  s  -> Left s
                    Right lf -> Right $ Canvas tf lf defaultL height width stdgen

tileCoords :: (Int, Int) -> Tile -> [(Int, Int)]
tileCoords (x, y) t = [ (x + z, y + z2) | z <- [0..getSize t - 1], z2 <- [0..getSize t - 1] ]

type MyMap = Map.IntMap (Map.IntMap Char)

instance Show Canvas where
  show (Canvas tf lf defaultL height width stdgen)
    = mapToString populateMapWithTiles
      where
        m :: MyMap
        m = Map.empty
              
        _m :: [(Int, Int)]
        _m = [ (x, y) | y <- [0..height - 1], x <- [0..width - 1] ]
        
        {-# INLINE letterExist #-}
        letterExist :: (Int, Int) -> MyMap -> Bool
        letterExist (x, y) ymap = case Map.lookup y ymap >>= Map.lookup x of
                                    Nothing -> False
                                    Just _  -> True
        
        {-# INLINE placeable #-}
        placeable :: [(Int, Int)] -> MyMap -> Bool
        placeable xs ymap = all (\x -> withinBounds x && not (x `letterExist` ymap)) xs
                            where
                              withinBounds (x, y) = ((y < height) && (x < width))
        
        updateMap :: Char -> MyMap -> [(Int, Int)] -> MyMap
        updateMap l = foldr forY
                      where
                        forY (x, y) = Map.alter forX y
                                      where
                                        forX (Just xmap) = Just $ Map.insert x l xmap
                                        forX Nothing     = Just $ Map.singleton x l
                                
        populateMapWithTiles :: MyMap
        populateMapWithTiles
          = snd $ foldl' f (stdgen, m) _m
            where
              f :: (StdGen, MyMap) -> (Int, Int) -> (StdGen, MyMap)
              f (g, result) x 
                = if   letterExist x result
                  then (g, result)
                  else let (g1, g2) = split g
                           t        = getTile defaultL tf lf g
                           tC       = tileCoords x t
                           tL       = getLetter t
                       in  if   placeable tC result
                           then (g1,  (updateMap tL result tC))
                           else f (g2, result) x

        mapToString :: MyMap -> String
        mapToString ymap = intercalate "\n" $ map (Map.foldr (:) "") (Map.elems ymap)
