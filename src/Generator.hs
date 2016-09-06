{-# LANGUAGE BangPatterns #-}

module Generator
( Generator
, Frequency
, mkGen
, pick
) where

import System.Random
import Data.List

newtype FData a = FData ((Int, Int), a)
                  deriving (Eq)

data Generator a = GSmall100 [FData a]
                 | GLarge100 [a]
                 deriving (Eq)

type Frequency = Int

mkGen :: [(Frequency, a)] -> Either String (Generator a)
mkGen list
  | totalFreq == 100 = if   length list <= 70 -- Arbitrarily chosen number
                       then Right $ mkGSmall100 list
                       else Right $ mkGLarge100 list
  | otherwise        = Left $ "Error: Generator: Total Frequency (" ++ show totalFreq ++ ") is not 100"
  where
    totalFreq = sum (map fst list)
    
mkGSmall100 :: [(Frequency, a)] -> Generator a
mkGSmall100 list
  = GSmall100 $ go 0 list []
  where
    go :: Int -> [(Frequency, a)] -> [FData a] -> [FData a]
    go _      []             result = result
    go !index ((freq, a):xs) result 
      = go nextIndex xs (FData ((index, nextIndex - 1), a):result)
        where
          nextIndex = index + freq

mkGLarge100 :: [(Frequency, a)] -> Generator a
mkGLarge100 = GLarge100 . concatMap expand
              where
                expand :: (Frequency, a) -> [a]
                expand (freq, a) = replicate freq a

_extract :: FData a -> a
_extract (FData (_, a)) = a

check :: Int -> FData a -> Bool
check key (FData ((x,y), _)) = key >= x && key <= y
                
pick :: Generator a -> StdGen -> a
pick (GSmall100 fds) gen = let key      = fst $ randomR (0, 99) gen
                               (Just a) = find (check key) fds 
                           in  _extract a
                             
pick (GLarge100 list) gen = list !! key
                            where
                              key = fst $ randomR (0, 99) gen
