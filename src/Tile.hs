{-# LANGUAGE DeriveGeneric 
           , OverloadedStrings #-}

module Tile 
( TileSpec (..)
, TileFactory
, mkTileFac

, LetterSpec (..)
, LetterFactory
, mkLetterFac

, Tile
, tileToLetters
, getSize
, getLetter

, getTile
) where

import           Generator (Generator)
import qualified Generator

import Control.Applicative
import Data.Aeson
import GHC.Generics
import System.Random

data TileSpec = TileSpec { size         :: Int
                         , chance       :: Int
                         , letterChance :: Double
                         } deriving (Eq, Generic, Show)
                   
data LetterSpec = LetterSpec { letter :: Char
                             , lchance :: Int
                             } deriving (Eq, Show)

instance FromJSON TileSpec

instance FromJSON LetterSpec where
  parseJSON (Object v) =   LetterSpec
                       <$> v .: "letter"
                       <*> v .: "chance"
  parseJSON _          = empty

--------------------------------------------------------------------------------

type TData = (Int, Double)
newtype TileFactory = HundredTileFac (Generator TData) -- handle maximum of 100 different tiles
                      deriving (Eq)

mkTileFac :: [TileSpec] -> Either String TileFactory
mkTileFac ts
  | length ts <= 100 = if spChance == 100
                       then factory
                       else Left $ "Error: TileFactory: Chance for all tiles should add up to exactly 100 (Current: " ++ show spChance ++ ")"
  | otherwise        = Left "Error: TileFactory can only handle up to 100 tiles"
  where
    spChance = sum (map chance ts)
    
    convertT :: TileSpec -> (Int, (Int, Double))
    convertT (TileSpec {size = s, chance = c, letterChance = l}) = (c, (s, l))
    
    factory = case Generator.mkGen (map convertT ts) of
                Left s    -> Left s
                Right gen -> Right $ HundredTileFac gen
                
--------------------------------------------------------------------------------

newtype LetterFactory = HundredLetterFac (Generator Char) -- handle maximum of 100 different tiles
                        deriving (Eq)

mkLetterFac :: [LetterSpec] -> Either String LetterFactory
mkLetterFac ls
  | length ls <= 100 = if spChance == 100
                       then factory
                       else Left $ "Error: LetterFactory: Chance for all letters should add up to exactly 100 (Current: " ++ show spChance ++ ")"
  | otherwise        = Left "Error: TileFactory can only handle up to 100 tiles"
  where
    spChance = sum (map lchance ls)
    
    convertL :: LetterSpec -> (Int, Char)
    convertL (LetterSpec {letter = l, lchance = c }) = (c, l)
    
    factory = case Generator.mkGen (map convertL ls) of
                Left s    -> Left s
                Right gen -> Right $ HundredLetterFac gen
                
--------------------------------------------------------------------------------
    
data Tile = Tile Int Char
            deriving (Eq, Show)
                   
tileToLetters :: Tile -> [String]
tileToLetters (Tile _size _letter) = replicate _size (replicate _size _letter)

{-# INLINE getSize #-}
getSize :: Tile -> Int
getSize (Tile s _) = s

{-# INLINE getLetter #-}
getLetter :: Tile -> Char
getLetter (Tile _ l) = l

--------------------------------------------------------------------------------

getTile :: Char -> TileFactory -> LetterFactory -> StdGen -> Tile
getTile defaultL (HundredTileFac gTData) (HundredLetterFac gLetter) stdgen
  = let (_size, lc) = Generator.pick gTData stdgen
        (g, gg)     = split stdgen
    in  Tile _size (if   (fst $ random g) <= lc
                    then (Generator.pick gLetter gg)
                    else defaultL)

--------------------------------------------------------------------------------
