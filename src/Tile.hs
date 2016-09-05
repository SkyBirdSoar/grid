module Tile 
( module Unit

, TileSpec (..)
, chanceTS
, TileFactory
, mkTileFac

, LetterSpec (..)
, chanceLS
, LetterFactory
, mkLetterFac

, Tile
, tileToLetters
, getSize
, getLetter

, getTile
) where

import Unit
import           Generator (Generator)
import qualified Generator

import Data.List
import System.Random

newtype TileSpec = TileSpec ( Unit  -- Size
                            , Int   -- Spawn Chance (out of 100)
                            , Float -- Spawn Letter
                            ) deriving (Eq, Show)
                   
newtype LetterSpec = LetterSpec ( Char
                                , Int
                                ) deriving (Eq, Show)

{-# INLINE chanceTS #-}
chanceTS :: TileSpec -> Int
chanceTS (TileSpec (_,c,_)) = c

{-# INLINE chanceLS #-}
chanceLS :: LetterSpec -> Int
chanceLS (LetterSpec (_,c)) = c

--------------------------------------------------------------------------------

type TData = (Unit, Float)
newtype TileFactory = HundredTileFac (Generator TData) -- handle maximum of 100 different tiles
                      deriving (Eq)

mkTileFac :: [TileSpec] -> Either String TileFactory
mkTileFac ts
  | length ts <= 100 = if spChance == 100
                       then factory
                       else Left $ "Error: TileFactory: Total Spawn chance (" ++ show spChance ++ ") is not 100"
  where
    spChance = sum (map chanceTS ts)
    
    convertT :: TileSpec -> (Int, (Unit, Float))
    convertT (TileSpec (s,c,l)) = (c, (s, l))
    
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
                       else Left $ "Error: LetterFactory: Total Spawn chance (" ++ show spChance ++ ") is not 100"
  where
    spChance = sum (map chanceLS ls)
    
    convertL :: LetterSpec -> (Int, Char)
    convertL (LetterSpec (l, c)) = (c, l)
    
    factory = case Generator.mkGen (map convertL ls) of
                Left s    -> Left s
                Right gen -> Right $ HundredLetterFac gen
                
--------------------------------------------------------------------------------
    
data Tile = Tile Unit Char
            deriving (Eq, Show)
                   
tileToLetters :: Tile -> [String]
tileToLetters tile = replicate size (replicate size letter)
                     where
                       size = let (Unit x) = getSize tile in fromIntegral x
                       letter = getLetter tile

{-# INLINE getSize #-}
getSize :: Tile -> Unit
getSize (Tile s _) = s

{-# INLINE getLetter #-}
getLetter :: Tile -> Char
getLetter (Tile _ c) = c

--------------------------------------------------------------------------------

getTile :: Char -> TileFactory -> LetterFactory -> StdGen -> Tile
getTile defaultL (HundredTileFac gTData) (HundredLetterFac gLetter) stdgen
  = let (size, lc) = Generator.pick gTData stdgen
        (g, gg)    = split stdgen
    in  if   (fst $ random g) <= lc
        then Tile size (Generator.pick gLetter gg)
        else Tile size defaultL

--------------------------------------------------------------------------------
