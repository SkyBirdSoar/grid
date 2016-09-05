module Main where

import Canvas
import qualified Canvas

import System.Random
import System.Exit
import Data.List
import Options.Applicative

-------------
-------------
_MIN_SIZE = 1
_MAX_SIZE = 4

_TILE_SPECS = [ TileSpec ((Unit 5),  2, 0.5)
              , TileSpec ((Unit 4), 10, 0.2)
              , TileSpec ((Unit 3), 40, 0.6)
              , TileSpec ((Unit 1), 25,   1)
              , TileSpec ((Unit 2), 18,   1)
              , TileSpec ((Unit 1),  5, 0.0)
              ]
              
_LETTER_SPECS = [ LetterSpec ('█', 25)
                , LetterSpec ('▒', 55)
                , LetterSpec ('░', 19)
                , LetterSpec ('_',  1)
                ]
                
_DEFAULT_LETTER = ' '

main :: IO ()
main = do stdgen <- getStdGen
          execParser desc >>= 
            (\(height, width, seed) -> 
              case (fill _TILE_SPECS 
                         _LETTER_SPECS
                         _DEFAULT_LETTER 
                         (fromIntegral height)
                         (fromIntegral width)
                         (maybe stdgen (mkStdGen . fromIntegral) seed)
                   ) of
                 Left s -> putStrLn s
                 Right (Just c) -> putStrLn . show $ c
            )
       where
         desc = info (helper <*> opts)
                  (  fullDesc
                  <> progDesc "Generate an ASCII picture"
                  <> header "grid - generate an ASCII picture" )
                  
         opts :: Parser (Int, Int, Maybe Int)
         opts = (,,)
                <$> option auto
                (   long "height"
                <>  short 'H'
                <>  metavar "N"
                <>  help "Set the height N of the image generated as N"
                )
                <*> option auto
                (   long "width"
                <>  short 'W'
                <>  metavar "N"
                <>  help "Set the width of the image generated as N"
                )
                <*> (optional $ option auto
                (   long "seed"
                <>  short 'S'
                <>  metavar "N"
                <>  help "Set the seed N used to generate the image"
                ))