module Main where

import Canvas

import Options.Applicative
import System.Random
import System.Exit

-------------
-------------
_TILE_SPECS = [ TileSpec ((Unit 5),  2, 0.5)
              , TileSpec ((Unit 4), 10, 0.5)
              , TileSpec ((Unit 3), 40, 0.6)
              , TileSpec ((Unit 1), 25,   1)
              , TileSpec ((Unit 2), 18, 0.5)
              , TileSpec ((Unit 1),  5, 0.0)
              ]
              
_LETTER_SPECS = [ LetterSpec ('█', 25)
                , LetterSpec ('▓', 25)
                , LetterSpec ('▒', 25)
                , LetterSpec ('░', 25)
                ]
                
_DEFAULT_LETTER = ' '

run :: StdGen -> (Int, Int, Maybe Int) -> Either String Canvas
run stdgen (height, width, seed) = mkCanvas _TILE_SPECS 
                                            _LETTER_SPECS
                                            _DEFAULT_LETTER 
                                            (fromIntegral height)
                                            (fromIntegral width)
                                            (maybe stdgen (mkStdGen . fromIntegral) seed)

main :: IO ()
main = do stdgen <- getStdGen
          res <- (execParser desc >>= return . run stdgen)
          case res of
             Left s  -> putStrLn s
             Right c -> putStrLn . show $ c
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