{-# LANGUAGE OverloadedStrings #-}

module Main where

import Canvas

import           Control.Applicative
import qualified Data.ByteString.Lazy as B
import           Data.Aeson
import           Options.Applicative
import           System.Random
import           System.Exit

-------------
-------------

data Config = Config [TileSpec] [LetterSpec] Char

instance FromJSON Config where
  parseJSON (Object v) =   Config
                       <$> v .: "tiles"
                       <*> v .: "letters"
                       <*> v .: "defaultLetter"
  parseJSON _          = empty

run :: [TileSpec] -> [LetterSpec] -> Char -> Int -> Int -> StdGen -> Either String Canvas
run = mkCanvas

main :: IO ()
main = do stdgen <- getStdGen
          (height, width, seed, configLoc) <- execParser desc
          config <- B.readFile configLoc
          case decode config of
            Nothing -> do Prelude.putStrLn "Configuration file is invalid."
                          exitFailure
            Just (Config ts ls dL) -> let res = run ts ls dL height width (maybe stdgen (mkStdGen . fromIntegral) seed)
                                      in case res of
                                           Left s  -> do Prelude.putStrLn ("Configuration File is invalid: " ++ s)
                                                         exitFailure
                                           Right c -> Prelude.putStrLn . show $ c
                   
       where
         desc = info (helper <*> opts)
                  (  fullDesc
                  <> progDesc "Generate an ASCII picture"
                  <> header "grid - generate an ASCII picture" )
                  
         opts :: Parser (Int, Int, Maybe Int, String)
         opts = (,,,)
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
                <*> strOption
                (   long "config"
                <>  short 'C'
                <>  value "./grid_config.json"
                <>  metavar "FILE"
                <>  help "Read configuration data from FILE"
                )