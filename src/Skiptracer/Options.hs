module Skiptracer.Options (
    Options (..),
    getOpts
) where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data Options =
    Options
    { optFileName     :: String
    , optPreludeFile  :: Maybe String
    , optMaxTraces    :: Int
    , optHideFuncs    :: [String]
    , optSkipFuncs    :: [String]
    , optOnlyFuncs    :: Maybe [String]
    , optSkipPatMat   :: Bool
    , optSplitPrimOps :: Bool
    } deriving Show

-- | getOpts is similar to getArgs, but returns an Options
getOpts :: IO Options
getOpts =
    let parser   = optionsParser <**> helper
        progInfo = fullDesc <> header "skiptracer - A Step-Wise Tracer for Simple Haskell Programs"
    in execParser (info parser progInfo)

optionsParser :: Parser Options
optionsParser = Options
    <$> argument str (metavar "FILENAME" <> help "The file name to read from")
    <*> optional (option str (long "prelude" <> help "The file to load the prelude from" <> metavar "FILE"))
    <*> option auto (long "max-trace" <> help "Maximum number of traces to generate" <> showDefault <> value 1000 <> metavar "MAX")
    <*> many (strOption (long "hide" <> short 'h' <> help "Function applications to hide" <> metavar "FUNC"))
    <*> many (strOption (long "skip" <> short 's' <> help "Function applications to skip entirely" <> metavar "FUNC"))
    <*> optional (some (strOption (long "only" <> short 'o' <> help "Function applications to selectively show" <> metavar "FUNC")))
    <*> switch (long "skip-pattern" <> help "Skip pattern matching")
    <*> switch (long "split-prim-ops" <> help "Do not try to combine primitive operations")
