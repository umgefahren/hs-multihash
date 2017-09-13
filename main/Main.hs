{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString.Lazy  (toStrict)
import           Data.Monoid           ((<>))
import           Options.Applicative

import           System.IO.Streams     (stdin, stdout, withFileAsInput, write)

import qualified Data.Multihash.Base   as MB
import qualified Data.Multihash.Digest as MH


data Termination = Null | Newline deriving (Show, Eq)
data Config =
    Config
    { cfFile       :: Maybe FilePath
    , cfAlgo       :: MH.HashAlgorithm
    , cfBase       :: MB.BaseEncoding
    , cfhashStream :: Maybe MH.Digest
    , cfTerm       :: Termination
    } deriving Show


main :: IO ()
main = do
    -- TODO add file checking
    config <- execParser opts
    digest <- maybe (hashStdin config) (hashFile config) $ cfFile config
    write (multihashStream config digest) stdout
  where
    hashStdin config = MH.hashStream (cfAlgo config) stdin
    hashFile config file = withFileAsInput file . MH.hashStream $ cfAlgo config
    multihashStream (Config _file algo base _hashStream term) =
        Just . toStrict . line term . MB.encode base . MH.encode algo

    line Null    = (<> "\0")
    line Newline = (<> "\n")


opts :: ParserInfo Config
opts = info
       (helper <*> (Config
                    <$> fileArg
                    <*> algoOpt
                    <*> baseOpt
                    <*> checkOpt
                    <*> nullTermFlag
                   ))
       (fullDesc
        <> header "Generate a multihashStream for the given input."
        <> progDesc "hashStream from FILE or stdin if not given.")


algoOpt :: Parser MH.HashAlgorithm
algoOpt =
    option auto
    $  long "algorithm"
    <> short 'a'
    <> metavar "ALGO"
    <> showDefault <> value MH.SHA256
    <> help ("hashStream algorithm to apply to input, ignored if checking hashStream " <> show ([minBound..] :: [MH.HashAlgorithm]))


baseOpt :: Parser MB.BaseEncoding
baseOpt =
    option auto
    $  long "encoding"
    <> short 'e'
    <> metavar "ENCODING"
    <> showDefault <> value MB.Base58
    <> help ("Base encoding of output digest, ignored if checking hashStream " <> show ([minBound..] :: [MB.BaseEncoding]))


checkOpt :: Parser (Maybe MH.Digest)
checkOpt =
    optional . option auto
    $  long "check"
    <> short 'c'
    <> metavar "DIGEST"
    <> help "Check for matching digest"


nullTermFlag :: Parser Termination
nullTermFlag =
    flag Newline Null
    $  long "print0"
    <> short '0'
    <> help "End filenames with NUL, for use with xargs"


fileArg :: Parser (Maybe FilePath)
fileArg = optional . argument str $ metavar "FILE"
