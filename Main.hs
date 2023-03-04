module Main where

import Options.Applicative

import qualified Codec.Archive.Mpq as Mpq

import qualified Data.ByteString.Lazy as BL

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (liftIO, MonadIO)

import System.FilePath
import Data.Maybe


data Action = AddAction FilePath [(FilePath, Maybe FilePath)]
    deriving (Show)

parseOptions = customExecParser (prefs showHelpOnEmpty) opts
  where
    opts = info (pCommand <**> helper)
            ( fullDesc
            <> header ("mpq - Small mpq utility") )

    pCommand = hsubparser
        ( command "add" (info addOptions (progDesc "Adds a file to the mpq archive")))


    addOptions =
        AddAction <$> pMpq
                  <*> some pFileOptionalName

    pMpq = strArgument
      ( metavar "MPQ"
      <> help "Path to the mpq archive"
      )

    pFileOptionalName =
        (,) <$> pFile
            <*> optional pNameArg

    pFile = strArgument
        (  metavar "FILE"
        <> help "File to add to the archive"
        )

    pNameArg = strOption
        ( long "name"
        <> metavar "NAME"
        <> help "Add FILE under this NAME to the archive"
        )

main = do
    options <- parseOptions
    case options of
        AddAction{} -> addFile options

addFile (AddAction mpq filesToAdd) = runMaybeT $ do
    archive <- MaybeT $ Mpq.open mpq
    forM_ filesToAdd $ \(fp, optionalName) -> do
        let internalName = head $ catMaybes [ optionalName, Just $ takeFileName fp ]
        bs <- liftIO $ BL.readFile fp
        liftIO $ Mpq.addFile archive internalName bs
    liftIO $ Mpq.close archive
