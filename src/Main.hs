{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Codec.Archive.Mpq qualified as Mpq
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe
import Data.ByteString.Builder (hPutBuilder)
import Data.ByteString.Lazy qualified as BL
import Data.Maybe
import Options.Applicative
import System.FilePath
import System.IO (BufferMode (BlockBuffering), hSetBinaryMode, hSetBuffering, stdout)

data Action
  = AddAction FilePath [(FilePath, Maybe FilePath)]
  | CatAction FilePath FilePath
  deriving (Show)

parseOptions :: IO Action
parseOptions = customExecParser (prefs showHelpOnEmpty) opts
  where
    opts =
      info
        (pCommand <**> helper)
        ( fullDesc
            <> header "mpq - Small mpq utility"
        )

    pCommand =
      hsubparser
        ( command "add" (info addOptions (progDesc "Adds a file to the mpq archive"))
            <> command "cat" (info catOptions (progDesc "Extracts a file from the mpq archive and writes it to stdout"))
        )

    addOptions =
      AddAction
        <$> pMpq
        <*> some pFileOptionalName

    catOptions = CatAction <$> pMpq <*> strArgument (metavar "PATH" <> help "Filepath to read from the mpq archive")

    pMpq =
      strArgument
        ( metavar "MPQ"
            <> help "Path to the mpq archive"
        )

    pFileOptionalName =
      (,)
        <$> pFile
        <*> optional pNameArg

    pFile =
      strArgument
        ( metavar "FILE"
            <> help "File to add to the archive"
        )

    pNameArg =
      strOption
        ( long "name"
            <> metavar "NAME"
            <> help "Add FILE under this NAME to the archive"
        )

main :: IO ()
main = do
  options <- parseOptions
  void $ case options of
    AddAction mpq filesToAdd -> addFile mpq filesToAdd
    CatAction mpq path -> catFile mpq path
  pure ()

addFile :: (MonadIO m, Foldable t) => FilePath -> t (FilePath, Maybe FilePath) -> m (Maybe ())
addFile mpq filesToAdd = runMaybeT $ do
  archive <- MaybeT $ Mpq.open mpq
  forM_ filesToAdd $ \(fp, optionalName) -> do
    let internalName = head $ catMaybes [optionalName, Just $ takeFileName fp]
    bs <- liftIO $ BL.readFile fp
    liftIO $ Mpq.addFile archive internalName bs
  liftIO $ Mpq.close archive

catFile :: (MonadIO m) => FilePath -> FilePath -> m (Maybe ())
catFile mpq path = runMaybeT $ do
  archive <- MaybeT $ Mpq.open mpq
  builder <- MaybeT $ Mpq.readFile archive path
  liftIO $ do
    hSetBinaryMode stdout True
    hSetBuffering stdout (BlockBuffering Nothing)
    hPutBuilder stdout builder
    Mpq.close archive
