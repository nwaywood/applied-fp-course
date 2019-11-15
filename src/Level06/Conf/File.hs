{-# LANGUAGE OverloadedStrings #-}
module Level06.Conf.File where

import           Control.Monad.IO.Class     (liftIO)
import           Data.ByteString            (ByteString, pack)

import           Data.Text                  (Text)

import           Data.Bifunctor             (bimap, first)
import           Data.Monoid                (Last (Last))

import           Control.Exception          (try)
-- import Data.ByteString.Lazy as BLU -- from utf8-string
import qualified Data.Attoparsec.ByteString as AB

import           Waargonaut                 (Json)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError (ParseFailed))

import           Level06.AppM               (AppM (runAppM))
import           Level06.Types              (ConfigError (BadConfFile),
                                             PartialConf (PartialConf))
-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to write a
-- 'waargonaut' 'Decoder' to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> runAppM $ readConfFile "badFileName.no"
-- Left (<YourErrorConstructorHere> "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> runAppM $ readConfFile "files/test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readConfFile
  :: FilePath
  -> AppM ConfigError ByteString
readConfFile fp = error "blah"
  -- Reading a file may throw an exception for any number of
  -- reasons. Use the 'try' function from 'Control.Exception' to catch
  -- the exception and turn it into an error value that is thrown as
  -- part of our 'AppM' transformer.
  --
  -- No exceptions from reading the file should escape this function.
  --
  -- let test = try (readFile fp) in _ <$> test -- liftIO $ bimap BadConfFile pack test

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> AppM ConfigError PartialConf
parseJSONConfigFile =
  error "parseJSONConfigFile not implemented"

-- Go to 'src/Level06/Conf.hs' next.
