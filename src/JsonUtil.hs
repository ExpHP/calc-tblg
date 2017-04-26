
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module JsonUtil where

import           Prelude hiding (FilePath, interact)
import qualified Prelude
import           "base" Control.Arrow
import           "shake" Development.Shake
import qualified "text" Data.Text.IO as Text.IO
import           "lens" Control.Lens hiding ((<.>), strict)
import qualified "aeson" Data.Aeson as Aeson
import qualified "lens-aeson" Data.Aeson.Lens as Aeson.Lens
import qualified "yaml" Data.Yaml as Yaml
import qualified "bytestring" Data.ByteString.Lazy as LByteString
import qualified "bytestring" Data.ByteString as ByteString
import           "turtle-eggshell" Eggshell hiding (need)
import qualified "binary" Data.Binary as Binary
import qualified "zlib" Codec.Compression.GZip as GZip

readJsonEither :: (Aeson.FromJSON a)=> Prelude.FilePath -> IO (Either String a)
readJsonEither = LByteString.readFile >=> pure . Aeson.eitherDecode

readJson :: (Aeson.FromJSON a)=> Prelude.FilePath -> IO a
readJson = readJsonEither >=> either fail pure

writeJson :: (Aeson.ToJSON a)=> Prelude.FilePath -> a -> IO ()
writeJson p = Aeson.encode >>> LByteString.writeFile p

readYaml :: (Aeson.FromJSON a)=> Prelude.FilePath -> IO a
readYaml = ByteString.readFile >=> Yaml.decodeEither >>> either fail pure

writeYaml :: (Aeson.ToJSON a)=> Prelude.FilePath -> a -> IO ()
writeYaml = Yaml.encodeFile -- how nice of them!

readBinary :: (Binary.Binary a)=> Prelude.FilePath -> IO a
readBinary fp = Binary.decode . GZip.decompress <$> LByteString.readFile fp

writeBinary :: (Binary.Binary a)=> Prelude.FilePath -> a -> IO ()
writeBinary fp = LByteString.writeFile fp . GZip.compress . Binary.encode

-- Get a value from a nested object in a JSON file.
-- (NOTE: unable to traverse arrays)
getJson :: (MonadIO io)=> FilePath -> [Text] -> io (Maybe Aeson.Value)
getJson fpath jpath = liftIO $ getter <$> Text.IO.readFile (idgaf fpath) where
    -- since I still don't know much about lenses,
    -- let's try to at least sum up what's going on in here
    --  by printing out the types of various expressions:
    --
    -- λ> let s = "{\"a\": {\"b\": 3}}"
    -- s                                                    :: Text
    -- s ^? _Value                                          :: Maybe Value
    --
    -- -- If we want to shortcircuit the case where an index doesn't exist...
    -- s ^? _Value . key "a"                                :: Maybe Value
    --
    -- -- If we want writing to be able to create new paths...
    -- s ^? _Value . _Object                                :: Maybe (HashMap Text Value)
    -- s ^? _Value . _Object . at "a"                       :: Maybe (Maybe Value)
    -- s ^? _Value . _Object . at "a" . non (Object mempty) :: Maybe Value
    --
    -- -- To write a (possibly new) Value, find an expression that produces
    -- --  'Maybe (Maybe Value)' and change up the operators a bit:
    -- λ> s & _Value . _Object . at "c" ?~ Number 6
    -- "{\"a\":{\"b\":3},\"c\":6}"
    getter t = t ^? lens
    lens = foldl (.) (Aeson.Lens._Value) (fmap Aeson.Lens.key jpath)

-- Set a path in a JSON file, creating new objects as necessary.
-- (NOTE: unable to traverse arrays)
setJson :: (MonadIO io)=> FilePath -> [Text] -> Aeson.Value -> io ()
setJson fpath []          value = editFile fpath (const $ idgaf $ Aeson.encode value)
setJson fpath (key0:keys) value = editFile fpath (lens ?~ value) where
    atKey key = Aeson.Lens._Object . at key
    lens = foldl (.)
        (Aeson.Lens._Value . atKey key0)
        -- FIXME aaaggh stupid incomprehensible double dots, what is this?!
        --       damn both my ability to mechanically produce such expressions,
        --       as well as my inability to read them afterwards
        ((non (Aeson.Object mempty) .) . atKey <$> keys)
