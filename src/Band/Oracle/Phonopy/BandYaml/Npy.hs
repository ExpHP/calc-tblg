{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wall #-}

module Band.Oracle.Phonopy.BandYaml.Npy where

import           "exphp-prelude" ExpHPrelude hiding (sequence, sequence_)
import           "base" Data.Complex
import           "base" Debug.Trace
import           "base" Control.Exception
import           "binary" Data.Binary.Get
import qualified "bytestring" Data.ByteString.Lazy as LByteString
import qualified "data-binary-ieee754" Data.Binary.IEEE754 as Binary
import qualified "attoparsec" Data.Attoparsec.ByteString.Char8 as C8
import           "attoparsec" Data.Attoparsec.ByteString
import           "attoparsec-binary" Data.Attoparsec.Binary
import qualified "bytestring" Data.ByteString.Char8 as ByteString
import qualified "vector" Data.Vector as Vector
import qualified "vector" Data.Vector.Unboxed as UVector

infixl 4 <<
(<<) :: Applicative m => m a -> m b -> m a
(<<) = (<*)

data Npy = Npy
    { npyVersion :: Version
    , npyHeader :: Header
    , npyBlob :: ByteString
    }

data Version = Version
    { versionMajor :: Int
    , versionMinor :: Int
    } deriving (Eq, Show, Read, Ord)

data Header = Header
    { headerGet :: Get (Complex Double)
    , headerElemSize :: Int
    , headerDims :: [Int]
    }

readKetsFile :: FilePath -> IO (Vector (Vector (UVector (Complex Double))))
readKetsFile fp = do
    bs <- ByteString.readFile fp
    Npy{..} <- either fail pure $ parseOnly npy bs
    pure $ runGet (getKets npyHeader) (LByteString.fromStrict npyBlob)


getKets :: Header -> Get (Vector (Vector (UVector (Complex Double))))
getKets Header{..} = do
    let [nk, 1, na3, nb] = headerDims
    (assert $ nb == na3) pure () -- sanity check
    Vector.replicateM nk $
        -- unbox inner
        fmap (fmap Vector.convert) $
        -- put band first, then atom
        fmap transposeV $
        Vector.replicateM na3 $
            Vector.replicateM nb $
                headerGet

transposeV :: Vector (Vector a) -> Vector (Vector a)
transposeV = Vector.fromList . fmap Vector.fromList . transpose . fmap toList . toList

npy :: Parser Npy
npy = do
    magic
    theVersion <- version
    theSize <- headerSize theVersion
    theHeaderText <- C8.take theSize
    theHeader@Header{..} <- either fail pure $ parseOnly header theHeaderText
    let blobSize = headerElemSize * product headerDims
    theBlob <- C8.take blobSize
    endOfInput
    pure $ Npy { npyVersion = theVersion
               , npyHeader = theHeader
               , npyBlob = theBlob
               }

magic :: Parser ()
magic = do
    () <$ word8 0x93
    () <$ string "NUMPY"

version :: Parser Version
version = Version <$> (fromIntegral <$> anyWord8)
                  <*> (fromIntegral <$> anyWord8)


headerSize :: Version -> Parser Int
headerSize (Version 1 0) = fromIntegral <$> anyWord16le
headerSize (Version 2 0) = fromIntegral <$> anyWord32le
headerSize v = fail $ "unrecognized format version: " ++ show v


-- must be used on just the header string; not the full file.
header :: Parser Header
header = do
    -- Make no mistake; this file makes no attempt to actually implement the spec,
    --   which contains such phrases as "a [python] object that can be passed
    --   as an argument to the numpy.dtype() constructor".
    -- The only aim of this parsing code is to to catch errors when the conditions
    --   diverge from our assumptions.
    _ <- C8.string "{'descr': '<c16', 'fortran_order': False, 'shape': "

    _ <- C8.char '('
    dims <- (C8.decimal) `sepBy` (C8.string ", ")
    skipWhile (inClass ",)} \t\r\n")
    endOfInput

    pure $ Header { -- note: <c16 and >c16 both put the real part first
                    headerGet = (:+) <$> Binary.getFloat64le
                                     <*> Binary.getFloat64le
                  , headerDims = dims
                  , headerElemSize = 16
                  }
