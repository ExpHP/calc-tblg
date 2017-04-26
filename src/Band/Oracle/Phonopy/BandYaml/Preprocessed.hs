{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wall #-}

module Band.Oracle.Phonopy.BandYaml.Preprocessed where

import           "exphp-prelude" ExpHPrelude hiding (sequence, sequence_)
import           "base" Data.Complex
import           "attoparsec" Data.Attoparsec.ByteString.Char8
import qualified "bytestring" Data.ByteString.Char8 as ByteString
import qualified "vector" Data.Vector as Vector
import qualified "vector" Data.Vector.Unboxed as UVector

infixl 4 <<
(<<) :: Applicative m => m a -> m b -> m a
(<<) = (<*)

readKetsFile :: FilePath -> IO (Vector (UVector (Complex Double)))
readKetsFile fp =
    ByteString.readFile fp
        <&> parseOnly (skipSpace >> kets << skipSpace << endOfInput)
        >>= either fail pure

kets :: Parser (Vector (UVector (Complex Double)))
kets = p <?> "kets" where
    p = fmap Vector.fromList $
        ket `sepBy1'` skipSpace1

ket :: Parser (UVector (Complex Double))
ket = p <?> "ket" where
    p = fmap UVector.fromList $
        skipKetHeader >> skipSpace1 >> (ketElement `sepBy1'` skipSpace1)

skipKetHeader :: Parser ()
skipKetHeader = p <?> "skipKetHeader" where
    p = do
        char '-' >> skipSpace1
        char '#' >> skipSpace1
        skipDecimal

ketElement :: Parser (Complex Double)
ketElement = p <?> "ketElement" where
    p = do
        char '-' >> skipSpace1
        re <- double << skipSpace1
        im <- double
        pure $! (re :+ im)

skipDecimal :: Parser ()
skipDecimal = p <?> "skipDecimal" where p = skipWhile1 isDigit

skipWhile1 :: (Char -> Bool) -> Parser ()
skipWhile1 p = satisfy p >> skipWhile p

skipSpace1 :: Parser ()
skipSpace1 = satisfy isSpace >> skipSpace
