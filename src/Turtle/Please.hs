{-# LANGUAGE PackageImports #-}

module Turtle.Please
    ( module Turtle
    , fold, foldIO
    ) where

import qualified "turtle" Turtle as Turtle.Naughty
import           "turtle" Turtle hiding (fold, foldIO)

fold :: (MonadIO io)=> Fold a b -> Shell a -> io b
fold = flip Turtle.Naughty.fold

foldIO :: (MonadIO io)=> FoldM IO a b -> Shell a -> io b
foldIO = flip Turtle.Naughty.foldIO
