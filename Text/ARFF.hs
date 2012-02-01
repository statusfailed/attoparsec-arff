{-# LANGUAGE OverloadedStrings #-}
module ARFF where

import Prelude hiding (takeWhile)
import Control.Monad
import Control.Applicative ((<|>), (<*>), (<$>))
import Data.Map as Map
import Data.Set as Set

-- ByteStrings up in hurr
import Data.ByteString as BS hiding (takeWhile)
import Data.Char as Char

-- Attoparsec
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString as BS hiding (takeWhile)
import Data.Attoparsec.Char8 as C

data DataType = Numeric | Nominal (Set.Set ByteString) | String deriving(Show)

-- Represents header metadata
data Attribute = Attribute
  { name :: ByteString
  , dataType :: DataType
  } deriving (Show)

data Header = Header
  { relation :: ByteString -- Name of the relation (@RELATION foo)
  , attributes :: [Attribute] -- Mapping of indexes to values
  } deriving (Show)

before :: Parser p1 -> Parser p2 -> Parser p1
before p1 p2 = do
  p <- p1
  p2 >> return p
