{-# LANGUAGE OverloadedStrings #-}
module ARFF where

import Prelude hiding (takeWhile)
import Control.Monad
import Control.Applicative
import Data.Map as Map
import Data.Set as Set

-- ByteStrings up in hurr
import qualified Data.ByteString as BS
import Data.Char as Char

-- Attoparsec
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.Text as Text

data AttributeType = Numeric | Nominal (Set.Set BS.ByteString) | String deriving(Show)

-- Represents header metadata
data Attribute = Attribute
  { name :: BS.ByteString
  , dataType :: AttributeType
  } deriving (Show)

data Header = Header
  { title :: BS.ByteString -- Name of the relation (@RELATION foo)
  , attributes :: [Attribute] -- Mapping of indexes to values
  } deriving (Show)

-- | Parse two expressions sequentially, returning the result of the first.
before :: Parser p1 -> Parser p2 -> Parser p1
before p1 p2 = do
  p <- p1
  p2 >> return p

-- | matches non-newline space characters
isInlineSpace :: Char -> Bool
isInlineSpace c = Char.isSpace c && c /= '\n' && c /= '\r'

-- | Parses a sequence of non-newline space characters
lineSpace :: Parser ()
lineSpace = skipWhile isInlineSpace

-- | Arguments to '@' directives, e.g. @RELATION foo. 
-- TODO: Check these rules against the spec!
identifier :: Parser BS.ByteString
identifier = takeWhile Char.isAlphaNum

-- Matches ignored data, e.g. comments up to end of line
comment :: Parser ()
comment = char '%' >> skipWhile (not . Text.isEndOfLine)

-- | Matches what should be the end of the line- optional comment then newline.
lineEnd :: Parser()
lineEnd = lineSpace >> (comment >> endOfLine) <|> endOfLine

relation :: Parser BS.ByteString
relation = char '@' >> stringCI "relation" >> lineSpace >>
           identifier `before` lineEnd

-- | Parse the attribute type: @ATTRIBUTE <name> <type>
attributeType :: Parser AttributeType
attributeType = (stringCI "numeric" >> return Numeric)
                <|> (stringCI "real" >> return Numeric)
                <|> (nominal >>= return . Nominal . Set.fromList)
  where nominal = do
          char '{'
          xs <- (takeWhile $ (not . isSep)) `sepBy` (char ',')
          char '}'
          return xs
        isSep c = c == '{' || c == ',' || c == '}' -- Advised not to use InClass

-- | Parse an attribute: @ATTRIBUTE <Name> <Type>
attribute :: Parser Attribute
attribute = do char '@' >> stringCI "attribute" >> lineSpace
               i <- identifier `before` lineSpace
               t <- attributeType `before` lineEnd
               return $ Attribute i t
