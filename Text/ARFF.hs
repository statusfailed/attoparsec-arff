{-# LANGUAGE OverloadedStrings #-}
module ARFF where
{-
  ( identifier
  , comment
  , relation
  , attributeType
  , attribute
  , header
  ) where
-}

-- TODO:
-- * Fix haddock comments!
-- * Re-do exports.

import Prelude hiding (takeWhile)
import Control.Monad
import Control.Applicative
import Data.Map as Map
import Data.Set as Set
import Data.Maybe

-- ByteStrings up in hurr
import qualified Data.ByteString as BS
import Data.Char as Char

-- Attoparsec
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.Text as Text

data AttributeType = Numeric | Nominal [BS.ByteString] | String deriving(Show)

-- Represents metadata for a single attribute
data Attribute = Attribute
  { name :: BS.ByteString
  , dataType :: AttributeType
  } deriving (Show)

-- | Represents the entire header
data Header = Header
  { title :: BS.ByteString
  -- ^ Name of the relation (\@RELATION foo)
  , attributes :: [Attribute]
  -- ^ Mapping of indexes to values
  } deriving (Show)

-- | Value of a single attribute in a single row
data AttributeValue = NumericValue (Maybe Double) |
                 NominalValue (Maybe BS.ByteString) |
                 StringValue (Maybe String)

showAttributeValue :: AttributeValue -> String
showAttributeValue (NumericValue x) = show x
showAttributeValue (NominalValue x) = show x
showAttributeValue (StringValue x) = show x

instance Show AttributeValue where
  show = showAttributeValue

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

-- Matches ignored data, e.g. comments up to end of line
comment :: Parser ()
comment = char '%' >> skipWhile (not . Text.isEndOfLine)

-- | Matches what should be the end of the line- optional comment then newline.
lineEnd :: Parser()
lineEnd = lineSpace >> (comment >> endOfLine) <|> endOfLine

-- | @identifier@ parses arguments to '\@' directives, e.g. "\@RELATION foo" 
-- TODO: Check these rules against the spec!
-- TODO: Allow quoted identifiers with spaces inside.
identifier :: Parser BS.ByteString
identifier = takeWhile (\x -> Char.isAlphaNum x || x == '-')

-- | Parse the title of the relation
relation :: Parser BS.ByteString
relation = char '@' >> stringCI "relation" >> lineSpace >> identifier

-- | Parse the attribute type: \@ATTRIBUTE <name> <type>
-- TODO: Fix parsing of Nominal attribute names.
attributeType :: Parser AttributeType
attributeType = (stringCI "numeric" >> return Numeric)
                <|> (stringCI "real" >> return Numeric)
                <|> (nominal >>= return . Nominal)
                <?> "Attribute Type"
  where nominal = do
          char '{' >> lineSpace
          xs <- identifier `sepBy` (lineSpace >> char ',' >> lineSpace)
          lineSpace >> char '}'
          return xs

-- | Parse an attribute: \@ATTRIBUTE <Name> <Type>
attribute :: Parser Attribute
attribute = do char '@' >> stringCI "attribute" >> lineSpace
               i <- identifier `before` lineSpace
               t <- attributeType
               return $ Attribute i t
            <?> "Attribute"

-- | Parses the next expected line
line :: Parser p -> Parser p
line p' = skipMany lineEnd >> lineSpace >> p' `before` lineEnd

-- | Parse an ARFF header.
header :: Parser Header
header = do
  t <- line relation
  as <- manyTill (line attribute) (line $ stringCI "@data")
  return $ Header t as

-- | Parse a value of the expected type
--value :: AttributeType -> Parser AttributeValue
--value (Nominal xs) = choice $ Prelude.map string (Set.toList xs)
--foo = ["a", "b", "c"] :: [BS.ByteString]
--bar = Prelude.map string foo
value (Nominal xs) = ((choice $ Prelude.map string xs) >>= return . NominalValue . Just)
                     <|> (char '?' >> return (NominalValue Nothing))
                     <?> "Expected one of " ++ (concat $ Prelude.map show xs)
--                     <|>  return  . NominalValue Nothing

-- | Create a parser which parses a single row of AttributeValues, expecting
-- each to be in order of the Attributes supplied.
--row :: [Attribute] ->
--       Parser [AttributeValue]
--row = foldl (>>) $ map valueParser [Attribute]
