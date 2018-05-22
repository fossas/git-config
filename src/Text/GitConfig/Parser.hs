{-|
Module      : Text.GitConfig.Parser
Description : Parser-combinators for Git configuration files.
Copyright   : (c) Fernando Freire, 2018
License     : BSD3
Maintainer  : dogonthehorizon@gmail.com
Stability   : experimental

This module provides parser-combinators for Git configuration files.

It attempts to follow the syntax for Git configuration files outlined in
this document:

https://git-scm.com/docs/git-config/2.16.0#_syntax

One notable omission is that no legacy compatability is explicitly provided
(e.g. dot-separated subsection names are not guaranteed to parse correctly).
-}

{-# LANGUAGE TypeFamilies #-}

module Text.GitConfig.Parser (
  -- * Types
  Parser,
  Section(..),
  GitConfig,
  -- * Lexer
  spaceConsumer,
  lexeme,
  symbol,
  brackets,
  quotes,
  escSeq,
  -- * Parser
  sectionName,
  sectionHeader,
  variableName,
  variableValue,
  mapping,
  section,
  config,
  parseConfig
) where

import           Control.Applicative        (empty)
import           Control.Monad              (void)
import           Data.Functor               (($>))
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as M
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec            (ParseError, Parsec, Token, between,
                                             eof, many, parse, sepBy, some,
                                             (<?>), (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, eol,
                                             letterChar, printChar, satisfy,
                                             space1)
import qualified Text.Megaparsec.Char.Lexer as Lexer

type GitConfigError = ParseError (Token Text) Void
type Parser = Parsec Void Text

data Section = Section [Text] (HashMap Text Text)
  deriving (Eq, Show)

type GitConfig = [Section]

-- | Whitespace consumer for this parser.
--
-- Whitespace is considered to be any space character
-- (including carriage return) as well as line comments starting with `#` and
-- `;` .
spaceConsumer :: Parser ()
spaceConsumer = Lexer.space space1 lineComment empty
  where lineComment = Lexer.skipLineComment "#" <|> Lexer.skipLineComment ";"

-- | A lexeme for this parser.
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

-- | A symbol for this parser.
symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer

-- | Return a parser in between brackets.
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Return a parser in between quotes.
quotes :: Parser a -> Parser a
quotes = between (symbol "\"") (symbol "\"")

-- | Parser for escape sequences.
escSeq :: Parser Char
escSeq = char '\\' *> escSeqChar
  where
    escSeqChar =  char '"'
              <|> char '\\'
              <|> char '/'
              <|> (char 'n' $> '\n')
              <|> (char 't' $> '\t')
              <|> (char 'r' $> '\r')
              <|> (char 'b' $> '\b')
              <|> (char 'f' $> '\f')
              <?> "escaped character"

-- | Parse a section name.
--
-- Section names are case-insensitive. Only alphanumeric
-- characters, - and . are allowed in section names. Sections can be further
-- divided into subsections. To begin a subsection put its name in double
-- quotes, separated by space from the section name, in the section header.
sectionName :: Parser [Text]
sectionName = (section <|> subSection) `sepBy` spaceConsumer
  where
    sectionChar = alphaNumChar <|> char '.' <|> char '-'
    section     = fmap T.pack . some $ sectionChar
    subSection  = fmap T.pack . quotes . many $
      escSeq <|> satisfy (\x -> x `notElem` ['"', '\\'])

-- | Parse a section header.
--
-- A section begins with the name of the section in square brackets and
-- continues until the next section begins.
sectionHeader :: Parser [Text]
sectionHeader = brackets sectionName

-- | Parse a variable name.
--
--  The variable names are case-insensitive, allow only alphanumeric
--  characters and -, and must start with an alphabetic character.
variableName :: Parser Text
variableName = fmap (T.toLower . T.pack) . lexeme $ p
  where
    p = (:) <$> letterChar <*> many (alphaNumChar <|> char '-')

-- | Parse a variable value.
variableValue :: Parser Text
variableValue = T.pack <$> between spaceConsumer eol (many printChar)

-- | Parse a tuple of 'Text' key and value.
--
--  in the form name = value (or just name, which is a short-hand to say that
--  the variable is the boolean "true"). -- FIXME parse boolean true
--
--  A line that defines a value can be continued to the next line by ending
--  it with a \; the backquote and the end-of-line are stripped. Leading
--  whitespaces after name =, the remainder of the line after the first
--  comment character # or ;, and trailing whitespaces of the line are
--  discarded unless they are enclosed in double quotes. Internal whitespaces
--  within the value are retained verbatim.
mapping :: Parser (Text, Text)
mapping = do
  varName <- variableName
  varValue <- do void (symbol "=")
                 variableValue <|> pure ""
              <|> pure "true"
  return (varName, varValue)

-- | Parse a complete git config section.
section :: Parser Section
section = do
  header <- sectionHeader
  void spaceConsumer
  sectionValues <- mapping `sepBy` spaceConsumer
  return $ Section header (M.fromList sectionValues)

-- | Parse a complete git config.
config :: Parser GitConfig
config = between spaceConsumer eof $ many section

parseConfig :: Text -> Either GitConfigError GitConfig
parseConfig = parse config "noSrc"
