{-# LANGUAGE TypeFamilies #-}

module Text.GitConfig.Parser where

import           Control.Applicative        (empty)
import           Control.Monad              (void)
import           Data.Functor               (($>))
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as M
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec            (MonadParsec, ParseError, Parsec,
                                             Token, between, many,
                                             sepBy, some, try, (<|>), eof, (<?>))
import           Text.Megaparsec.Char       (alphaNumChar, printChar, letterChar,
                                             space1, char, eol, satisfy)
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text
type GitConfigError = ParseError (Token Text) Void

-- FIXME I'm not satisfied with this type, it doesn't capture
--       a Git configuration's hierarchy properly. Rose tree could work,
--       but there may be a better data  structure for capturing this.
data Section = Section [Text] (HashMap Text Text)
  deriving (Show)

-- | Whitespace consumer for this parser.
spaceConsumer :: Parser ()
spaceConsumer = Lexer.space space1 lineComment empty
  where lineComment = Lexer.skipLineComment "#" <|> Lexer.skipLineComment ";"

-- | A lexeme for this parser. Consumes trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

-- | A symbol for this parser. Consumes trailing whitespace.
symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer

-- | Combinator returning the combinator between brackets.
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Combinator returning the combinator between quotes.
quotes :: Parser a -> Parser a
quotes = between (symbol "\"") (symbol "\"")

-- | Parser for the `.` character.
dot :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
dot = char '.'
{-# INLINE dot #-}

-- | Parser for the `-` character.
dash :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
dash = char '-'
{-# INLINE dash #-}

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
--
-- From the git 2.16 docs found here:
-- https://git-scm.com/docs/git-config/2.16.0#_syntax
sectionName :: Parser [Text]
sectionName = (section <|> subSection) `sepBy` spaceConsumer
  where
    sectionChar = alphaNumChar <|> dot <|> dash
    section     = fmap T.pack . some $ sectionChar
    subSection  = fmap T.pack . quotes . many $
      escSeq <|> satisfy (\x -> x `notElem` ['"', '\\'])

-- | Parse a section header.
--
-- A section begins with the name of the section in square brackets and
-- continues until the next section begins.
--
-- From the git 2.16 docs found here:
-- https://git-scm.com/docs/git-config/2.16.0#_syntax
sectionHeader :: Parser [Text]
sectionHeader = brackets sectionName

-- | Parse a variable name.
--
--  The variable names are case-insensitive, allow only alphanumeri
--  characters and -, and must start with an alphabetic character.
--
-- From the git 2.16 docs found here:
-- https://git-scm.com/docs/git-config/2.16.0#_syntax
variableName :: Parser Text
variableName = fmap (T.toLower . T.pack) . lexeme . try $ p
  where
    p = (:) <$> letterChar <*> many (alphaNumChar <|> dash)

-- | Parse a variable value.
variableValue :: Parser Text
variableValue = T.pack <$> between spaceConsumer eol (many printChar)

-- | Parse a mapping of 'Text' key to value.
--
--  in the form name = value (or just name, which is a short-hand to say that
--  the variable is the boolean "true").
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
  void (symbol "=")
  varValue <- variableValue
  return (varName, varValue)

-- | Parse a complete git config section.
section :: Parser Section
section = do
  header <- sectionHeader
  void spaceConsumer
  sectionValues <- mapping `sepBy` spaceConsumer
  return $ Section header (M.fromList sectionValues)

-- | Parse a complete git config.
config :: Parser [Section]
config = between spaceConsumer eof $ many section
