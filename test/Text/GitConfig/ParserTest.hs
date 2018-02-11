module Text.GitConfig.ParserTest where

import           Data.Monoid                      ((<>))
import           Data.Text                        (Text)
import           Test.SmallCheck.Series.Instances ()
import           Test.Tasty                       (TestTree)
import           Test.Tasty.SmallCheck            (testProperty)
import qualified Text.GitConfig.Parser            as P
import           Text.Megaparsec                  (parse)

test_symbol :: TestTree
test_symbol = testProperty
  "symbol should parse the given text and consume trailing whitespace" $
    \t ->
      case parse (P.symbol (t :: Text)) "" (t <> "  ") of
        Left _ -> False
        Right t' -> t == t'
