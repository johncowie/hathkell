module Read2
( readAST )
where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import Text.Read (readMaybe)
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.Parsec.Token as T
import qualified Numeric as N

import Core ( Symbol
            , listExpression
            , intLiteral
            , boolLiteral
            , stringLiteral
            , symbolName)

readAST :: String -> Either P.ParseError [Symbol]
readAST = P.parse parseSymbols "error"

parseSymbols :: P.CharParser () [Symbol]
parseSymbols = (P.many parseSymbol) <* P.eof

parseSymbol :: P.CharParser () Symbol
parseSymbol = value <* P.spaces
  where value = P.choice [ parseBool
                         , parseList
                         , parseString
                         , parseInt
                         , parseSymbolName]
                         P.<?> "Doh"

parseSeries :: Char -> P.CharParser () a -> Char -> P.CharParser () [a]
parseSeries left parser right =
  P.between (P.char left <* P.spaces) (P.char right) $
          (parser <* P.spaces) `P.sepBy` P.spaces

parseList :: P.CharParser () Symbol
parseList = listExpression <$> (P.spaces *> (parseSeries '(' parseSymbol ')') <* P.spaces)

parseString :: P.CharParser () Symbol
parseString = stringLiteral <$> (P.between (P.char '\"') (P.char '\"') (P.many schar))
  where schar = P.satisfy (`notElem` "\"")

parseInt :: P.CharParser () Symbol
parseInt = (intLiteral . read) <$> (P.many1 (P.oneOf "0123456789"))

parseSymbolName :: P.CharParser () Symbol
parseSymbolName = symbolName <$> (P.many1 symbolChar)
  where symbolChar = P.satisfy isSymbolChar
        isSymbolChar c = (not (Char.isSpace c)) && (notElem c "\"()")

parseBool :: P.CharParser () Symbol
parseBool = P.try (boolLiteral <$> (        True  <$ (P.string "true")
                               P.<|> False <$ (P.string "false")))

-- parseString :: P.CharParser () Symbol
-- parseString = stringLiteral <$> (P.between (P.char '"') (P.char '"') (P.many jchar))
--   where jchar = P.char '\\' *> (p_escape P.<|> p_unicode)
--                 P.<|> P.satisfy (`notElem` "\"\\")
--
-- p_escape :: P.CharParser () Char
-- p_escape = P.choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
--     where decode c r = r <$ P.char c
--
-- p_unicode :: P.CharParser () Char
-- p_unicode = P.char 'u' *> (decode <$> P.count 4 P.hexDigit)
--     where decode x = toEnum code
--               where ((code,_):_) = N.readHex x
