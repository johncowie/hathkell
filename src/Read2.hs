module Read2
( readAST )
where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import Util (firstRight, applyFunctions)
import Text.Read (readMaybe)
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.Parsec.Token as T
import qualified Numeric as N

import Core ( Symbol
            , listExpression
            , intLiteral
            , boolLiteral
            , stringLiteral
            , symbolName
            )

type ParseError = String

--- reader stuff

whitespace :: Char -> Bool
whitespace ' ' = True
whitespace '\n' = True
whitespace '\t' = True
whitespace _ = False

replace :: Char -> String -> String -> String
replace _ _ [] = []
replace o replacement (c:cs)
  | o == c = replacement ++ (replace o replacement cs)
  | otherwise = c:(replace o replacement cs)

splitOn :: (Char -> Bool) -> String -> [String]
splitOn _ [] = []
splitOn p xs =
  case firstWord of
    "" -> []
    _  -> firstWord:splitOn p rest
  where (_,trimmed) = break (not . p) xs
        (firstWord,rest) = break p trimmed

tokens = (splitOn whitespace) .
         (replace ')' " ) ") .
         (replace '(' " ( ")

readNext :: [Symbol] -> [String] -> Either ParseError ([Symbol], [String])
readNext csyms [] = Right (csyms, [])
readNext csyms ("(":xs) = do
  result <- (readNext [] xs)
  let (syms, strings) = result
  readNext (csyms ++ [listExpression syms]) strings
readNext syms (")":xs) = Right (syms, xs)
readNext csyms (token:xs) = do
  symbol <- readValue token
  readNext (csyms ++ [symbol]) xs

-- TODO check for empty
-- TODO check parentheses balance

readASTOld :: String -> Either ParseError [Symbol]
readASTOld = (fmap fst) . (readNext []) . tokens

readValue ::  String -> Either ParseError Symbol
readValue s = firstRight ("Can't parse token " ++ s) .
                 applyFunctions [readIntLiteral,
                                 readBoolLiteral,
                                 readSymbolName] $ s

readEither :: (Read a) => String -> String -> Either ParseError a
readEither typeName s = maybe (Left errorMessage) Right (readMaybe s)
 where errorMessage = "can't parse " ++ s ++ " to type " ++ typeName

readIntLiteral :: String -> Either ParseError Symbol
readIntLiteral = (fmap intLiteral) . (readEither "Integer")

readBoolLiteral :: String -> Either ParseError Symbol
readBoolLiteral "true" = Right (boolLiteral True)
readBoolLiteral "false" = Right (boolLiteral False)
readBoolLiteral s = Left ("can't parse " ++ s ++ " to type Bool")

readSymbolName :: String -> Either ParseError Symbol
readSymbolName = Right . symbolName


--- parsec


readAST :: String -> Either P.ParseError [Symbol]
readAST = P.parse parseSymbols "error"

parseSymbols :: P.CharParser () [Symbol]
parseSymbols = (P.many parseSymbol) <* P.eof

parseSymbol :: P.CharParser () Symbol
parseSymbol = value <* P.spaces
  where value = parseBool
                P.<|> parseList
                P.<|> parseString
                P.<|> parseInt
                P.<|> parseSymbolName

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
parseBool = boolLiteral <$> (        True  <$ (P.string "true")
                               P.<|> False <$ (P.string "false"))

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
