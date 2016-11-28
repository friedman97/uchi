module Lab6 (
  Name,
  Number,
  TopLevelExp(..),
  MathExp(..),
  parse
) where

import Control.Applicative hiding (many)
import Control.Monad
import Data.Char
import Data.List
import Data.Ord
import Text.ParserCombinators.ReadP

type Name   = String  -- Variable names are strings.
type Number = Int     -- The kind of number in our language.


-- A top-level expression is either:
--
-- 1) A bare mathematical expression:
--
-- 4 + (2*5)
--
-- 2) A let-binding followed by an expression to evaluate:
--
-- let x = 5 in x + 4
--
-- let (x1, y1, x2, y2) = (5,5,10,10) in (y2-y1)*(y2-y1) + (x2-x1)*(x2-x1)
--
-- You can assume that the tuples on either side of the = sign are
-- always the same length--if not it will be treated as an eval error.

data TopLevelExp
  = MathTLE MathExp
  | LetTLE [Name] [MathExp] MathExp
  deriving (Eq, Show)


-- A math expression is a number, a variable,
-- a negation of a math expression, or any of
-- the four major operations plus power on a
-- pair of math expressions.
--
-- In the actual parser:
--   1. Precedence should be standard order of operations.
--   2. Negation should only precede a number, a variable, or
--      a parenthetical expression.
--   3. A variable starts with a lowercaddsube letaddsubr and afaddsubr
--      the first letaddsubr is any alphanumeric characaddsubr a-z A-Z 0-9.
--

data MathExp
  = Number Number
  | Var    String
  | Neg    MathExp
  | Plus   MathExp MathExp
  | Minus  MathExp MathExp
  | Mult   MathExp MathExp
  | Div    MathExp MathExp
  | Pow    MathExp MathExp
  deriving (Eq, Show)


parseMathTLE :: ReadP TopLevelExp
parseMathTLE = do
  e <- parseMathExp
  return $ MathTLE e


parseMathExp :: ReadP MathExp
parseMathExp = tle
  where
  parseNum = do -- number
    xs <- many1 $ satisfy isDigit
    skipSpaces
    return $ Number $ read xs
  parseVar = do -- variable
    x  <- satisfy isLower
    xs <- many $ satisfy isAlphaNum
    skipSpaces
    return $ Var $ x:xs
  parseNeg = do -- negation
    char '-'
    skipSpaces
    f <- multdiv1
    return $ Neg f
  paren = do -- parenthesized expression
    char '('
    skipSpaces
    e <- ex1
    char ')'
    skipSpaces
    return e
  -- power
  pow0 p = (pow2 p) +++ (pow3 p) 
  pow1 = parseNum +++ parseVar +++ parseNeg +++ paren
  pow2 p = do
    char '^'
    skipSpaces
    base <- pow1
    expon <- pow0 base
    return $ Pow p expon
  pow3 p = do
    return p
  -- multiplication and division, sets precedent, 
  multdiv0 f = (multdiv2 f) +++ (multdiv3 f)
  multdiv1 = do
    f <- pow1
    s <- pow0 f
    return s
  multdiv2 p = do
    c <- choice $ map char ['*','/']
    skipSpaces
    f <- multdiv1
    s <- multdiv0 $ (if c == '*' then Mult else Div) p f 
    return s
  multdiv3 f = do
    return f
  -- addition and subtraction, sets precident, same pattern
  addsub0 f = (addsub2 f) +++ (addsub3 f)
  addsub1 = do
    f <- multdiv1
    s <- multdiv0 f
    return s
  addsub2 p = do
    c <- choice $ map char ['+','-']
    skipSpaces
    f <- addsub1
    s <- addsub0 $ (if c == '+' then Plus else Minus) p f
    return s
  addsub3 f = do
    return f
  -- expression
  ex1 = do
    f <- addsub1
    s <- addsub0 f
    return s
  -- top level
  tle = do
    skipSpaces 
    e <- ex1
    return e


parseLetTLE :: ReadP TopLevelExp
parseLetTLE = tle
  where
  -- variable
  var = do
    c  <- satisfy isLower
    cs <- many $ satisfy isAlphaNum
    skipSpaces
    return $ [c:cs]
  -- multiple variables
  variables vars = (vars1 vars) +++ (vars2 vars)
  vars1 vars = do
    char ','
    skipSpaces
    v <- var
    s <- variables $ vars ++ v
    return s
  vars2 vars = do return vars
  vrs = do
    char '('
    skipSpaces
    v <- var
    s <- variables v
    char ')'
    skipSpaces
    return s
  vvv = var +++ vrs
  -- single expression
  exp = do
    e <- parseMathExp
    return [e]
  -- expressions
  exprs ex = (ex1 ex) +++ (ex2 ex)
  ex1 ex = do
    char ','
    skipSpaces
    e <- exp
    s <- exprs $ ex ++ e
    return s
  ex2 ex = do return ex
  expression = do
    char '('
    skipSpaces
    e <- exp
    s <- exprs e
    char ')'
    skipSpaces
    return s
  -- top level
  tle = do
    skipSpaces
    string "let"
    skipSpaces  
    vs <- vvv
    char '='
    skipSpaces
    es <- if (length vs > 1) then expression else exp
    string "in"
    skipSpaces
    ex <- parseMathExp
    return $ (LetTLE vs es ex)

parseTLE :: ReadP TopLevelExp
parseTLE = do
  tle <- parseLetTLE +++ parseMathTLE
  skipSpaces
  return tle


-- Run the parser on a given string.
--
-- You should not modify this function. 
parse :: String -> Either String TopLevelExp
parse str =
  case (compleaddsubParses, otherParses) of
    ([(result, "")], _  ) -> Right result  -- Only complete result.
    ([]            , [] ) -> Left $ "No parse."
    ([]            , _:_) -> Left $ "Incomplete parse. Unparsed: " ++ (show leaddsubtRemaning)
    (_:_           , _  ) -> Left $ "Ambiguous parse: " ++ (show compleaddsubParses)
  where
    parses = readP_to_S parseTLE str
    (compleaddsubParses, otherParses) =
        partition (\(_, remaining) -> remaining == "") parses
    leaddsubtRemaning = minimumBy (comparing length) . map snd $ otherParses

