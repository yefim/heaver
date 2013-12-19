{-
This is OurStache, our own Mustache-like template system engine built for Heaver.

It has four main components:
- Context - the Map which stores the variable state
- Expr - the AST intermediate state
- stacheP - the parser which turns a string into an Expr
- pp - the printer which turns an Expr and a Context into a String

It implements a subset of the Mustache spec. Specifically:

{{ variable }} - Variable replacement with HTML escaping
{{{ variable }}} - And unescaped too

Control statements:
{{ #variable }}
  ...
{{ /variable }}
Act as if/then if variable is a Single
Act as forloop if variable is a List

{{.}} - control context variable
{{{.}}} - unescaped
Essentially the 'i' of the for loop.
Also falls back to single value if used inside an if/then control block.
-}

{-# OPTIONS -Wall -fno-warn-orphans -fwarn-tabs -fno-warn-type-defaults #-}

module OurStache where

import Parser
import ParserCombinators
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (liftM, liftM2, liftM3)
import Test.HUnit


type Variable = String

data ContextValue =
    Single Value
  | List [Value]
  | None
  deriving (Show, Eq)

data Value = 
    IntVal Int
  | BoolVal Bool
  | StringVal String
  | Empty
  deriving (Show, Eq)

data Expr = 
    Plain String -- }}thisstuff{{
  | Var Variable -- {{{var}}}
  | VarE Variable -- {{var}}
  | Dot -- {{{.}}}
  | DotE -- {{.}}
  | Control Variable Expr -- {{#v}}...{{/v}}
  | Append Expr Expr
  deriving (Show, Eq)


data Context = Context {
  vars :: Map Variable ContextValue,
  ctl :: Value -- control value - the #v from within a control statement
}

-- Some Context helpers:

buildCtx :: [(Variable, ContextValue)] -> Context
buildCtx l = Context (Map.fromList l) Empty

getCtx :: Variable -> Context -> ContextValue
getCtx v c = Map.findWithDefault None v (vars c)

showCtx :: Variable -> Context -> String
showCtx v c = case (getCtx v c) of
                Single val -> showValue val
                List l -> concatMap showValue l
                None -> ""

showValue :: Value -> String
showValue (IntVal v) = show v
showValue (BoolVal v) = show v
showValue (StringVal v) = v
showValue Empty = ""

setCtl :: Context -> Value -> Context
setCtl c v = c { ctl = v }

-- HTML escaping, the difference between double and triple brackets
escapeHtml :: String -> String
escapeHtml = concatMap escapeHtmlChar

escapeHtmlChar :: Char -> String
escapeHtmlChar '<' = "&lt;"
escapeHtmlChar '>' = "&gt;"
escapeHtmlChar '&' = "&amp;"
escapeHtmlChar '"' = "&quot;"
escapeHtmlChar '\'' = "&#39;"
escapeHtmlChar c = [c]


{-
This is the printer.
It takes an Expr AST, and then stuffs Context variables into it
To produce output including the values of the variables, as Mustache would.
-}
pp :: Expr -> Context -> String
pp (Plain s) _ = s
pp (Var v) c = showCtx v c
pp (VarE v) c = escapeHtml $ showCtx v c
pp (Dot) c = showValue $ ctl c
pp (DotE) c = escapeHtml . showValue $ ctl c
pp (Control v e) c = case (getCtx v c) of
                       Single val -> pp e (setCtl c val) -- display e
                       List l -> concatMap (\val -> pp e $ setCtl c val) l -- loop e over l
                       None -> "" -- hide block
pp (Append e1 e2) c = pp e1 c ++ pp e2 c


emCtx :: Context
exCtx :: Context
emCtx = buildCtx []
exCtx = buildCtx [("hi", Single (StringVal "sup")),
                  ("html", Single (StringVal "<html>")),
                  ("wat", Single (IntVal 37)),
                  ("l", List [(StringVal "one"), (StringVal "two")]),
                  ("n", None)]

-- put some tests here with emCtx/exCtx
-- use function at bottom to help generate Expr ASTs
printerTest :: Test
printerTest = TestList [pp (Plain "hi") emCtx ~?= "hi",
                        pp (Var "hi") emCtx ~?= "",
                        pp (Var "hi") exCtx ~?= "sup",
                        pp (Var "wat") exCtx ~?= "37",
                        pp (Var "n") exCtx ~?= "",
                        pp (VarE "html") exCtx ~?= "&lt;html&gt;",
                        pp (Append (Var "hi") (Plain " guys")) exCtx ~?= "sup guys",
                        pp (Control "l" Dot) emCtx ~?= "",
                        pp (Control "l" (Append (Plain "hi") Dot)) exCtx ~?= "hionehitwo"]


-- Now begins the parsing.
openP :: Parser Char String
closeP :: Parser Char String
openP = liftM2 seq (string "#") varNameP
closeP = liftM2 seq (string "/") varNameP

controlP :: Parser Char Expr
controlP = liftM3 (\var subExpr _ -> Control var subExpr)
                  (doubleWrap openP)
                  stacheP
                  (doubleWrap closeP)

dotP :: Parser Char Expr
dotEP :: Parser Char Expr
varP :: Parser Char Expr
varEP :: Parser Char Expr
dotP = liftM (const Dot) $ string "."
dotEP = liftM (const DotE) $ string "."
varP = liftM Var varNameP
varEP = liftM VarE varNameP


spaces :: Parser Char String
spaces = many space

alphaNums :: Parser Char String
alphaNums = many (alpha <|> digit)

doubleOpen :: Parser Char String
doubleClose :: Parser Char String
tripleOpen :: Parser Char String
tripleClose :: Parser Char String
doubleOpen = string "{{"
doubleClose = string "}}"
tripleOpen = string "{{{"
tripleClose = string "}}}"

--1 alpha, then 0 or more alphanums - standard naming restriction
varNameP :: Parser Char String
varNameP = liftM2 (\f r -> f:r) alpha alphaNums

-- first character being { is okay
-- but then stop whenever you find a second {
plainP :: Parser Char Expr
plainP = liftM2 (\f r -> Plain (f:r)) get (many1 (satisfy (/= '{')))

spaceWrap :: Parser Char b -> Parser Char b
spaceWrap p = between spaces p spaces

doubleWrap :: Parser Char b -> Parser Char b
tripleWrap :: Parser Char b -> Parser Char b
doubleWrap p = between doubleOpen (spaceWrap p) doubleClose
tripleWrap p = between tripleOpen (spaceWrap p) tripleClose

doubleBP :: Parser Char Expr
tripleBP :: Parser Char Expr
doubleBP = doubleWrap $ choice [dotEP, varEP]
tripleBP = tripleWrap $ choice [varP, dotP]

-- the mother parser of any one expr
exprP :: Parser Char Expr
exprP = choice [tripleBP, controlP, doubleBP, plainP]

-- Chain exprP together with Append to create the fulltext parser
stacheP :: Parser Char Expr
stacheP = (liftM2 Append exprP stacheP) <|> exprP


-- Extracts the Expr from a parse, assuming it works
-- Use to help build tests
extract :: String -> Expr
extract s = fst $ head $ doParse stacheP s

-- Easy test generator - assumes no trailing unmatched characters
pt :: String -> Expr -> Test
pt s e = doParse stacheP s ~?= [(e, "")]

parserTest :: Test
parserTest = TestList [pt "{{#d}}hi{{/d}}" (Control "d" (Plain "hi")),
                       pt "{{{abc }}}" (Var "abc"),
                       pt "{{ abc }}" (VarE "abc"),
                       pt "{{  . }}" DotE,
                       pt "{{{.}}}" Dot,
                       pt "{{abc }}} " (Append (VarE "abc") (Plain "} ")),
                       pt "{{a}} hi {{{b}}} sup { {{#c}}{{{.}}}{{/c}} end" (Append (VarE "a") (Append (Plain " hi ") (Append (Var "b") (Append (Plain " sup ") (Append (Plain "{ ") (Append (Control "c" Dot) (Plain " end")))))))]

{-
main :: IO ()
main = do 
   _ <- runTestTT $ TestList [printerTest, parserTest]
   return ()
-}
