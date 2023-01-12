module RefLangParser where

import RefLang (Expr (..))
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

refLangDef :: LanguageDef st
refLangDef =
  haskellStyle
    { reservedOpNames = ["\\", "->", "=", "!", ":=", ";"],
      reservedNames = ["in", "input", "let", "output", "ref"]
    }

refLang :: TokenParser st
refLang = makeTokenParser refLangDef

pVar :: Parser String
pVar = identifier refLang

pSym :: String -> Parser ()
pSym = reservedOp refLang

pKeyword :: String -> Parser ()
pKeyword = reserved refLang

pExpr :: Parser Expr
pExpr = pFun <|> pLet <|> pSeqLike
  where
    pFun = mkFun <$> (pSym "\\" *> many1 pVar <* pSym "->") <*> pExpr
    pLet =
      Let <$> (pKeyword "let" *> pVar <* pSym "=")
        <*> (pExpr <* pKeyword "in")
        <*> pExpr
    pSeqLike = mkSeq <$> pExprSet <*> optionMaybe (pSym ";" *> pExpr)

    pExprSet = buildExpressionParser opTable pExprSimple where
    pExprSimple =
      parens refLang pExpr
        <|> (Var <$> pVar)
        <|> (Num <$> integer refLang)
        <|> (const Inp <$> pKeyword "input")

    opTable =
      [ [Prefix $ pure Get <* pSym "!"],
        [ Infix (pure App) AssocLeft,
          Prefix $ pure Out <$> pKeyword "output",
          Prefix $ pure Ref <$> pKeyword "ref"
        ],
        [Infix (pure Set <* pSym ":=") AssocRight]
      ]

    mkFun = flip $ foldr Fun
    mkSeq = foldl Seq

parseExpr :: String -> Either ParseError Expr
parseExpr = runParser pExpr () "input"
