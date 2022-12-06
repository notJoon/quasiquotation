{-# START_FILE  Expr.hs #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Expr
    ( Exp(..)
    , pExp
    , parse
    ) where
import           Control.Applicative            ( (*>)
                                                , (<$>)
                                                , (<*)
                                                )
import           Data.Char                      ( digitToInt )
import           Data.Data
import           Data.Typeable
import           Text.ParserCombinators.Parsec

-- show 
data Exp = EInt Int
        | EAdd Exp Exp
        | ESub Exp Exp
        | EMul Exp Exp
        | EDiv Exp Exp
        deriving (Show, Data, Typeable)

pExp :: Parser Exp
--- 
pExp = pTerm `chainl1` spaced addop
addop :: Parser (Exp -> Exp -> Exp)
addop = fmap (const EAdd) (char '+') <|> fmap (const ESub) (char '-')

pTerm = spaced pNum `chainl1` spaced mulop
mulop :: Parser (Exp -> Exp -> Exp)
mulop = pOps [EMul,EDiv] ['*','/']

pNum :: Parser Exp
pNum = fmap (EInt . digitToInt) digit

pOps :: [a] -> [Char] -> Parser a
-- pOps fs cs = foldr1 (<|>) $ map pOp $ zip fs cs
pOps fs cs = foldr1 (<|>) $ zipWith (curry pOp) fs cs

whenP :: a -> Parser b -> Parser a
whenP = fmap . const

spaced :: Parser a -> Parser a
spaced p = spaces *> p <* spaces

pOp :: (a,Char) -> Parser a
pOp (f,s) = f `whenP` char s

parseExp :: (MonadFail m, Monad m) => (String, Int, Int) -> String -> m Exp
parseExp (file, line, col) s =
    case runParser p () "" s of
        Left err -> fail $ show err
        Right e -> return e
    where
        p = do
            updatePosition file line col
            spaces
            e <- pExp
            spaces
            eof
            return e

updatePosition file line col = do
    pos <- getPosition
    setPosition $
        (flip setSourceName) file $
        (flip setSourceLine) line $
        (flip setSourceColumn) col $
        pos