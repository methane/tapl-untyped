import Control.Applicative hiding ((<|>))
import Data.Char
import Text.Parsec
import Text.Parsec.String


data Term = TmAbs String Term | TmApp Term Term | TmVar String | TmBool Bool deriving Show

bool :: Parser Term
bool = do
        string "true"
        return $ TmBool True
       <|> do
        string "false"
        return $ TmBool False

app :: Parser Term
app = nonapp `chainl1` call
    where call = do { char ' '; return TmApp }

name = many1 $ oneOf ('\'' : ['a' .. 'z'])

lambda :: Parser Term
lambda = do
    char '\\'
    v <- name
    char '.'
    t <- term
    return $ TmAbs v t

paren :: Parser Term
paren = do
    char '('
    t <- term
    char ')'
    return t

var :: Parser Term
var = TmVar <$> name

nonapp :: Parser Term
nonapp = paren <|> lambda <|> bool <|> var

term :: Parser Term
term = try app <|> nonapp

expr :: Parser Term
expr = term <* eof

test s = do
    putStr (s ++ ":\n  ")
    print (parse expr s s)

main = do
    test "true"
    test "false"
    test "\\x.(x)"
    test "(\\x.(x))"
    test "(\\x.(x)) true"
    test "(\\x.(x)) xy'"
    test "x y z"
