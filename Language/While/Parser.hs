module Language.While.Parser (stmts) where
import Prelude hiding (LT,GT,EQ)
import Text.Parsec
import Language.While.Syntax
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Expr as E

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser 
         (L.emptyDef
           {
                 P.commentStart   = ""
               , P.commentEnd     = ""
               , P.commentLine    = "#"
               , P.nestedComments = False 
               , P.identStart     = letter <|> char '_'
               , P.identLetter    = alphaNum <|> oneOf "_'"
               , P.opStart        = P.opLetter L.emptyDef
               , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , P.reservedOpNames = ["=","*","/","+","-","<","<=",">",">=","==","~"]
               , P.reservedNames  = ["puts", "skip", "while", "if", "else", "end", "true", "false"]
               , P.caseSensitive  = True
           }
         )

--To make stuff less crowded, use this shorthand
integer   = P.integer lexer
parens    = P.parens lexer
semi      = P.semi lexer
colon     = P.colon lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer
stringLiteral = P.stringLiteral lexer


--Tables of operators and their precedence
binary  name fun assoc = E.Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = E.Prefix (do{ reservedOp name; return fun })
postfix name fun       = E.Postfix (do{ reservedOp name; return fun })


rexpr = do
          a1 <- aexpr
          op <- relation
          a2 <- aexpr
          return $ op a1 a2

relation =    (reservedOp "<" >> return (LT)) 
          <|> (reservedOp "<=" >> return (LTE))
          <|> (reservedOp ">" >> return (GT))
          <|> (reservedOp ">=" >> return (GTE))
          <|> (reservedOp "==" >> return (EQ))
          <|> (reservedOp "!=" >> return (NE))
            
              

boolean_optable = [
                      [
                        prefix "~" (NOT)
                      ]
                  ]

boolean_constant = (do
                      reserved "false"
                      return F
                  <|> do
                        reserved "true"
                        return T) <?> "Boolean Constant"

bool_term = parens bexpr
             <|> rexpr 
             <|> boolean_constant
             <?> "Boolean term"

bexpr = E.buildExpressionParser boolean_optable bool_term
       <?> "expression"


--Arithmetic Expressions
arith_optable = [
                   [prefix "-" (Neg)] 
                  ,[binary "*" (Mul) E.AssocLeft, binary "/" (Div) E.AssocLeft]
                  ,[binary "+" (Add) E.AssocLeft, binary "-" (Sub) E.AssocLeft]
                ]          
constant = do
              i <- integer
              return $ Const i

arith_ident = do
                i <- identifier
                return $ Ident $ Var i

arith_term = parens aexpr
             <|> constant 
             <|> arith_ident 
             <?> "simple expression"

aexpr = E.buildExpressionParser arith_optable arith_term
       <?> "expression"

--Statement Parsing
skip = do
         reserved "skip"
         return Skip

assign = do
           i <- identifier
           reservedOp "="
           a <- aexpr
           return $ Assign (Var i) a

ifthen = do 
            reserved "if"
            b <- bexpr
            colon
            s1 <- stmts
            reserved "else"
            colon 
            s2 <- stmts
            reserved "end"
            return $ If b s1 s2

while = do
           reserved "while"
           b <- bexpr
           colon
           s1 <- stmts
           reserved "end"
           return $ While b s1

puts = do
          reserved "puts"
          e <- aexpr
          return $ Puts e

stmt = puts <|> skip <|> assign <|> ifthen <|> while <?> "Statement"

stmts = do
          ss <- stmt `endBy1` semi
          return $ Seq ss
