{
module Parser where

import Data.Char
import Control.Monad.Except
import PartialT
import UntypedLambda
}

%name lambdaParser
%tokentype { Token }
%monad {ThrowsError StuckState}

%token
'λ'         { TokenLambda     }
'.'         { TokenDot        }
'('         { TokenParen True  }
')'         { TokenParen False }
'+'         { TokenPlus       }
int         { TokenInt $$     }
real        { TokenReal $$   }
var         { TokenVar $$     }
steps       { TokenSteps      }
confidence  { TokenConfidence }

%%

Command :: { Command }
Command : steps int Term          { StepTest $2 $3 }
        | confidence real Term   { ConfidenceTest $2 $3 }

Term    :: { Term }
Term : Value          { ValTerm $1 }
     | '(' Term ')'   { $2 }
     | var            { Variable $1 }
     | Term Term      { Application $1 $2 }
     | Term '+' Term  {Addition $1 $3}
Value   :: { Value }
Value : int               { IntConst $1 }
      | 'λ' var '.' Term  { Abstraction $2 $4 }

{
happyError :: [Token] -> ThrowsError StuckState a
happyError ts = throwError $ BadParse (map show ts)

data Token = TokenLambda | TokenDot | TokenParen Bool | TokenPlus |
             TokenInt Integer | TokenReal Double | TokenVar String |
             TokenSteps | TokenConfidence deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) | isSpace c = lexer cs
             | isAlpha c = lexWord (c:cs)
             | isDigit c = lexNum  (c:cs)
lexer ('λ':cs) = TokenLambda : lexer cs
lexer ('.':cs) = TokenDot : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('(':cs) = TokenParen True : lexer cs
lexer (')':cs) = TokenParen False : lexer cs
lexer (':':cs) = lexWord (':':cs)

lexNum cs = let (num, rest) = span (\x -> isDigit x || x == '.') cs in
              case reads num :: [(Integer, String)] of
                [(n, [])] -> TokenInt n : lexer rest
                _ -> case reads num :: [(Double, String)] of
                  [(n, [])] -> TokenReal n : lexer rest
lexWord cs = case span (\x -> isAlphaNum x || x == ':') cs of
  (":steps", rest) -> TokenSteps : lexer rest
  (":confidence", rest) -> TokenConfidence : lexer rest
  ("lambda", rest) -> TokenLambda : lexer rest
  (var, rest) -> TokenVar var : lexer rest
}
