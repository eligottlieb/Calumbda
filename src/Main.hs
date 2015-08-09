module Main where

import System.IO
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Free
import Errors
import UntypedLambda
import Parser

confidenceSteps :: Double -> Integer
confidenceSteps q | q >= 0 && q < 1 = ceiling $ (1 / (1-q)) ** 2
stepsConfidence :: Integer -> Double
stepsConfidence 0 = 0.0
stepsConfidence 1 = 0.0
stepsConfidence m = 1 - 1 / sqrt (fromIntegral m)

data Calude = Normalized Value | Failed EvalError |
              Nontermination Double deriving Show
caludeSteps :: Term -> Integer -> Calude
caludeSteps t m = case runThrowsPartial $ evaluation of
  Left (EvalFailure err) -> Failed err
  Right (Pure val) -> Normalized val
  Right (Free _) -> Nontermination (stepsConfidence m)
  where evaluation = unwrapN m (eval [] t)
caludeConfidence :: Term -> Double -> Calude
caludeConfidence t q = case runThrowsPartial $ evaluation of
  Left (EvalFailure err) -> Failed err
  Right (Pure val) -> Normalized val
  Right (Free _) -> Nontermination q
  where evaluation = unwrapN (confidenceSteps q) (eval [] t)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

command :: Command -> Calude
command (StepTest m t) = caludeSteps t m
command (ConfidenceTest q t) = caludeConfidence t q

readCommand :: String -> ThrowsError StuckState Command
readCommand = lambdaParser . lexer

commandString :: String -> IO String
commandString c = case fmap command $ readCommand c of
  Left err -> return $ show err
  Right val -> return $ show val

evalAndPrint :: String -> IO ()
evalAndPrint expr = commandString expr >>= putStrLn

untilM :: Monad m => (a -> Bool) -> m a -> (a -> m b) -> b -> m b
untilM pred first second otherwise = do
  result <- first
  if pred result then
    second result >> untilM pred first second otherwise
  else
    return otherwise

main :: IO ()
main = untilM (/= ":quit") (readPrompt ">> ") evalAndPrint ()
