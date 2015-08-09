module Main where

import System.IO
import System.Console.Haskeline
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Free
import PartialT
import UntypedLambda
import Parser

confidenceSteps :: Double -> Integer
confidenceSteps q | q >= 0 && q < 1 = ceiling $ (1 / (1-q)) ** 2
stepsConfidence :: Integer -> Double
stepsConfidence 0 = 0.0
stepsConfidence 1 = 0.0
stepsConfidence m = 1 - 1 / sqrt (fromIntegral m)

data Calude = Normalized Value | Failed EvalError |
              NotYet Integer (ThrowsPartial StuckState Value)
instance Show Calude where
  show (Normalized v) = "Normalized: " ++ show v
  show (Failed e) = "Evaluation error: " ++ show e
  show (NotYet n p) = "Nontermination: p_" ++ show n ++ "(...) = " ++
                      show (stepsConfidence n)

calude :: Term -> Integer -> Calude
calude t m = case runThrowsPartial $ evaluation of
  Left (EvalFailure err) -> Failed err
  Right (Pure val) -> Normalized val
  Right (Free w) -> NotYet m (wrap w)
  where evaluation = unwrapN m (eval [] t)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

command :: Command -> Calude
command (StepTest m t) = calude t m
command (ConfidenceTest q t) = calude t (confidenceSteps q)

readCommand :: String -> ThrowsError StuckState Command
readCommand = lambdaParser . lexer

commandString :: String -> String
commandString c = case fmap command $ readCommand c of
  Left err -> show err
  Right val -> show val

evalAndPrint :: String -> InputT IO ()
evalAndPrint expr = outputStrLn $ commandString expr

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      maybeLine <- getInputLine ">> "
      case maybeLine of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just line -> do evalAndPrint line
                        loop
