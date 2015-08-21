module Main where

import System.IO
import System.Console.Haskeline
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Free
import PartialT
import UntypedLambda
import Parser

confidenceSteps :: ProgramSize a => Double -> a -> Integer
confidenceSteps q a| q >= 0 && q < 1 = max m m' where
  m = 2^(2 * size a) + 1
  m' = ceiling $ (1 / (1-q)) ** 2
stepsConfidence :: ProgramSize a => Integer -> a -> Double
stepsConfidence m a =
  if m >= 2^(2 * size a) + 1 then
    1 - 1 / sqrt (fromIntegral m)
  else
    0.0

data Calude = Normalized Value | Failed EvalError |
              NotYet Integer Term (ThrowsPartial StuckState Value)
instance Show Calude where
  show (Normalized v) = "Normalized: " ++ show v
  show (Failed e) = "Evaluation error: " ++ show e
  show (NotYet n t _) = "Nontermination: p_" ++ show n ++ "(...) = " ++
                      show (stepsConfidence n t)

calude :: Term -> Integer -> Calude
calude t m = case runThrowsPartial $ evaluation of
  Left (EvalFailure err) -> Failed err
  Right (Pure val) -> Normalized val
  Right (Free w) -> NotYet m t (wrap w)
  where evaluation = unwrapN m (eval [] t)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

command :: Command -> Calude
command (StepTest m t) = calude t m
command (ConfidenceTest q t) = calude t (confidenceSteps q t)

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
