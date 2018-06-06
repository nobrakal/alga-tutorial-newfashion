{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Foldable (forM_)
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import System.Exit
import Control.Monad.IO.Class

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Console.Haskeline

import System.Console.ANSI

import Module.Zero
import Module.One

import Eval
import Types

data Instruction = GoToModule Int | GoToSubModule Int | Skip

modules :: [Module]
modules = [mod0, mod1]

help :: T.Text
help = T.unlines
  [ "Help: "
  , "Type \"help\" to show this message"
  , "Type \"quit\" to quit"
  , "Type \"clue\" to have a clue on the current goal"
  , "Type \"skip\" to show the solution and skip"
  ]

main :: IO ()
main = do
  -- Start the interpreter
  putStrLn "STARTING..."
  _ <- evalIt "True == False"
  putStrLn "Welcome in alga-tutorial, this will teach you the basis of algebraic-graphs"
  breakLine
  doInItalic $ putStrLn "Type \"help\" to get help"
  breakLine
  runModules modules

runModules :: [Module] -> IO ()
runModules arr = forM_ arr $ \m@Module{..}-> do
  let (Just pos) = elemIndex m arr
  doInItalic $ putStr $ "Module "++ show pos ++ "/" ++ show (length arr) ++ ": "
  T.putStrLn desc
  breakLine
  runSubModules subs

runSubModules :: [SubModule] -> IO ()
runSubModules arr = forM_ arr $ \s@SubModule{..} -> do
  let (Just pos) = elemIndex s arr
  doInItalic $ putStr $ "SubModule "++ show pos ++ "/" ++ show (length arr) ++ ": "
  T.putStrLn abstract
  breakLine
  T.putStrLn instruction
  breakLine
  runSubModule clue fullAnswer conclusion
  breakLine

runSubModule :: T.Text -> Answer -> T.Text -> IO ()
runSubModule clue ans conclusion = do
  res <- runInputT defaultSettings $ runQuestion clue ans conclusion
  case res of
    Left i ->
      case i of
         Skip -> T.putStrLn conclusion
    Right res' ->
      if res'
         then do
           doInColor Green $ putStrLn "Great, you find the right response"
           T.putStrLn conclusion
         else do
           doInColor Red $ putStrLn "Wrong answer"
           breakLine
           runSubModule clue ans conclusion

runQuestion :: T.Text -> Answer -> T.Text -> InputT IO (Either Instruction Bool)
runQuestion clue ans@Answer{..} conclusion = do
  answerUser <- runInputT defaultSettings $ fromMaybe (error "Nothing as input") <$> getInputLine "λ: "
  case answerUser of
    "help" -> liftIO (T.putStrLn help) >> runQuestion clue ans conclusion
    "quit" -> liftIO $ die "Bye"
    "clue" -> do
      liftIO $ do
        doInColor Blue $ putStr "Clue: "
        T.putStrLn clue
      runQuestion clue ans conclusion
    "skip" -> liftIO $ do
      doInColor Blue $ putStr "Skipping: "
      putStr "The solution was: \""
      T.putStr answer
      putStrLn "\""
      return $ Left Skip
    _ -> do
      res <- liftIO $ evalIt $ T.unpack verify ++ " (" ++ answerUser ++ ") (" ++ T.unpack answer ++ " :: " ++ T.unpack typeOf ++ " )"
      case res of
        Right val -> return $ Right val
        Left e -> do
          liftIO $ doInColor Red $ T.putStrLn e
          return $ Right False

breakLine :: IO ()
breakLine = putStr "\n"

doInColor :: Color -> IO () -> IO ()
doInColor color action = do
  setSGR [SetColor Foreground Dull color]
  action
  setSGR [Reset]

doInItalic :: IO () -> IO ()
doInItalic action = do
  setSGR [SetItalicized True]
  action
  setSGR [Reset]

