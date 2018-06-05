{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Foldable (forM_)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import System.Exit
import Control.Monad.IO.Class

import Algebra.Graph

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Console.Haskeline

import System.Console.ANSI

import Module.Zero

import Mueval
import Types

modules :: [Module]
modules = [mod0]

help :: T.Text
help = T.unlines
  [ "Help"
  , "Type \"help\" to show this message"
  , "Type \"quit\" to quit"
  , "Type \"clue\" to have a clue on the current goal"
  , "Type \"skip\" to show the solution and skip"
  ]

main :: IO ()
main = do
  putStrLn "Welcome in alga-tutorial, this will teach you the basis of algebraic-graphs"
  breakLine
  doInItalic $ putStrLn "Type \"help\" to get help"
  breakLine
  runModules modules

runModules :: [Module] -> IO ()
runModules arr = forM_ arr $ \m@(Module desc subs) -> do
  putStr "Module: "
  T.putStrLn desc
  breakLine
  runSubModules subs

runSubModules :: [SubModule] -> IO ()
runSubModules arr = forM_ arr $ \(SubModule abstract instruction clue answer verify conclusion) -> do
  T.putStrLn abstract
  breakLine
  T.putStrLn instruction
  breakLine
  runInputT defaultSettings' $ runSubModule clue answer verify conclusion
  breakLine
  where
    defaultSettings' = defaultSettings {historyFile = Just ".history"}

runSubModule :: T.Text -> T.Text -> T.Text -> T.Text -> InputT IO ()
runSubModule clue answer verify conclusion = do
  res <- runQuestion clue answer verify conclusion
  case res of
    Nothing -> liftIO $ T.putStrLn conclusion
    Just res' ->
      if res'
         then liftIO $ do
           doInColor Green $ putStrLn "Great, you find the right response"
           T.putStrLn conclusion
           breakLine
         else do
           liftIO $ do
             doInColor Red $ putStrLn "Wrong answer"
             breakLine
           runSubModule clue answer verify conclusion

runQuestion :: T.Text -> T.Text -> T.Text -> T.Text -> InputT IO (Maybe Bool)
runQuestion clue answer verify conclusion = do
  answerUser <- runInputT defaultSettings $ fromMaybe (error "Nothing as input") <$> getInputLine "λ: "
  case answerUser of
    "help" -> liftIO (T.putStrLn help) >> runQuestion clue answer verify conclusion
    "quit" -> liftIO $ die "Bye"
    "clue" -> do
      liftIO $ do
        doInColor Blue $ putStr "Clue: "
        T.putStrLn clue
      runQuestion clue answer verify conclusion
    "skip" -> liftIO $ do
      doInColor Blue $ putStr "Skipping: "
      putStr "The solution was: \""
      T.putStr answer
      putStrLn "\""
      return Nothing
    au -> do
      res <- liftIO $ mueval $ T.unpack verify ++ " (" ++ answerUser ++ ") (" ++ T.unpack answer ++ " :: Graph Int )"
      case res of
        (Right (_,_,val)) -> return $ Just $ val == "True"
        Left e -> do
          liftIO $ doInColor Red $ T.putStrLn e
          return $ Just False

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

