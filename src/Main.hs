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
runModules [] = return ()
runModules (Module desc subs:xs) = do
  putStr $ "Part " ++ show (length modules - length xs - 1) ++ ": "
  T.putStrLn desc
  breakLine
  runSubModules subs
  runModules xs

-- TODO fold
runSubModules :: [SubModule] -> IO ()
runSubModules arr = forM_ arr $ \(SubModule abstract instruction clue answer conclusion) -> do
  T.putStrLn abstract
  breakLine
  T.putStrLn instruction
  breakLine
  runInputT defaultSettings' $ runSubModule clue answer conclusion
  where
    defaultSettings' = defaultSettings {historyFile = Just ".history"}

runSubModule :: T.Text -> Either T.Text T.Text -> T.Text -> InputT IO ()
runSubModule clue answer conclusion = do
  res <- runQuestion clue answer conclusion
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
           runSubModule clue answer conclusion

runQuestion :: T.Text -> Either T.Text T.Text -> T.Text -> InputT IO (Maybe Bool)
runQuestion clue a@(Left answer') conclusion = do
  answerUser <- runInputT defaultSettings $ fromMaybe (error "Nothing as input") <$> getInputLine "λ: "
  case answerUser of
    "help" -> liftIO (T.putStrLn help) >> runQuestion clue a conclusion
    "quit" -> liftIO $ die "Bye"
    "clue" -> do
      liftIO $ do
        doInColor Blue $ putStr "Clue: "
        T.putStrLn clue
      runQuestion clue a conclusion
    "skip" -> liftIO $ do
      doInColor Blue $ putStr "Skipping: "
      putStr "The solution was: \""
      T.putStr answer'
      putStrLn "\""
      return Nothing
    au -> do
      res <- liftIO $ mueval $ "(" ++ answerUser ++ ") == (" ++ T.unpack answer' ++ " :: Graph Int )"
      case res of
        (Right (_,_,val)) -> return $ Just $ val == "True"
        Left e -> do
          liftIO $ doInColor Red $ T.putStrLn e
          return $ Just False
runQuestion _ _ _ = error "Right ? Right !"

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

