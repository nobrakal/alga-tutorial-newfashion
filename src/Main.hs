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
  , "Type \"module i\" to go to the module n° i"
  , "Type \"submodule i\" to go to the submodule n° i"
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
  doInItalic $ putStr $ "Module "++ show pos ++ "/" ++ show (length arr - 1) ++ ": "
  T.putStrLn desc
  breakLine
  runSubModules subs

runSubModules :: [SubModule] -> IO ()
runSubModules arr = forM_ arr $ \s@SubModule{..} -> do
  let (Just pos) = elemIndex s arr
  doInItalic $ putStr $ "SubModule "++ show pos ++ "/" ++ show (length arr - 1) ++ ": "
  T.putStrLn abstract
  breakLine
  T.putStrLn instruction
  breakLine
  runSubModule arr clue fullAnswer conclusion
  breakLine

runSubModule :: [SubModule] -> T.Text -> Answer -> T.Text -> IO ()
runSubModule arr clue ans conclusion = do
  res <- runInputT defaultSettings $ runQuestion clue ans conclusion
  case res of
    Left instr ->
      case instr of
        Skip -> T.putStrLn conclusion
        GoToModule i ->
          if i >= 0 && i < length modules
             then runModules $ drop i modules
             else defaultR
        GoToSubModule i ->
          if i >= 0 && i < length arr
             then runSubModules $ drop i arr
             else defaultR
    Right res' ->
      if res'
         then do
           doInColor Green $ putStrLn "Great, you find the right response"
           T.putStrLn conclusion
         else do
           doInColor Red $ putStrLn "Wrong answer"
           breakLine
           defaultR
  where
    defaultR = runSubModule arr clue ans conclusion

runQuestion :: T.Text -> Answer -> T.Text -> InputT IO (Either Instruction Bool)
runQuestion clue ans@Answer{..} conclusion = do
  answerUser <- runInputT defaultSettings $ fromMaybe (error "Nothing as input") <$> getInputLine "λ: "
  case words answerUser of
    ("help":_) -> liftIO (T.putStrLn help) >> runQuestion clue ans conclusion
    ("quit":_) -> liftIO $ die "Bye"
    ("clue":_) -> do
      liftIO $ do
        doInColor Blue $ putStr "Clue: "
        T.putStrLn clue
      runQuestion clue ans conclusion
    ("skip":_) -> liftIO $ do
      doInColor Blue $ putStr "Skipping: "
      putStr "The solution was: \""
      T.putStr answer
      putStrLn "\""
      return $ Left Skip
    ("module":xs:_) -> return $ Left $ GoToModule $ read xs
    ("submodule":xs:_) -> return $ Left $ GoToSubModule $ read xs
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

