{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Foldable (forM_)
import Control.Monad (when)
import Data.Maybe (fromMaybe)

import Algebra.Graph

import qualified Data.Text as T

import System.Console.Haskeline

import System.Console.ANSI

import Module.Zero

import Mueval
import Types

modules :: [Module]
modules = [mod0]

main :: IO ()
main = do
  putStrLn "Welcome in alga-tutorial, this will teach you the basis of algebraic-graphs"
  breakLine
  runModules modules

runModules :: [Module] -> IO ()
runModules [] = return ()
runModules (Module desc subs:xs) = do
  putStrLn $ T.unpack desc
  breakLine
  runSubModules 0 subs
  runModules xs

runSubModules :: Int -- ^ The number of time the question was asked
              -> [SubModule]
              -> IO ()
runSubModules _ [] = return ()
runSubModules times s@(SubModule abstract instructions clue answer conclusion:xs) = do
  when (times == 0) $ do
    putStrLn $ T.unpack abstract
    breakLine
    putStrLn $ T.unpack instructions
    breakLine
  res <- runQuestion instructions answer
  if res
     then do
       doInColor Green $ putStrLn "Great, you find the right response"
       breakLine
       putStrLn $ T.unpack conclusion
       runSubModules 0 xs
     else do
       doInColor Red $ putStrLn "Wrong answer"
       breakLine
       runSubModules (times+1) s

runQuestion :: T.Text -> Either T.Text T.Text -> IO Bool
runQuestion instructions (Left answer') = do
  answerUser <- runInputT defaultSettings $ fromMaybe (error "Nothing as input") <$> getInputLine "λ: "
  res <- mueval $ "(" ++ answerUser ++ ") == (" ++ T.unpack answer' ++ " :: Graph Int )"
  case res of
    (Right (_,_,val)) -> return $ val == "True"
    Left e -> do
      doInColor Red $ putStrLn $ T.unpack e
      return False
runQuestion _ _ = error "Right ? Right !"

breakLine :: IO ()
breakLine = putStr "\n"

doInColor :: Color -> IO () -> IO ()
doInColor color action = do
  setSGR [SetColor Foreground Dull color]
  action
  setSGR [Reset]
