{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Foldable (forM_)
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import System.Exit
import Control.Monad.IO.Class
import Data.Monoid ((<>))

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Console.Haskeline

import System.Console.ANSI

import Module.Zero
import Module.One
import Module.Two
import Module.Three

import Eval
import Types

data Instruction = GoToModule Int | GoToSubModule Int | Skip | Help | Clue

modules :: [Module]
modules = [mod0, mod1, mod2, mod3]

help :: Int -> T.Text
help n = T.unlines
  [ "Help:\n"
  , " Type \"help\" to show this message"
  , " Type \"quit\" to quit"
  , " Type \"clue\" to have a clue on the current goal"
  , " Type \"skip\" to show the solution and skip"
  , " Type \"module i\" to go to the module n° i. Modules available: " <> T.pack (show [0..(length modules - 1)])
  , " Type \"submodule i\" to go to the submodule n° i. Submodules available: " <> T.pack (show [0..n])
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
  runModules Nothing

runModules :: Maybe Int -> IO ()
runModules i = forM_ (maybe id drop i modules) $ \m@Module{..}-> do
  let (Just pos) = elemIndex m modules
  doInItalic $ putStr $ "Module "++ show pos ++ "/" ++ show (length modules - 1) ++ ": "
  T.putStrLn desc
  breakLine
  runSubModules subs Nothing
  die "Bye, thank you for doing this tutorial"

runSubModules :: [SubModule] -> Maybe Int -> IO ()
runSubModules arr i = forM_ (maybe id drop i arr) $ \s@SubModule{..} -> do
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
  res <- runInputT defaultSettings $ runQuestion ans
  case res of
    Left instr ->
      case instr of
        Clue -> do
          doInColor Blue $ putStr "Clue: "
          T.putStrLn clue
          defaultR
        Help -> do
          T.putStrLn $ help $ length arr - 1
          defaultR
        Skip -> T.putStrLn conclusion
        GoToModule i ->
          if i >= 0 && i < length modules
             then runModules $ Just i
             else defaultR
        GoToSubModule i ->
          if i >= 0 && i < length arr
             then runSubModules arr $ Just i
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

runQuestion :: Answer -> InputT IO (Either Instruction Bool)
runQuestion Answer{..} = do
  answerUser <- runInputT defaultSettings $ fromMaybe (error "Nothing as input") <$> getInputLine "λ: "
  case words answerUser of
    ("help":_) -> return $ Left Help
    ("quit":_) -> liftIO $ die "Bye"
    ("clue":_) -> return $ Left Clue
    ("skip":_) -> liftIO $ do
      doInColor Blue $ putStr "Skipping: "
      putStr "The solution was: \""
      T.putStr answer
      putStrLn "\""
      return $ Left Skip
    ("module":xs:_) -> return $ Left $ GoToModule $ read xs
    ("submodule":xs:_) -> return $ Left $ GoToSubModule $ read xs
    _ -> do
      res <- liftIO $ evalIt $ "let " ++ T.unpack (T.intercalate ";" decl) ++ " in " ++ T.unpack verify ++ case typeOf of
        GraphInt -> " (" ++ answerUser ++ ") (" ++ T.unpack answer ++" :: Graph Int )"
        Str -> " \"" ++ answerUser ++ "\" \"" ++ T.unpack answer ++ "\""
        Comparison -> show (length answerUser) ++ " " ++ show (T.length answer) ++ " && (==)" ++ " (" ++ answerUser ++ ") (" ++ T.unpack answer ++" :: Graph Int )"
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

