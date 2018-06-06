{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Foldable (forM_)
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import System.Exit
import Data.Monoid ((<>))
import Control.Monad (when, forever)
import Data.Bifunctor (second)

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

help :: Maybe Int -> T.Text
help mn = T.unlines $
  [ "Help:\n"
  , " Type \"help\" to show this message"
  , " Type \"quit\" to quit"
  , " Type \"module i\" to go to the module n° i. Modules available: " <> T.pack (show [0..(length modules - 1)])
  ] ++ maybe [] (\n ->
    [ " Type \"clue\" to have a clue on the current goal"
    , " Type \"skip\" to show the solution and skip"
    , " Type \"submodule i\" to go to the submodule n° i. Submodules available: " <> T.pack (show [0..n])
    ]) mn

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
  runSubModules pos subs Nothing
  putStrLn "You have finished the modules, what do you want to do next ?"
  forever $ do
    answerUser <- getUserInput
    au <- handleCommand Nothing answerUser
    case au of
      Left com -> case com of
        Help -> T.putStrLn $ help Nothing
        GoToModule j -> when (j >= 0 && j < length modules) $ runModules $ Just j
        _ -> doInColor Red $ putStrLn "Impossible action"
      Right{} -> doInColor Red $ putStrLn "It is not an action, type \"help\" for help"


runSubModules :: Int -- ^ The curent position in the module list
              -> [SubModule] -- ^ The list of submodules
              -> Maybe Int -- ^ Maybe somewhere to start in the submodule list
              -> IO ()
runSubModules pos arr i = forM_ (maybe id drop i arr) $ \s@SubModule{..} -> do
  let (Just pos') = elemIndex s arr
  doInItalic $ putStr $ "SubModule "++ show pos' ++ "/" ++ show (length arr - 1) ++ ": "
  T.putStrLn abstract
  breakLine
  T.putStrLn instruction
  breakLine
  runSubModule pos arr clue fullAnswer conclusion
  breakLine

runSubModule :: Int -> [SubModule] -> T.Text -> Answer -> T.Text -> IO ()
runSubModule pos arr clue ans conclusion = do
  res <- runQuestion ans
  case res of
    Left instr ->
      case instr of
        Clue -> do
          doInColor Blue $ putStr "Clue: "
          T.putStrLn clue
          defaultR
        Help -> do
          T.putStrLn $ help $ Just $ length arr - 1
          defaultR
        Skip -> T.putStrLn conclusion
        GoToModule i ->
          if i >= 0 && i < length modules
             then runModules $ Just i
             else defaultR
        GoToSubModule i ->
          if i >= 0 && i < length arr
             then runSubModules pos arr (Just i) >> runModules (Just (pos + 1))
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
    defaultR = runSubModule pos arr clue ans conclusion

runQuestion :: Answer -> IO (Either Instruction Bool)
runQuestion ans = do
  answerUser <- getUserInput
  handCom <- handleCommand (Just ans) answerUser
  sequence $ second (verifyInput ans) handCom

-- | Handle the command if there was one in the string. Either return the string intact
handleCommand :: Maybe Answer -> String -> IO (Either Instruction String)
handleCommand mans answerUser = case words answerUser of
  ("help":_) -> return $ Left Help
  ("quit":_) -> die "Bye"
  ("clue":_) -> return $ Left Clue
  ("skip":_) -> do
    doInColor Blue $ putStr "Skipping: "
    case mans of
      Nothing -> return ()
      Just Answer{..} -> do
        putStr "The solution was: \""
        T.putStr answer
    putStrLn "\""
    return $ Left Skip
  ("module":xs:_) -> return $ Left $ GoToModule $ read xs
  ("submodule":xs:_) -> return $ Left $ GoToSubModule $ read xs
  _ -> return $ Right answerUser

verifyInput :: Answer -> String -> IO Bool
verifyInput Answer{..} answerUser = do
  res <- evalIt $ "let " ++ T.unpack (T.intercalate ";" decl) ++ " in " ++ T.unpack verify ++ case typeOf of
     GraphInt -> " (" ++ answerUser ++ ") (" ++ T.unpack answer ++" :: Graph Int )"
     Str -> " \"" ++ answerUser ++ "\" \"" ++ T.unpack answer ++ "\""
     Comparison -> show (length answerUser) ++ " " ++ show (T.length answer) ++ " && (==)" ++ " (" ++ answerUser ++ ") (" ++ T.unpack answer ++" :: Graph Int )"
  case res of
     Right val -> return val
     Left e -> do
       doInColor Red $ T.putStrLn e
       return False

getUserInput :: IO String
getUserInput = runInputT defaultSettings $ fromMaybe (error "Nothing as input") <$> getInputLine "λ: "

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

