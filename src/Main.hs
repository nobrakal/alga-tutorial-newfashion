{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Foldable (forM_)
import Data.Maybe (fromMaybe)
import Data.List (elemIndex, mapAccumL)
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
import Module.Four

import Eval
import Types

data Instruction = GoToModule Int | GoToSubModule Int | Skip | Help | Clue

modules :: [Module]
modules = [mod0, mod1, mod2, mod3, mod4]

help :: Maybe Int -> T.Text
help mn = T.unlines $
  [ "__Help__ :\n"
  , " Type \"help\" to show this message"
  , " Type \"quit\" to quit"
  , " Type \"module i\" to go to the module n° i. Modules available: \n  " <> T.intercalate "\n  " (snd (mapAccumL (\k v  -> (k+1, T.pack (show k ++ ". ") <> name v) ) (0 :: Int) modules))
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
  render $ help Nothing
  runMain

runModules :: Int -> IO ()
runModules i = do
  forM_ (drop i modules) $ \m@Module{..}-> do
    let (Just pos) = elemIndex m modules
    doInItalic $ putStr $ "Module "++ show pos ++ "/" ++ show (length modules - 1) ++ ": "
    T.putStrLn desc
    breakLine
    runSubModules pos subs Nothing
  putStrLn "You have finished the modules, what do you want to do next ?"
  runMain

runMain :: IO ()
runMain = forever $ do
    answerUser <- getUserInput
    au <- handleCommand Nothing answerUser
    case au of
      Left com -> case com of
        Help -> render $ help Nothing
        GoToModule j -> when (j >= 0 && j < length modules) $ runModules j
        _ -> doInColor Red $ putStrLn "Impossible action"
      Right{} -> doInColor Red $ putStrLn "It is not an action, type \"help\" for help"


runSubModules :: Int -- ^ The curent position in the module list
              -> [SubModule] -- ^ The list of submodules
              -> Maybe Int -- ^ Maybe somewhere to start in the submodule list
              -> IO ()
runSubModules pos arr i = forM_ (maybe id drop i arr) $ \s@SubModule{..} -> do
  let (Just pos') = elemIndex s arr
  doInItalic $ putStr $ "SubModule "++ show pos' ++ "/" ++ show (length arr - 1) ++ ": "
  render abstract
  render instruction
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
          render clue
          defaultR
        Help -> do
          render $ help $ Just $ length arr - 1
          defaultR
        Skip -> render conclusion
        GoToModule i ->
          if i >= 0 && i < length modules
             then runModules i
             else defaultR
        GoToSubModule i ->
          if i >= 0 && i < length arr
             then runSubModules pos arr (Just i) >> runModules (pos + 1)
             else defaultR
    Right res' ->
      if res'
         then do
           doInColor Green $ putStrLn "Great, you find the right response"
           T.putStrLn $ "Your answer was an equivalent of: \"" <> answer ans <> "\""
           breakLine
           render conclusion
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

-- | Verify the given input using a provided answer to test
verifyInput :: Answer -> String -> IO Bool
verifyInput ans answerUser = evalWithAns ans answerUser >>= either (\e -> printRed e >> return False) return
  where
    printRed e = removeAndPrint $ foldr (\todo -> T.replace todo (T.takeWhile (/= '=') todo)) e (decl ans)
    removeAndPrint = doInColor Red . T.putStrLn . T.replace (answer ans) "ANSWER"

getUserInput :: IO String
getUserInput = runInputT defaultSettings' $ fromMaybe (error "Nothing as input") <$> getInputLine "λ: "
  where
    defaultSettings' = defaultSettings {historyFile = Just ".history"}

breakLine :: IO ()
breakLine = putStr "\n"

doInColor :: Color -> IO () -> IO ()
doInColor color = (>>) (setSGR [SetColor Foreground Dull color]) . actAndReset

doInItalic :: IO () -> IO ()
doInItalic = (>>) (setSGR [SetItalicized True]) . actAndReset

doUnderlined :: IO () -> IO ()
doUnderlined = (>>) (setSGR [SetUnderlining SingleUnderline]) . actAndReset

-- | Render a Text, it output in italic words like _home_ and underline like __sweet__
-- BreakLine at the end
render :: T.Text -> IO ()
render t = do
  forM_ (T.lines t) $ \line -> do
    forM_ (T.split (== ' ') line) $ \word -> do
      render' word
      putStr " "
    breakLine
  breakLine

render' :: T.Text -> IO ()
render' t
  | T.length t < 2 = T.putStr t
  | T.take 2 t == "__" && T.drop (T.length t - 2) t == "__" = doUnderlined $ T.putStr $ T.drop 2 $ T.take (T.length t - 2) t
  | T.head t == '_' && T.last t == '_' = doInItalic $ T.putStr $ T.tail $ T.init t
  | otherwise = T.putStr t

actAndReset :: IO () -> IO ()
actAndReset act = act >> setSGR [Reset]
