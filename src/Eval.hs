{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Eval (evalWithAns, evalIt) where

import Language.Haskell.Interpreter hiding (typeOf)
import qualified Data.Text as T
import Data.Bifunctor (first)

import Types

evalIt :: String -> IO (Either T.Text Bool)
evalIt e = fmap (first handleError) $ runInterpreter $ interpretIt e

handleError :: InterpreterError -> T.Text
handleError (UnknownError s) = T.pack $ "Unknown Error:" ++ s
handleError (NotAllowed s) = T.pack $ "Not Allowed: " ++ s
handleError (GhcException s) = T.pack $ "Ghc Exception: " ++ s
handleError (WontCompile lst) = "Wont Compile: \n" `T.append` T.unlines (map (\(GhcError s) -> T.pack s) lst)

interpretIt :: String -> InterpreterT IO Bool
interpretIt e = do
  setImports ["Prelude","Data.Foldable", "Data.Traversable","Algebra.Graph"]
  interpret e (as :: Bool)

evalWithAns :: Answer -> String ->  IO (Either T.Text Bool)
evalWithAns Answer{..} answerUser = evalIt $ "let " ++ T.unpack (T.intercalate ";" decl) ++ " in " ++ T.unpack verify ++ case typeOf of
  GraphInt -> " (" ++ answerUser ++ ") (" ++ T.unpack answer ++" :: Graph Int )"
  CanFind -> " (" ++ answerUser ++ " ) ( " ++ T.unpack answer ++" )"
  Str -> " \"" ++ answerUser ++ "\" \"" ++ T.unpack answer ++ "\""
  Comparison -> show (length answerUser) ++ " " ++ show (T.length answer) ++ " && (==)" ++ " (" ++ answerUser ++ ") (" ++ T.unpack answer ++" :: Graph Int )"
