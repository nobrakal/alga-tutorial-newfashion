{-# LANGUAGE OverloadedStrings #-}

module Eval (evalIt) where

import Language.Haskell.Interpreter
import qualified Data.Text as T
import Data.Bifunctor (first)

evalIt :: String -> IO (Either T.Text Bool)
evalIt e = fmap (first handleError) $ runInterpreter $ interpretIt e

handleError :: InterpreterError -> T.Text
handleError (UnknownError s) = T.pack $ "Unknown Error:" ++ s
handleError (NotAllowed s) = T.pack $ "Not Allowed: " ++ s
handleError (GhcException s) = T.pack $ "Ghc Exception: " ++ s
handleError (WontCompile lst) = "Wont Compile: \n" `T.append` T.unlines (map (\(GhcError s) -> T.pack s) lst)

interpretIt :: String -> InterpreterT IO Bool
interpretIt e = do
  setImports ["Prelude","Algebra.Graph"]
  interpret e (as :: Bool)
