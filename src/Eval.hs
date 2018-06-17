{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Eval (evalWithAns, evalIt) where

import Language.Haskell.Interpreter hiding (typeOf)
import qualified Data.Text as T
import Data.Bifunctor (first)
import Data.Monoid ((<>))

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
  setImportsF
    [ ModuleImport "Prelude" NotQualified NoImportList
    , ModuleImport "System.IO.Unsafe" NotQualified (ImportList ["unsafePerformIO"] )
    , ModuleImport "Data.Foldable" NotQualified NoImportList
    , ModuleImport "Data.Traversable" NotQualified NoImportList
    , ModuleImport "Control.Applicative" NotQualified (ImportList ["liftA2"] )
    , ModuleImport "Data.Function" NotQualified NoImportList
    , ModuleImport "Data.Char" NotQualified NoImportList
    , ModuleImport "Algebra.Graph" NotQualified NoImportList
    ]
  interpret e (as :: Bool)

evalWithAns :: Answer -> String ->  IO (Either T.Text Bool)
evalWithAns Answer{..} answerUser = evalIt $ "let " ++ T.unpack (T.intercalate ";" $ map (\(x,(t,b)) -> x <> if b then " :: " <> t else "") decl) ++ " in " ++ T.unpack verify ++ case typeOf of
  GraphInt -> " (" ++ answerUser ++ ") (" ++ T.unpack answer ++" :: Graph Int )"
  IOGraphInt -> "( unsafePerformIO (" ++ answerUser ++ ")) (unsafePerformIO (" ++ T.unpack answer ++" :: IO (Graph Int) ))"
  CanFind -> " (" ++ answerUser ++ " ) ( " ++ T.unpack answer ++" )"
  Str -> " \"" ++ answerUser ++ "\" \"" ++ T.unpack answer ++ "\""
  Comparison -> show (length answerUser) ++ " " ++ show (T.length answer) ++ " && (==)" ++ " (" ++ answerUser ++ ") (" ++ T.unpack answer ++" :: Graph Int )"
  CustomComp f -> " \"" ++ answerUser ++ "\" \"" ++ T.unpack answer ++ "\"" ++ " && (==)" ++ " (" ++ f answerUser ++ ") (" ++ T.unpack answer ++" :: Graph Int )"
