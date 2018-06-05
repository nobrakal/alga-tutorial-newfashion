{-# LANGUAGE OverloadedStrings #-}

module Mueval (mueval) where

import Control.Concurrent
import System.Process
import System.Environment (getEnvironment, lookupEnv)
import System.Exit

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import           Data.Text (Text)
import qualified Data.Text as T

-- | Evaluate the given expression and return either an error or an
-- (expr,type,value) triple.
-- Muted verion of https://github.com/tryhaskell/tryhaskell/blob/master/src/TryHaskell.hs
mueval :: String -> IO (Either Text (Text,Text,Text))
mueval e = do
  env <- getEnvironment
  let importsfp = "Imports.hs"
      timeout = fromMaybe "1" $ lookup "MUEVAL_TIMEOUT" env
      options = ["-i","-t",timeout,"--expression",e] ++
                ["--no-imports","-l",importsfp]
  (status,out,err) <- readProcessWithExitCode "mueval" options ""
  case status of
    ExitSuccess ->
      case T.lines (T.pack out) of
        [e',typ,value'] | T.pack e == e' -> return (Right (T.pack e,typ,value'))
        _ -> return (Left ("Unable to get type and value of expression: " <> T.pack e))
    ExitFailure{} ->
      case T.lines (T.pack out) of
        [e',_typ,value'] | T.pack e == e' -> return (Left value')
        [e',_typ]        | T.pack e == e' -> return (Left "Evaluation killed!")
        _ -> return (Left $ T.pack (out <> if out == "" then err <> " " <> show status else ""))
