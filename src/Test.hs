module Main where

import Data.Foldable (forM_)

import System.Exit

import Module.Zero (mod0)
import Module.One  (mod1)

import Eval
import Types

import qualified Data.Text as T


-- | It verify that the answer provided is verified by itself
main :: IO ()
main = forM_ (subs mod0 ++ subs mod1) $ \submod -> do
  let (Answer ans typeOf verify) = fullAnswer submod
  res <- evalIt $ T.unpack verify ++ case typeOf of
    GraphInt -> " (" ++ T.unpack ans ++ ") (" ++ T.unpack ans ++ " :: Graph Int )"
    Str -> " \"" ++ T.unpack ans ++ "\" \"" ++ T.unpack ans ++ "\""
    Comparison -> show (T.length ans) ++ " " ++ show (T.length ans) ++ " && (==)" ++ " (" ++ T.unpack ans ++ ") (" ++ T.unpack ans ++" :: Graph Int )"
  either (\e -> die $ "Non Coherent\n" ++ show submod ++ show e) (const $ return ()) res
