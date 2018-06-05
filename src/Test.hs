module Main where

import Data.Foldable (forM_)

import System.Exit

import Module.Zero (mod0)
import Eval
import Types

import qualified Data.Text as T


-- | It verify that the answer provided is verified by itself
main :: IO ()
main = forM_ (subs mod0) $ \submod -> do
  let a = T.unpack $ answer submod
  res <- evalIt $ T.unpack (verify submod)++ " (" ++ a ++ ") (" ++ a ++ " :: Graph Int )"
  either (\e -> die $ "Non Coherent\n" ++ show submod ++ show e) (const $ return ()) res
