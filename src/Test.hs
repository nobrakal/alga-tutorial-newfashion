{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (forM_)

import System.Exit

import Module.Zero
import Module.One
import Module.Two
import Module.Three
import Module.Four

import Eval
import Types

import qualified Data.Text as T


-- | It verify that the answer provided is verified by itself
main :: IO ()
main = forM_ (subs mod0 ++ subs mod1 ++ subs mod2 ++ subs mod3 ++ subs mod4) $ \submod -> do
  let ans = fullAnswer submod
  res <- evalWithAns ans (T.unpack $ answer ans)
  either (\e -> die $ "Non Coherent\n" ++ show submod ++ show e) (const $ return ()) res
