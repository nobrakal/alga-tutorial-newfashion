{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (forM_)

import Algebra.Graph
import qualified Data.Text as T

import Module.Zero

import Mueval
import Types

main :: IO ()
main = do
  putStrLn "Welcome  in alga-tutorial, this will teach you the basis of algebraic-graphs"
  let modules = [mod0]
  forM_ modules $ \(Module desc subs) -> do
    putStrLn $ T.unpack desc
    forM_ subs $ \(SubModule abstract instructions clue answer) -> do
      putStrLn $ T.unpack abstract
      putStrLn $ T.unpack instructions
      case answer of
        Left answer' -> do
          answerUser <- getLine
          res <- mueval $ "(" ++ answerUser ++ ") === (" ++ T.unpack answer' ++ " :: Graph Int )"
          case res of
            (Right (status,typ,val)) ->
              if val == "True"
                 then putStrLn "Great :D"
                 else putStrLn "Wrong"
            Left e -> error $ show e
        _ -> error "Right ? Right !"

