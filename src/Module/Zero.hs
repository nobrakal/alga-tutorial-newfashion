{-# LANGUAGE OverloadedStrings #-}

module Module.Zero
  (mod0)
where

import Types

mod0 :: Module
mod0 = Module
  {
  desc = "Algebraic graphs ? Again mathematics and computer science mixed ? Follow the tour",
  subs = [s00]
  }

s00 :: SubModule
s00 = SubModule
  {
  abstract = "Imagine you are a skillful haskell developper, and you want to define a graph structure recursively. What is the base case ? An empty graph",
  instructions = "How would you describe an empty graph ?",
  clue = "Maybe simply by writing the thing down",
  answer = Left "Empty"
  }
