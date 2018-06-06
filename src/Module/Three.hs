{-# LANGUAGE OverloadedStrings #-}

module Module.Three
  (mod3)
where

import Types

mod3 :: Module
mod3 = Module
  {
  name = "Useful functions",
  desc = "Ok, we have the basis. Now, how can we reason over graphs ?",
  subs = [s0]
  }

s0 :: SubModule
s0 = SubModule
  {abstract = "Graph are a Functor instance. This means that you can use fmap on graphs. It will apply a function on each vertex"
  , instruction = "You want add 1 to each vertex of a graph \"gr\". How will you do that using fmap ?"
  , clue = "TODO"
  , fullAnswer = Answer "fmap (+1) gr" GraphInt "(==)" ["gr = 1 + 2 * 3 + 5 * 6"]
  , conclusion = "TODO"
  }

