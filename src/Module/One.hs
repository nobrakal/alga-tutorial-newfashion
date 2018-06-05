{-# LANGUAGE OverloadedStrings #-}

module Module.One
  (mod1)
where

import Types

mod1 :: Module
mod1 = Module
  {
  desc = "Ok we know how to create algebraic graphs, but doing everything with primitives can be long. We will try to add some tool in our bags",
  subs = [s10]
  }

s10 :: SubModule
s10 = SubModule
  {
  abstract = "For example, we want something like \" overlays :: [Graph a] -> Graph a\". How can this be done ?",
  instruction = "Write overlays",
  clue = "TODO",
  answer = Right "overlays = foldr Overlay Empty",
  conclusion = "TODO"
  }
