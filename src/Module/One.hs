{-# LANGUAGE OverloadedStrings #-}

module Module.One
  (mod1)
where

import Types
import qualified Data.Text as T

mod1 :: Module
mod1 = Module
  {
  name = "Basics 2",
  desc = "Ok we know how to create algebraic graphs, but doing everything with primitives can be long. We will try to add some tools in our bag",
  subs = [s10, s11]
  }

s10 :: SubModule
s10 = SubModule
  {abstract = T.intercalate "\n"
    ["If your vertices are a Num instance, then your Graph is also is a Num instance."
    ,"Here we have:\n"
    ," fromInteger = Vertex\n"
    ,"So in a context of a Graph: 1 == Vertex 1"
    ]
  , instruction = "Try to connect 'Vertex 0' and 'Vertex 1' with this syntax"
  , clue = "TODO"
  , fullAnswer = Answer "Connect 0 1" "Graph Int" "(==)"
  , conclusion = "TODO"
  }

s11 :: SubModule
s11 = SubModule
  {abstract = T.intercalate "\n"
    ["If you are a Num instance, then you have '+' and '*' defined"
    ,"Here we have:\n"
    ," (+) = Overlay"
    ," (*) = Connect"
    ]
  , instruction = "Try to connect 'Vertex 0' and 'Vertex 1' with this syntax, and with what we have seen before"
  , clue = "TODO"
  , fullAnswer = Answer "0 * 1" "Graph Int" "(==)"
  , conclusion = "TODO"
  }

