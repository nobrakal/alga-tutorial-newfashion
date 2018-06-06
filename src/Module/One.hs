{-# LANGUAGE OverloadedStrings #-}

module Module.One
  (mod1)
where

import Types
import qualified Data.Text as T

mod1 :: Module
mod1 = Module
  {
  name = "The Num instance",
  desc = "Ok we know how to create algebraic graphs, but doing everything with primitives can be long. We will try to add some tools in our bag",
  subs = [s0, s1, s2, s3]
  }

s0 :: SubModule
s0 = SubModule
  {abstract = T.intercalate "\n"
    ["If your vertices are a Num instance, then your Graph is also is a Num instance."
    ,"Here we have:\n"
    ," fromInteger = Vertex\n"
    ,"So in a context of a Graph: 1 == Vertex 1"
    ]
  , instruction = "Try to connect 'Vertex 0' and 'Vertex 1' with this syntax"
  , clue = "TODO"
  , fullAnswer = Answer "Connect 0 1" Comparison "(<=)" []
  , conclusion = "TODO"
  }

s1 :: SubModule
s1 = SubModule
  {abstract = T.intercalate "\n"
    ["If you are a Num instance, then you have '+' and '*' defined"
    ,"Here we have:\n"
    ," (+) = Overlay"
    ," (*) = Connect"
    ]
  , instruction = "Try to connect 'Vertex 0' and 'Vertex 1' with this syntax, and with what we have seen before"
  , clue = "TODO"
  , fullAnswer = Answer "0 * 1" Comparison "(<=)" []
  , conclusion = "TODO"
  }

s2 :: SubModule
s2 = SubModule
  {abstract = T.intercalate "\n"
      ["Your mathematician instincts are awaken, if there is a (+) operator, then there are good chances that there is neutral element n such that:\n"
      , " n + 1 = 1"
      ]
  , instruction = "What can be this neutral element ?"
  , clue = "TODO"
  , fullAnswer = Answer "Empty" GraphInt "(==)" []
  , conclusion = "TODO"
  }

s3 :: SubModule
s3 = SubModule
  {abstract = T.intercalate "\n"
      ["If there is a (*) operator, then there are good chances that there is neutral element n such that:\n"
      , " n * 1 = 1"
      ]
  , instruction = "What can be this neutral element ?"
  , clue = "TODO"
  , fullAnswer = Answer "Empty" GraphInt "(==)" []
  , conclusion = "TODO"
  }
