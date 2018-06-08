{-# LANGUAGE OverloadedStrings #-}

module Module.One
  (mod1)
where

import Types
import qualified Data.Text as T

mod1 :: Module
mod1 = Module
  {
  name = "The __Num__ instance",
  desc = "Ok we know how to create algebraic graphs, but doing everything with primitives can be long. We will try to add some tools in our bag",
  subs = [s0, s1, s2, s3]
  }

s0 :: SubModule
s0 = SubModule
  {abstract = T.intercalate "\n"
    ["If your vertices are a _Num_ instance, then your Graph is also is a _Num_ instance."
    ,"Here we have:\n"
    ,">>> fromInteger = Vertex\n"
    ,"So in a context of a Graph: 1 == Vertex 1"
    ]
  , instruction = "Try to connect 'Vertex 0' and 'Vertex 1' with this syntax"
  , clue = "So like you can certainly drop the \"Vertex\" constructor"
  , fullAnswer = Answer "Connect 0 1" Comparison "(<=)" []
  , conclusion = "Is there other simplifications ?"
  }

s1 :: SubModule
s1 = SubModule
  {abstract = T.intercalate "\n"
    ["If you are a _Num_ instance, then you have '+' and '*' defined"
    ,"Here we have:\n"
    ,">>> (+) = Overlay"
    ,">>> (*) = Connect"
    ]
  , instruction = "Try to connect 'Vertex 0' and 'Vertex 1' with this syntax, and with what we have seen before"
  , clue = "Connect = (+)"
  , fullAnswer = Answer "0 * 1" Comparison "(<=)" []
  , conclusion = "Note that (+) is commutative but not (*)"
  }

s2 :: SubModule
s2 = SubModule
  {abstract = T.intercalate "\n"
      ["Your mathematician instincts are awaken, if there is a (+) operator, then there are good chances that there is neutral element n such that:\n"
      ,">>> n + 1 == 1"
      ]
  , instruction = "What can be this neutral element ?"
  , clue = "Try to find a synonym to \"zero\" in graph theory"
  , fullAnswer = Answer "Empty" GraphInt "(==)" []
  , conclusion = "Let's continue"
  }

s3 :: SubModule
s3 = SubModule
  {abstract = T.intercalate "\n"
      ["If there is a (*) operator, then there are good chances that there is neutral element n such that:\n"
      ,">>> n * 1 = 1"
      ]
  , instruction = "What can be this neutral element ?"
  , clue = "What neutral element have you already seen ?"
  , fullAnswer = Answer "Empty" GraphInt "(==)" []
  , conclusion = "You are certainly a mathematician"
  }
