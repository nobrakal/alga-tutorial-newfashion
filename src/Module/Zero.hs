{-# LANGUAGE OverloadedStrings #-}

module Module.Zero
  (mod0)
where

import Types
import qualified Data.Text as T

mod0 :: Module
mod0 = Module
  {
  name = "Basics",
  desc = "Algebraic graphs ? Again mathematics and computer science mixed ? Follow the tour.",
  subs = [s00, s01, s02, s03, s04]
  }

s00 :: SubModule
s00  = SubModule
  { abstract = "Imagine you are a skillful haskell developper, and you want to define a graph structure recursively. What is the base case ? An _Empty_ graph."
  , instruction = "How would you describe an empty graph ?"
  , clue = "Maybe simply by writing the thing down"
  , fullAnswer = Answer "Empty" Str "on (<=) (length . words)" []
  , conclusion = "You entered a single word, in Haskell this is likely a constant function. In Alga, the empty graph is simply denoted by the _Empty_ constructor"
  }

s01 :: SubModule
s01  = SubModule
  { abstract = "Ok then come an other base case: a single vertex"
  , instruction = "How would you describe a single _Vertex_ identified by \"0\" ?"
  , clue = "Maybe simply by writing the thing down"
  , fullAnswer = Answer "Vertex 0" Str "(\\y _ -> (length (words y) == 2) && isDigit (last $ last $ words y))" []
  , conclusion = "You entered a single word followed by a number. In Alga, the vertex constructor is called _Vertex_"
  }

s02 :: SubModule
s02  = SubModule
  { abstract = "Next, you are wanting to put several vertices together, lets say by overlaying them"
  , instruction = "How would you describe a graph that _Overlay_ a single vertex 0 and a single vertex 1 ?"
  , clue = "Maybe simply by writing the thing down"
  , fullAnswer = Answer "Overlay (Vertex 0) (Vertex 1)" (CustomComp (\x -> unwords $ "Overlay" : tail (words x))) "(\\x _ -> (length (words x) > 1))" []
  , conclusion = "You entered a single function for overlaying two vertices. In Alga, this function is named _Overlay_ . Note that _Overlay_ is _commutative_ : Overlay (Vertex 0) (Vertex 1) == Overlay (Vertex 1) (Vertex 0)"
  }

s03 :: SubModule
s03  = SubModule
  { abstract = "Now you are wanting to connect them by an edge. Algebraic graph represents __directed__ __graphs__ , so an edge from 0 to 1 is not the same than an edge from 1 to 0"
  , instruction = "How would you describe a graph that _Connect_ a single vertex 0 to a single vertex 1 ?"
  , clue = "Maybe simply by writing the thing down"
  , fullAnswer = Answer "Connect (Vertex 0) (Vertex 1)" (CustomComp (\x -> unwords $ "Connect" : tail (words x))) "(\\x _ -> (length (words x) > 1))" []
  , conclusion = "You entered a single function for connecting two vertices. In Alga, this function is named _Connect_ .\nWell you have found the definition of the Graph in Alga: \n\ndata Graph a = Empty\n" `T.append` T.unlines (map (T.append (T.replicate 13 (T.singleton ' ')))
        [ "| Vertex a"
        , "| Overlay (Graph a) (Graph a)"
        , "| Connect (Graph a) (Graph a)"
        ])
  }

s04 :: SubModule
s04 = SubModule
  { abstract = "Is that all ? Yes, almsot. As you noticed, you can _Overlay_ and _Connect_ any graph. Connecting two graphs correspond to add an edge between each vertex from the left side to the right one."
  , instruction = "For example, try to find a condensed representation the graph composed by the following edges: (0,1), (0,2), (1,2)"
  , clue = "Use Connect wisely, you don't need to overlay anything"
  , fullAnswer = Answer "Connect (Vertex 0) (Connect (Vertex 1) (Vertex 2))" GraphInt "(===)" []
  , conclusion = "You know alga's basics :)"
  }

