{-# LANGUAGE OverloadedStrings #-}

module Module.Four
  (mod4)
where

import Types
import qualified Data.Text as T

mod4 :: Module
mod4 = Module
  {
  name = "An example: a network",
  desc = T.intercalate "\n"
    [ "Networks are often modelized through graphs. Let's see how we can handle that case with Alga"
    , "You are going to run a server to link people, previously registered on your super website"
    ],
  subs =
    [
    SubModule
      { abstract = "We start with a list of possible clients \"clients\". You want to turn it into an algebraic graph composed only by single vertices. Each client is represented by an id of type \"Int\""
      , instruction = "How can you do that ?"
      , clue = "It seems you are going to _Fold_ a list"
      , fullAnswer = Answer "foldr (\\x y -> Vertex x + y) Empty clients" GraphInt "(==)" ["clients = [1,2,3,4,5,6,7,8,9]"]
      , conclusion = "The function you used is called _vertices_ in alga"
      }
    ,
    SubModule
      { abstract = "Now you are wanting to verify if everybody is present before running the session. You have a function \"isPresent :: Int -> Bool\" and the graph \"gr\" from the previous question. How can you obtain a \"Bool\" containing the information ?"
      , instruction = "Remember that Graph is an instance of _Functor_ and _Foldable_"
      , clue = "Take a look at the _and_ function from _Foldable_"
      , fullAnswer = Answer "and $ fmap isPresent gr" CanFind "(==)" ["gr = vertices [1,2,3,4,5,6,7,8,9]","isPresent x = (x==1)"]
      , conclusion = "Let's continue"
      }
    ,
    SubModule
      { abstract = "Everyone is ready ? Ok, then you are going to create the graph to modelize the connection. Each participant has provided a list of requested connections. So you have a function \"getConnections :: Int -> [Int]\", and your previous graph \"gr\"."
      , instruction = "Create the desired graph"
      , clue = "Don't forget _foldg_"
      , fullAnswer = Answer "foldg Empty (\\x -> x * vertices (getConnections x)) Overlay Connect gr" GraphInt "(==)" ["gr = vertices [1,2,3,4,5,6,7,8,9]","getConnections x = if x == 1 then [1,2,3] else []"]
      , conclusion = "Let's continue"
      }
    ,
    SubModule
      { abstract = "Oops every, you need to have an _undirected_ graph, if id 1 is connected to id 2 then id 2 is connected to id 1."
      , instruction = "Make it undirected, certainly using _edgeList_ . Don't worry about optimization (i.e. if you duplicate edges)"
      , clue = "For each edge, add the opposite in the graph"
      , fullAnswer = Answer "foldr (\\(x,y) -> Overlay (edge x y)) gr $ edgeList gr" GraphInt "(==)" ["gr = edges [(1,2), (53,25)]"]
      , conclusion = "Yeah the graph can go in production"
      }
    ,
    SubModule
      { abstract = "You just got a phone call, id 25 is bullied by id 53, please stop the connection between them"
      , instruction = "Remove the connection !"
      , clue = "A function must already exist, don't forget that you have two edges"
      , fullAnswer = Answer "removeEdge 25 53 $ removeEdge 53 25 gr" GraphInt "(==)" ["gr = edges [(1,2), (53,25)]"]
      , conclusion = "Let's continue"
      }
    ]
  }

