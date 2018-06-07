module Types where

import qualified Data.Text as T

import Data.Function (on)

-- | Modules Text are rendered using @render@ from Main.hs

data Module = Module {
  name :: T.Text,
  desc :: T.Text,
  subs :: [SubModule]
} deriving (Show)

instance Eq Module where
  (==) = on (==) name

data SubModule = SubModule {
  abstract :: T.Text,
  instruction :: T.Text,
  clue :: T.Text,
  fullAnswer :: Answer,
  conclusion :: T.Text
} deriving (Show)

data Answer = Answer {
  answer :: T.Text,
  typeOf :: TypeOf,
  verify :: T.Text, -- A function to verify the input (likely "==")
  decl   :: [T.Text] -- A list of declarations, used in the answer
} deriving (Show)

data TypeOf = GraphInt
            | Str
            | CanFind
            | Comparison
            deriving (Show)

instance Eq SubModule where
  (==) = on (==) abstract
