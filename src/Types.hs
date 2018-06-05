module Types where

import qualified Data.Text as T

import Data.Function (on)

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
  answer :: T.Text,
  verify :: T.Text, -- A function to verify the input (likely "==")
  conclusion :: T.Text
} deriving (Show)

instance Eq SubModule where
  (==) = on (==) abstract
