module Types where

import qualified Data.Text as T

data Module = Module {
  desc :: T.Text,
  subs :: [SubModule]
} deriving (Show)

data SubModule = SubModule {
  abstract :: T.Text,
  instruction :: T.Text,
  clue :: T.Text,
  answer :: T.Text,
  verify :: T.Text, -- A function to verify the input (likely "==")
  conclusion :: T.Text
} deriving (Show)
