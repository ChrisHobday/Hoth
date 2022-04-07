module Executor
  where

class Executor e where
  allowed :: e -> (a -> a) -> Bool
  as      :: e -> (a -> a) -> a -> a

