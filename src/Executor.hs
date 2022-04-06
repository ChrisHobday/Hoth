module Executor
  -- (  )
  where

class Executor e where
  -- permissions :: e -> [Permission]
  allowed :: e -> (a -> a) -> Bool
  as      :: e -> (a -> a) -> a -> a

