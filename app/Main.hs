{-# LANGUAGE TemplateHaskell #-}
module Main where

import Executor
import User

import Language.Haskell.TH

main :: IO ()
main = do
  print 'name
