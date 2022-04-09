{-# LANGUAGE TemplateHaskell #-}
module Main where

import Executor
import User

import Language.Haskell.TH
import System.Mem.StableName
import Data.Universe.Instances.Eq

one = 1

inc a = a + 1

inc2 a = a + 2

mult1 a = a * 1

mult2 a = a * 2

add a b = a + b

mult a b = a * b

main :: IO ()
main = do
  putStrLn "StableName Testing..."

  sn1 <- makeStableName 1
  sn1' <- makeStableName 1

  snOne <- makeStableName one
  snOne' <- makeStableName one

  snInc <- makeStableName (+1)
  snInc' <- makeStableName (+1)

  snWrappedInc <- makeStableName inc
  snWrappedInc' <- makeStableName inc

  snWrappedInc2 <- makeStableName inc2
  snWrappedInc2' <- makeStableName inc2

  snInc2 <- makeStableName (+2)
  snInc2' <- makeStableName (+2)

  snMult1 <- makeStableName (*1)
  snMult1' <- makeStableName (*1)

  snWrappedMult1 <- makeStableName mult1
  snWrappedMult1' <- makeStableName mult1

  snMult2 <- makeStableName (*2)
  snMult2' <- makeStableName (*2)

  snWrappedMult2 <- makeStableName mult2
  snWrappedMult2' <- makeStableName mult2

  snAdd <- makeStableName (+)
  snAdd' <- makeStableName (+)

  snWrappedAdd <- makeStableName add
  snWrappedAdd' <- makeStableName add

  snMult <- makeStableName (*)
  snMult' <- makeStableName (*)

  snWrappedMult <- makeStableName mult
  snWrappedMult' <- makeStableName mult

  --Test

  putStrLn $ "1 == 1 " ++ show (sn1 == sn1')
  putStrLn $ "one == one (one = 1) " ++ show (snOne == snOne')
  putStrLn $ "1 == one (one = 1) " ++ show (sn1 == snOne)

  putStrLn $ "(+1) == (+1) " ++ show (snInc == snInc')
  putStrLn $ "inc == inc " ++ show (snWrappedInc == snWrappedInc')
  putStrLn $ "(+1) == inc (inc a = a + 1) " ++ show (snInc == snWrappedInc)

  putStrLn $ "(+2) == (+2) " ++ show (snInc2 == snInc2')
  putStrLn $ "inc2 == inc2 (inc2 a = a + 2) " ++ show (snWrappedInc2 == snWrappedInc2')
  putStrLn $ "(+2) == inc2 (inc2 a = a + 2) " ++ show (snInc2 == snWrappedInc2)

  putStrLn $ "(*1) == (*1) " ++ show (snMult1 == snMult1')
  putStrLn $ "mult1 == mult1 (mult1 a = a * 1) " ++ show (snWrappedMult1 == snWrappedMult1')
  putStrLn $ "(*1) == mult1 (mult1 a = a * 1) " ++ show (snWrappedMult1 == snWrappedMult1')


  putStrLn $ "(+) == (+) " ++ show (snAdd == snAdd')
  putStrLn $ "add == add (add a b == a + b) " ++ show (snWrappedAdd == snWrappedAdd')
  putStrLn $ "(+) == add (add a b == a + b) " ++ show (snAdd == snWrappedAdd)

  -- False tests
  -- putStrLn $ "1 == (+1) " ++ show (sn1 == snInc)

  -- Retest after a while
  putStrLn $ "1 == 1 " ++ show (sn1 == sn1')
  putStrLn $ "one == one (one = 1) " ++ show (snOne == snOne')
  putStrLn $ "1 == one (one = 1) " ++ show (sn1 == snOne)
  putStrLn $ "(+1) == (+1) " ++ show (snInc == snInc')
  putStrLn $ "inc == inc " ++ show (snWrappedInc == snWrappedInc')
  putStrLn $ "(+1) == inc (inc a = a + 1) " ++ show (snInc == snWrappedInc)
  putStrLn $ "(+2) == (+2) " ++ show (snInc2 == snInc2')
  putStrLn $ "inc2 == inc2 (inc2 a = a + 2) " ++ show (snWrappedInc2 == snWrappedInc2')
  putStrLn $ "(+2) == inc2 (inc2 a = a + 2) " ++ show (snInc2 == snWrappedInc2)
  putStrLn $ "(+) == (+) " ++ show (snAdd == snAdd')
  putStrLn $ "add == add (add a b == a + b) " ++ show (snWrappedAdd == snWrappedAdd')
  putStrLn $ "(+) == add (add a b == a + b) " ++ show (snAdd == snWrappedAdd)