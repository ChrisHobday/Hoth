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

  snAdd1Mult1 <- makeStableName ((+1) . (*1))
  snAdd1Mult1' <- makeStableName ((+1) . (*1))

  snAdd <- makeStableName (+)
  snAdd' <- makeStableName (+)

  snWrappedAdd <- makeStableName add
  snWrappedAdd' <- makeStableName add

  snMult <- makeStableName (*)
  snMult' <- makeStableName (*)

  snWrappedMult <- makeStableName mult
  snWrappedMult' <- makeStableName mult

  snRename <- makeStableName rename
  snRename' <- makeStableName rename

  snRenameBob <- makeStableName (rename "Bob")
  snRenameBob' <- makeStableName (rename "Bob")

  snRenameJim <- makeStableName (rename "Jim")
  snRenameJim' <- makeStableName (rename "Jim")

  snUpdateIdx <- makeStableName updateIdx
  snUpdateIdx' <- makeStableName updateIdx

  snUpdateIdx100 <- makeStableName (updateIdx 100)
  snUpdateIdx100' <- makeStableName (updateIdx 100)

  -- Check composed functions
  snRenameBobUpdateIdx100 <- makeStableName (rename "Bob" . updateIdx 100)
  snRenameBobUpdateIdx100' <- makeStableName (rename "Bob" . updateIdx 100)


  -- Test
  putStrLn "Testing..."

  putStrLn "Expected True..."

  putStrLn $ "1 == 1                                             " ++ show (eqStableName sn1 sn1')
  putStrLn $ "one == one (one = 1)                               " ++ show (eqStableName snOne snOne')
  putStrLn $ "1 == one (one = 1)                                 " ++ show (eqStableName sn1 snOne)

  putStrLn $ "(+1) == (+1)                                       " ++ show (eqStableName snInc snInc')
  putStrLn $ "inc == inc                                         " ++ show (eqStableName snWrappedInc snWrappedInc')
  putStrLn $ "(+1) == inc (inc a = a + 1)                        " ++ show (eqStableName snInc snWrappedInc)

  putStrLn $ "(+2) == (+2)                                       " ++ show (eqStableName snInc2 snInc2')
  putStrLn $ "inc2 == inc2 (inc2 a = a + 2)                      " ++ show (eqStableName snWrappedInc2 snWrappedInc2')
  putStrLn $ "(+2) == inc2 (inc2 a = a + 2)                      " ++ show (eqStableName snInc2 snWrappedInc2)

  putStrLn $ "(*1) == (*1)                                       " ++ show (eqStableName snMult1 snMult1')
  putStrLn $ "mult1 == mult1 (mult1 a = a * 1)                   " ++ show (eqStableName snWrappedMult1 snWrappedMult1')
  putStrLn $ "(*1) == mult1 (mult1 a = a * 1)                    " ++ show (eqStableName snMult1 snWrappedMult1)

  putStrLn $ "(*2) == (*2)                                       " ++ show (eqStableName snMult2 snMult2')
  putStrLn $ "mult2 == mult2 (mult2 a = a * 2)                   " ++ show (eqStableName snWrappedMult2 snWrappedMult2')
  putStrLn $ "(*2) == mult2 (mult2 a = a * 2)                    " ++ show (eqStableName snMult2 snWrappedMult2)

  putStrLn $ "(+) == (+)                                         " ++ show (eqStableName snAdd snAdd')
  putStrLn $ "add == add (add a b == a + b)                      " ++ show (eqStableName snWrappedAdd snWrappedAdd')
  putStrLn $ "(+) == add (add a b == a + b)                      " ++ show (eqStableName snAdd snWrappedAdd)

  putStrLn $ "(*) == (*)                                         " ++ show (eqStableName snMult snMult')
  putStrLn $ "mult == mult (mult a b == a * b)                   " ++ show (eqStableName snWrappedMult snWrappedMult')
  putStrLn $ "(*) == mult (mult a b == a * b)                    " ++ show (eqStableName snMult snWrappedMult)

  putStrLn $ "rename == rename                                   " ++ show (eqStableName snRename snRename')
  putStrLn $ "rename 'Bob' == rename 'Bob'                       " ++ show (eqStableName snRenameBob snRenameBob')
  putStrLn $ "rename 'Jim' == rename 'Jim'                       " ++ show (eqStableName snRenameJim snRenameJim')

  putStrLn $ "updateIdx == updateIdx                             " ++ show (eqStableName snUpdateIdx snUpdateIdx')
  putStrLn $ "updateIdx 100 == updateIdx 100                     " ++ show (eqStableName snUpdateIdx100 snUpdateIdx100')
  putStrLn $ "snRenameBobUpdateIdx100 == snRenameBobUpdateIdx100 " ++ show (eqStableName snRenameBobUpdateIdx100 snRenameBobUpdateIdx100')
  putStrLn $ "((+1) . (*1)) == ((+1) . (*1))                     " ++ show (eqStableName snAdd1Mult1 snAdd1Mult1')

  putStrLn "Expected False..."

  putStrLn $ "1 == (+1)                                          " ++ show (eqStableName sn1 snInc)
  putStrLn $ "(+) == (*)                                         " ++ show (eqStableName snAdd snMult)
  putStrLn $ "(+) == mult (mult a b == a * b)                    " ++ show (eqStableName snAdd snMult)
  putStrLn $ "(+1) == (+2)                                       " ++ show (eqStableName snInc snInc2)
  putStrLn $ "(+1) == (*1)                                       " ++ show (eqStableName snInc snMult1)
  putStrLn $ "rename == rename 'Bob'                             " ++ show (eqStableName snRename snRenameBob)
  putStrLn $ "rename 'Bob' == rename 'Jim'                       " ++ show (eqStableName snRenameBob snRenameJim)
  putStrLn $ "rename == updateIdx                                " ++ show (eqStableName snRename snUpdateIdx)
  putStrLn $ "rename 'Bob' == updateIdx 100                      " ++ show (eqStableName snRenameBob snUpdateIdx100)
  putStrLn $ "rename 'Bob' == snRenameBobUpdateIdx100            " ++ show (eqStableName snRenameBob snUpdateIdx100)
  putStrLn $ "updateIdx 100 == snRenameBobUpdateIdx100           " ++ show (eqStableName snRenameBob snUpdateIdx100)

  let x = 10 + 20

  -- Retest
  putStrLn "Retesting..."

  putStrLn "Expected True..."

  putStrLn $ "1 == 1                                             " ++ show (eqStableName sn1 sn1')
  putStrLn $ "one == one (one = 1)                               " ++ show (eqStableName snOne snOne')
  putStrLn $ "1 == one (one = 1)                                 " ++ show (eqStableName sn1 snOne)

  putStrLn $ "(+1) == (+1)                                       " ++ show (eqStableName snInc snInc')
  putStrLn $ "inc == inc                                         " ++ show (eqStableName snWrappedInc snWrappedInc')
  putStrLn $ "(+1) == inc (inc a = a + 1)                        " ++ show (eqStableName snInc snWrappedInc)

  putStrLn $ "(+2) == (+2)                                       " ++ show (eqStableName snInc2 snInc2')
  putStrLn $ "inc2 == inc2 (inc2 a = a + 2)                      " ++ show (eqStableName snWrappedInc2 snWrappedInc2')
  putStrLn $ "(+2) == inc2 (inc2 a = a + 2)                      " ++ show (eqStableName snInc2 snWrappedInc2)

  putStrLn $ "(*1) == (*1)                                       " ++ show (eqStableName snMult1 snMult1')
  putStrLn $ "mult1 == mult1 (mult1 a = a * 1)                   " ++ show (eqStableName snWrappedMult1 snWrappedMult1')
  putStrLn $ "(*1) == mult1 (mult1 a = a * 1)                    " ++ show (eqStableName snMult1 snWrappedMult1)

  putStrLn $ "(*2) == (*2)                                       " ++ show (eqStableName snMult2 snMult2')
  putStrLn $ "mult2 == mult2 (mult2 a = a * 2)                   " ++ show (eqStableName snWrappedMult2 snWrappedMult2')
  putStrLn $ "(*2) == mult2 (mult2 a = a * 2)                    " ++ show (eqStableName snMult2 snWrappedMult2)

  putStrLn $ "(+) == (+)                                         " ++ show (eqStableName snAdd snAdd')
  putStrLn $ "add == add (add a b == a + b)                      " ++ show (eqStableName snWrappedAdd snWrappedAdd')
  putStrLn $ "(+) == add (add a b == a + b)                      " ++ show (eqStableName snAdd snWrappedAdd)

  putStrLn $ "(*) == (*)                                         " ++ show (eqStableName snMult snMult')
  putStrLn $ "mult == mult (mult a b == a * b)                   " ++ show (eqStableName snWrappedMult snWrappedMult')
  putStrLn $ "(*) == mult (mult a b == a * b)                    " ++ show (eqStableName snMult snWrappedMult)

  putStrLn $ "rename == rename                                   " ++ show (eqStableName snRename snRename')
  putStrLn $ "rename 'Bob' == rename 'Bob'                       " ++ show (eqStableName snRenameBob snRenameBob')
  putStrLn $ "rename 'Jim' == rename 'Jim'                       " ++ show (eqStableName snRenameJim snRenameJim')

  putStrLn $ "updateIdx == updateIdx                             " ++ show (eqStableName snUpdateIdx snUpdateIdx')
  putStrLn $ "updateIdx 100 == updateIdx 100                     " ++ show (eqStableName snUpdateIdx100 snUpdateIdx100')
  putStrLn $ "snRenameBobUpdateIdx100 == snRenameBobUpdateIdx100 " ++ show (eqStableName snRenameBobUpdateIdx100 snRenameBobUpdateIdx100')
  putStrLn $ "((+1) . (*1)) == ((+1) . (*1))                     " ++ show (eqStableName snAdd1Mult1 snAdd1Mult1')

  putStrLn "Expected False..."

  putStrLn $ "1 == (+1)                                          " ++ show (eqStableName sn1 snInc)
  putStrLn $ "(+) == (*)                                         " ++ show (eqStableName snAdd snMult)
  putStrLn $ "(+) == mult (mult a b == a * b)                    " ++ show (eqStableName snAdd snMult)
  putStrLn $ "(+1) == (+2)                                       " ++ show (eqStableName snInc snInc2)
  putStrLn $ "(+1) == (*1)                                       " ++ show (eqStableName snInc snMult1)
  putStrLn $ "rename == rename 'Bob'                             " ++ show (eqStableName snRename snRenameBob)
  putStrLn $ "rename 'Bob' == rename 'Jim'                       " ++ show (eqStableName snRenameBob snRenameJim)
  putStrLn $ "rename == updateIdx                                " ++ show (eqStableName snRename snUpdateIdx)
  putStrLn $ "rename 'Bob' == updateIdx 100                      " ++ show (eqStableName snRenameBob snUpdateIdx100)
  putStrLn $ "rename 'Bob' == snRenameBobUpdateIdx100            " ++ show (eqStableName snRenameBob snUpdateIdx100)
  putStrLn $ "updateIdx 100 == snRenameBobUpdateIdx100           " ++ show (eqStableName snRenameBob snUpdateIdx100)