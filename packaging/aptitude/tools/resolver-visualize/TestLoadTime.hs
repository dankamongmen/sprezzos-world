#!/usr/bin/env runhaskell

module Main where

import Control.Monad
import Data.IORef
import Resolver.Log
import System.Environment
import System.IO

progress :: IORef Integer -> Integer -> Integer -> IO ()
progress ref cur max =
    do lastPercent <- readIORef ref
       (if cur >= (max * (lastPercent + 10) `div` 100)
        then do let newPercent = lastPercent + 10
                print newPercent
                writeIORef ref newPercent
        else return ())

main = do args <- getArgs
          when (length args > 1) (error "Too many arguments: expected exactly one.")
          when (length args < 1) (error "Too few arguments: expected exactly one.")
          let [fn] = args
          h <- openFile fn ReadMode
          ref <- newIORef 0
          logFile <- loadLogFile h fn (progress ref)
          return ()
