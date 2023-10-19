{-# LANGUAGE ImplicitPrelude #-}

module Course.Scratch where

import Data.String (fromString)

helloWorld :: IO ()
helloWorld = print "Hello World!"

f x y = x + y
f :: Int -> Int -> Int

