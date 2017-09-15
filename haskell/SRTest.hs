{-# LANGUAGE RebindableSyntax #-}

-- Implementation of the calculus lambda-sr-let
--   Polymorphic delimited continuations
--   Asai and Kameyama, APLAS 2007
--   http://logic.cs.tsukuba.ac.jp/~kam/paper/aplas07.pdf
--   hereafter, AK07

-- The prefix example, AK07, Section 2.2
-- The example requires the answer-type polymorphism of the captured
-- continuation.

-- This file also illustrates RebindableSyntax, which lets us
-- use the convenient do-notation

module SRTest where

import ShiftResetGenuine

import Prelude (Int, fromInteger, Show(..), (.))
import qualified Prelude

-- define the standard `monadic' operations
-- to be monadish instead
m >>= f = m >== f
return x = ret x
fail x = Prelude.error x
f =<< m = m >>= f


visit :: [Int] -> C [Int] [[Int]] [Int]
visit [] = shift (\h -> return [])
visit (a:rest) = do
  r <- shift (\k -> do
                    b <- return (k [])
                    c <- reset ((return . k) =<< visit rest)
                    return (b:c))
  return (a:r)


test :: C a a [[Int]]
test = reset (visit [1,2,3])

test_r = run test
-- [[1],[1,2],[1,2,3]]
 

