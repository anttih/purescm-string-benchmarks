module Benchmark.RecordUpdate where

import Prelude

type R = { a :: Int, b :: Int, c :: Int, d :: Int, e :: Int, f :: Int, g :: Int, h :: Int, i :: Int }

test :: Int -> R
test n = go { a: 0, b: 0, c: 0, d: 0, e: 0, f: 0, g: 0, h: 0, i: 0 } 1
  where
    go :: R -> Int -> R
    go acc x = if x < n
               then go (acc { a = x, b = x, c = x, d = x, e = x, f = x, g = x, h = x, i = x }) (x + 1)
               else acc
