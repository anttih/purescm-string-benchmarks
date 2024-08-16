module Main where

import Prelude

import Data.Array.NonEmpty as NonEmpty
import Data.Foldable (for_)
import Data.List (List, fold, (:))
import Data.String (Pattern(..))
import Data.String.CodeUnits (slice)
import Data.String.CodeUnits as SCU
import Data.String.Regex (match, test) as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe (unsafeRegex) as Regex
import Data.Unfoldable (range, replicate)
import Effect (Effect)
import Effect.Class.Console (log)
import Performance.Minibench (benchWith', withUnits)
import Test.Assert (assert)

import Benchmark.Fibonacci as Fibonacci
import Benchmark.FibonacciTco as FibonacciTco
import Benchmark.Ackermann as Ackermann
import Benchmark.AckermannCps as AckermannCps
import Benchmark.RecordUpdate as RecordUpdate
import Benchmark.ListInsert as ListInsert

main :: Effect Unit
main = do
  let partsL = replicate 1000 "PureScript" :: List String
  bench "(list) fold" 1000 \_ -> fold partsL
  bench "(list) fold + slice" 1000 \_ -> slice 1000 1500 $ fold partsL

  let parts' = replicate 1000 "PureScript" :: Array String
  bench "(array) fold" 1000 \_ -> fold parts'
  bench "(array) fold + slice" 1000 \_ -> slice 1000 1500 $ fold parts'

  bench "regex test" 100 \_ -> Regex.test (Regex.unsafeRegex "foo ([a-z]+) baz" RegexFlags.noFlags) "foo bar baz"

  let re = Regex.unsafeRegex "foo ([a-z]+) baz" RegexFlags.noFlags
  assert $ Regex.test re "foo bar baz"
  bench "regex test (precompiled)" 100 \_ -> Regex.test re "foo bar baz"

  let re2 = Regex.unsafeRegex "foo" RegexFlags.global
  let input = fold (replicate 100000 "foo" :: List String)
  bench "regex match" 100 \_ -> NonEmpty.length <$> Regex.match re2 input

  bench "CodeUnits.indexOf'" 100 \_ -> SCU.indexOf' (Pattern "foo") 50000 input

  bench "Fibonacci  5" 1000 (\_ -> Fibonacci.test 5)
  bench "Fibonacci 10" 1000 (\_ -> Fibonacci.test 10)
  bench "Fibonacci 15" 1000 (\_ -> Fibonacci.test 15)
  bench "Fibonacci 20" 1000 (\_ -> Fibonacci.test 20)

  -- Fibonacci TCO --
  bench "FibonacciTco  5" 1000 (\_ -> FibonacciTco.test 5)
  bench "FibonacciTco 10" 1000 (\_ -> FibonacciTco.test 10)
  bench "FibonacciTco 15" 1000 (\_ -> FibonacciTco.test 15)
  bench "FibonacciTco 20" 1000 (\_ -> FibonacciTco.test 20)

  -- Ackermann --
  bench "Ackermann 2 3" 1000 (\_ -> Ackermann.test 2 3)
  bench "Ackermann 2 4" 1000 (\_ -> Ackermann.test 2 4)
  bench "Ackermann 3 1" 1000 (\_ -> Ackermann.test 3 1)
  bench "Ackermann 3 2" 1000 (\_ -> Ackermann.test 3 2)

  -- Ackermann CPS --
  bench "AckermannCps 2 3" 1000 (\_ -> AckermannCps.test 2 3)
  bench "AckermannCps 2 4" 1000 (\_ -> AckermannCps.test 2 4)
  bench "AckermannCps 3 1" 1000 (\_ -> AckermannCps.test 3 1)
  bench "AckermannCps 3 2" 1000 (\_ -> AckermannCps.test 3 2)

  -- Record update --
  bench "RecordUpdate    1000" 1000 (\_ -> RecordUpdate.test 1000)
  bench "RecordUpdate   10000" 1000 (\_ -> RecordUpdate.test 10000)
  bench "RecordUpdate  100000" 1000 (\_ -> RecordUpdate.test 100000)
  bench "RecordUpdate 1000000" 100 (\_ -> RecordUpdate.test 1000000)

  -- List insert --
  bench "ListInsert    1000" 1000 (\_ -> ListInsert.test 1000)
  bench "ListInsert   10000" 1000 (\_ -> ListInsert.test 10000)
  bench "ListInsert  100000" 1000 (\_ -> ListInsert.test 100000)
  bench "ListInsert 1000000" 100 (\_ -> ListInsert.test 1000000)

bench
  :: forall a
   . String
  -> Int
  -> (Unit -> a)
  -> Effect Unit
bench label n f = do
  res <- benchWith' n f
  log $ label <> ": { "
    <> "mean = "   <> withUnits res.mean   <> " | "
    <> "stddev = " <> withUnits res.stdDev <> " | "
    <> "min = "    <> withUnits res.min    <> " | "
    <> "max = "    <> withUnits res.max
    <> " }"

linear :: forall a. String -> (Int -> a) -> Effect Unit
linear label f = do
  let ns = 1 : ((_ * 100) <$> range 1 10)
  for_ ns \n -> do
    bench (label <> " n = " <> show n) 50 \_ -> f n

