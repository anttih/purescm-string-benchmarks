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
import Performance.Minibench (benchWith)
import Test.Assert (assert)

linear :: forall a. (Int -> a) -> Effect Unit
linear f = do
  let ns = 1 : ((_ * 100) <$> range 1 10)
  for_ ns \n -> do
    log $ show n <> ":"
    benchWith 50 \_ -> f n
    log ""

main :: Effect Unit
main = do
  log "-------------------- (list) fold --------------------"
  linear \n -> do
    let parts = replicate n "PureScript" :: List String
    fold parts

  log "-------------------- (list) fold + slice ------------"
  linear \n -> do
    let parts = replicate n "PureScript" :: List String
    slice (n * 10 / 4) (n * 10 / 4 * 3) $ fold parts

  log "-------------------- (array) fold -------------------"
  linear \n -> do
    let parts = replicate n "PureScript" :: Array String
    fold parts

  log "-------------------- (array) fold + slice -----------"
  linear \n -> do
    let parts = replicate n "PureScript" :: Array String
    slice (n * 10 / 4) (n * 10 / 4 * 3) $ fold parts

  log "-------------------- regex test --------------------"
  benchWith 100 \_ -> Regex.test (Regex.unsafeRegex "foo ([a-z]+) baz" RegexFlags.noFlags) "foo bar baz"

  log "-------------------- regex test (precompiled) ------"
  let re = Regex.unsafeRegex "foo ([a-z]+) baz" RegexFlags.noFlags
  assert $ Regex.test re "foo bar baz"
  benchWith 100 \_ -> Regex.test re "foo bar baz"

  log "-------------------- regex match -------------------"
  let re2 = Regex.unsafeRegex "foo" RegexFlags.global
  let input = fold (replicate 100000 "foo" :: List String)
  benchWith 100 \_ -> NonEmpty.length <$> Regex.match re2 input

  log "-------------------- CodeUnits.indexOf' ------------"
  benchWith 100 \_ -> SCU.indexOf' (Pattern "foo") 50000 input
