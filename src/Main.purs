module Main where

import Prelude

import Data.Foldable (for_)
import Data.List (List, fold, (:))
import Data.String.CodeUnits (slice)
import Data.Unfoldable (range, replicate)
import Effect (Effect)
import Effect.Class.Console (log)
import Performance.Minibench (benchWith)

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

  log "-------------------- (list) fold + slice --------------------"
  linear \n -> do
    let parts = replicate n "PureScript" :: List String
    slice (n * 10 / 4) (n * 10 / 4 * 3) $ fold parts

  log "-------------------- (array) fold --------------------"
  linear \n -> do
    let parts = replicate n "PureScript" :: Array String
    fold parts

  log "-------------------- (array) fold + slice --------------------"
  linear \n -> do
    let parts = replicate n "PureScript" :: Array String
    slice (n * 10 / 4) (n * 10 / 4 * 3) $ fold parts
