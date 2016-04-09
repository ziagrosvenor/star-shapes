module GameTypes (..) where

import Color exposing (Color)
import Random
import Graphics.Input.Field exposing (Content)

type alias Model =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , rad: Float
  , color: Color
}

type alias Enemy = {
    x: Float
  , y : Float
  , startPos: Position
  , endPos: Position
  , rad: Float
  , color: Color
}

type alias Enemies = {
  enemies: List Enemy,
  seed: Random.Seed
}

type alias Position = {
  x: Float,
  y: Float
}
