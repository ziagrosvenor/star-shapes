module Movement (..) where

invertPosition : Float -> Float -> Float -> Float -> Float
invertPosition dt axis velocity position =
  let
    newPosition = if position == axis / 2 then
      position - (axis / 1.01)
    else
      position + (axis / 1.01) 
  in
    clamp (-axis/2) (axis/2) (newPosition + dt * velocity)

isAtBorder : Float -> Float -> Bool
isAtBorder axis position =
  position == axis / 2 || position == -(axis /2)

moveItem : Float -> Float -> Float -> (Float, Float) -> Float -> Float -> Float
moveItem speed dt axis (startPos, endPos) velocity position =
  let
    increment = if startPos < endPos then
      (endPos - startPos)
    else
      (startPos - endPos)
  in
    clamp (-axis/2) (axis/2) (position + (increment / speed) + dt * velocity)
