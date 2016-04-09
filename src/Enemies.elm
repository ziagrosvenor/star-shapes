module Enemies (..) where 
import Random
import Time exposing (Time)
import List exposing (..)
import Color exposing (..)
import Movement exposing (..)
import Config exposing (..)
import GameTypes exposing (Enemy, Enemies, Model)

enemiesState = {
  enemies = [
    Enemy 50 50 {x = 1, y = 1} {x = 1, y = 1} 10 yellow
  ],

  seed = Random.initialSeed 20
 }

initialEnemies = getEnemies 10 (4, 30) enemiesState

getRandomX : Random.Seed -> (Float, Random.Seed)
getRandomX seed =
  Random.generate (Random.float -(areaW / 2) (areaW / 2)) seed

getRandomY : Random.Seed -> (Float, Random.Seed)
getRandomY seed =
  Random.generate (Random.float -(areaH / 2) (areaH / 2)) seed

getEnemyColor : Float -> Color
getEnemyColor x =
  if x <= 48 then
    lightRed
  else if x <= 56 then
    orange
  else
    red

getEnemies : Int -> (Float, Float) -> Enemies -> Enemies
getEnemies numEnemies size enemiesState =
    (foldl (\x {seed, enemies} ->
      let
        (randomStartX, firstSeed) = getRandomX seed
        (randomStartY, secondSeed) = getRandomY firstSeed
        (randomEndX, thirdSeed) = getRandomX secondSeed
        (randomEndY, forthSeed) = getRandomY thirdSeed

        (minSize, maxSize) = size
        (randomRad, fifthSeed) = Random.generate (Random.float minSize maxSize) forthSeed

        enemyColor = getEnemyColor randomRad

        startPos = {
          x = randomStartX,
          y = randomStartY
        }

        endPos = {
          x = randomEndX,
          y = randomEndY
        }

        nextEnemies = append enemies [
          Enemy randomStartX randomStartY startPos endPos randomRad enemyColor
        ]

      in
        {
          enemies = nextEnemies,
          seed = fifthSeed
        }
     ) enemiesState (repeat numEnemies ""))

updateEnemyPos : Time -> Enemy -> Enemy
updateEnemyPos dt {x, y, rad, color, startPos, endPos} = 
  let
    newX = if (isAtBorder areaW x) then
      invertPosition dt areaW 0 x
    else
      moveItem enemySpeed dt areaW (endPos.x, startPos.x) 0 x

    newY = if (isAtBorder areaH y) then
      invertPosition dt areaH 0 y
    else
      moveItem enemySpeed dt areaH (endPos.y, startPos.x) 0 y

    newStartPos = if x == areaW / 2 then
      endPos
     else
       startPos

    newEndPos = if x == areaW / 2 then
      startPos
     else
       endPos
  in
    Enemy newX newY newStartPos newEndPos rad color

getListOfCollidingEnemies : Model -> List Enemy -> List Enemy
getListOfCollidingEnemies hero enemies =
  filter (\{x, y, rad} ->
    (rad > hero.rad) && ((vecLen <| vecSub (hero.x, hero.y) (x, y)) < hero.rad + rad)
  ) enemies
