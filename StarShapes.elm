module StarShapes where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Result
import Http
import Task exposing (Task, andThen)
import Random
import Text
import Json.Encode as Encode 
import Json.Decode exposing ((:=))
import List exposing (..)
import SocketIO exposing (io, defaultOptions, emit, on)

-- MODEL

areaW = 960
areaH = 508

responses =
  Signal.mailbox "{x: 0, y: 0}"

type alias PositionData = {
  x: Int,
  y: Int
}

positionData = 
  Json.Decode.object2 PositionData
    ("x" := Json.Decode.int)
    ("y" := Json.Decode.int)

type alias Vec = (Float, Float)

socket = io "http://localhost:3009" defaultOptions
port response : Task String () 
port response = socket `andThen` on "OPPONENT_UPDATE" responses.address

vecLen : Vec -> Float
vecLen (x, y) = sqrt(x * x + y * y)

vecSub : Vec -> Vec -> Vec
vecSub (ax, ay) (bx, by) = (ax - bx, ay - by)

getEnemyColor : Float -> Color
getEnemyColor x =
  if x <= 8 then
    lightGreen
  else if x <= 16 then
    lightBlue
  else if x <= 24 then
    green
  else if x <= 32 then
    blue
  else if x <= 40 then
    lightOrange
  else if x <= 48 then
    lightRed
  else if x <= 56 then
    orange
  else
    red

getRandomX : Random.Seed -> (Float, Random.Seed)
getRandomX seed =
  Random.generate (Random.float -(areaW / 2) (areaW / 2)) seed

getRandomY : Random.Seed -> (Float, Random.Seed)
getRandomY seed =
  Random.generate (Random.float -(areaH / 2) (areaH / 2)) seed

type alias Model =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , rad: Float
  , color: Color
  }

type alias Position = {
  x: Float,
  y: Float
 }

type alias Enemy = {
  model: Model,
  startPos: Position,
  endPos: Position
}

type alias Enemies = {
  enemies: List Enemy,
  seed: Random.Seed
}

textStyle = { typeface = [ "roboto", "sans-serif" ]
            , height   = Just 24
            , color    = white
            , bold     = True
            , italic   = False
            , line = Nothing
          }

heroColor = white
opponentColor = purple

hero : Model
hero =
  Model 0 0 0 0 40 heroColor

opponent : Model
opponent  =
  Model 0 0 0 0 40 opponentColor

enemiesState = {
  enemies = [
    {
      model = Model 50 50 0 0 10 yellow,
      startPos = {
        x = 20,
        y = 20
        },
        endPos = {
          x = 0,
          y = 0
        }
      }
      ],

  seed = Random.initialSeed 20
 }

type alias Game = {
  hero: Model,
  opponent: Model,
  enemiesState: Enemies,
  score: Float,
  seed: Random.Seed,
  backgroundPos: (Float, Float)
}

game = {
  opponent = opponent,
  hero = hero,
  enemiesState = getEnemies 10 (4, 30) enemiesState,
  score = 0,
  seed = Random.initialSeed 3,
  backgroundPos = (areaW, areaH)
 }

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

        nextEnemies = append enemies [
          {
            model = Model randomStartX randomStartY 0 0 randomRad enemyColor,
            startPos = {
              x = randomStartX,
              y = randomStartY
            },
            endPos = {
              x = randomEndX,
              y = randomEndY
            }
          }
        ]

      in
        {
          enemies = nextEnemies,
          seed = fifthSeed
        }

     ) enemiesState (repeat numEnemies ""))

-- UPDATE

update : ( Time, { x : Int, y : Int }, { x : Int, y : Int } ) -> Game -> Game
update (timeDelta, direction, opponentDir) game =
  game
    |> updateEnemies
    |> newVelocity direction opponentDir
    |> updatePosition timeDelta
    |> detectCollision

updateEnemies : Game -> Game
updateEnemies game =
  let
    {enemiesState, hero} = game

    newEnemies = if length enemiesState.enemies >= 5 then
      enemiesState
    else
      (getEnemies 30 (4, hero.rad + 2.5) enemiesState)

    nextEnemiesState = if length enemiesState.enemies >= 5 then
      enemiesState
    else
      newEnemies
  in
    {
      game |
        enemiesState = nextEnemiesState
    }


newVelocity : { x:Int, y:Int } -> { x:Int, y:Int } -> Game -> Game
newVelocity {x,y} opp game =
  let
    scale = 10

    {hero, opponent} = game

    newVel n =
      if x == 0 || y == 0 then
        scale * toFloat n
      else
        scale * toFloat n / sqrt 2
  in
      { game |
          opponent = {
            opponent |
              vx = newVel opp.x,
              vy = newVel opp.y
          },
          hero = {
            hero |
              vx = newVel x,
              vy = newVel y
          }
      }


updateEnemyPos : Time -> Enemy -> Enemy
updateEnemyPos dt {model, startPos, endPos} = 
  let
    enemySpeed = 170
    {x, y, vx, vy, rad, color} = model

    newX = if x == areaW / 2 then
      clamp (-areaW/2) (areaW/2) ((x - (areaW/ 1.01)) + dt * vx)
    else if x == -(areaW / 2) then
      clamp (-areaW/2) (areaW/2) ((x + (areaW/ 1.01)) + dt * vx)
    else if startPos.x < endPos.x then
      clamp (-areaW/2) (areaW/2) (x + ((endPos.x - startPos.x) / enemySpeed) + dt * vx)
    else
      clamp (-areaW/2) (areaW/2) (x + ((startPos.x - endPos.x) / enemySpeed) + dt * vx)


    newY = if y == areaH / 2 then
      clamp (-areaH/2) (areaH/2) ((y - (areaH/ 1.02)) + dt * vy)
    else if y == -(areaH / 2) then
      clamp (-areaH/2) (areaH/2) ((y + (areaH/ 1.02)) + dt * vy)
    else if startPos.y < endPos.y then
      clamp (-areaH/2) (areaH/2) (y + ((endPos.y - startPos.y) / enemySpeed) + dt * vy)
    else
      clamp (-areaH/2) (areaH/2) (y + ((startPos.y - endPos.y) / enemySpeed) + dt * vy)

    newStartPos = if x == areaW / 2 then
      endPos
     else
       startPos

    newEndPos = if x == areaW / 2 then
      startPos
     else
       endPos

  in
    {
      model = Model newX newY vx vy rad color,
      endPos = newEndPos,
      startPos = newStartPos
    }

updateHeroPos : Time -> Model -> Model
updateHeroPos dt hero =
  let
    {x, y, vx, vy} = hero

    updatedX = if x == areaW / 2 then
      clamp (-areaW/2) (areaW/2) ((x - (areaW/ 1.01)) + dt * vx)
    else if x == -(areaW / 2) then
      clamp (-areaW/2) (areaW/2) ((x + (areaW/ 1.01)) + dt * vx)
    else
      clamp (-areaW/2) (areaW/2) (x + dt * vx)

    updatedY = if y == areaH / 2 then
      clamp (-areaH/2) (areaH/2) ((y - (areaH/ 1.02)) + dt * vy)
    else if y == -(areaH / 2) then
      clamp (-areaH/2) (areaH/2) ((y + (areaH/ 1.02)) + dt * vy)
    else
      clamp (-areaH/2) (areaH/2) (y + dt * vy)
  in
    {
      hero |
        x = updatedX,
        y = updatedY 
    }

updatePosition : Time -> Game -> Game
updatePosition dt game =
  let
    {hero, enemiesState, opponent} = game
    updatedEnemies = map (updateEnemyPos dt) enemiesState.enemies
    updatedHero = updateHeroPos dt hero
    updatedOpponent = updateHeroPos dt opponent 

  in
    {
      game |
        hero = updatedHero,
        opponent = updatedOpponent,
        enemiesState = {
          enemiesState |
            enemies = updatedEnemies
        }
    }

getListOfCollidingEnemies hero enemies =
  filter (\{model} ->
    (model.rad > hero.rad) && ((vecLen <| vecSub (hero.x, hero.y) (model.x, model.y)) < hero.rad + model.rad)
  ) enemies

isPlayerCollided player enemies =
  length (getListOfCollidingEnemies player enemies) > 0

detectCollision : Game -> Game
detectCollision game =
  let
    {enemiesState, score, hero} = game
    {enemies} = enemiesState

    enemiesToReturn = filter (\{model} ->
      if (hero.rad > model.rad) then
        (vecLen <| vecSub (hero.x, hero.y) (model.x, model.y)) > hero.rad + model.rad
      else
        True
    ) enemies

    damageAgainstPlayer = getListOfCollidingEnemies hero enemiesToReturn
    isCollided = isPlayerCollided hero enemies 

  in
      { game |
          hero = {
            hero |
              rad = if length enemies == length enemiesToReturn then hero.rad else hero.rad + 0.05,
              color = if isCollided then red else heroColor
          },
          score = toFloat (floor (score + toFloat((length enemies)) - (toFloat(length enemiesToReturn) + (toFloat(length damageAgainstPlayer) / 100)))),
          enemiesState = {
            enemiesState |
              enemies = enemiesToReturn
          }
      }


-- VIEW

view : (Int,Int) -> Game -> Element
view (w,h) {hero, opponent, enemiesState, score, backgroundPos} =
  let
    {enemies} = enemiesState
    heroLineColor = dotted hero.color
    dottedHeroLine = {
      heroLineColor |
        width = 4
    }

    opponentLineColor = dotted opponent.color
    dottedOpponentLine = {
      opponentLineColor |
        width = 4
    }

    (bgX, bgY) = backgroundPos

    background = toForm (image areaW areaH "http://media.indiedb.com/images/articles/1/152/151754/auto/stars.png")
    isPlayerCollidedWithEnemy = isPlayerCollided hero enemies

    heroForm = circle hero.rad |> outlined (dottedHeroLine) |> move (hero.x, hero.y)
    heroName = text (Text.style textStyle (Text.fromString "ME")) |> move (hero.x, hero.y)

    opponentForm = circle opponent.rad |> outlined (dottedOpponentLine) |> move (opponent.x, opponent.y)
    opponentName = text (Text.style textStyle (Text.fromString "P2")) |> move (opponent.x, opponent.y)

    enemyForms = map (\{model} ->
      let
        dottedEnemyLine = {
          dottedHeroLine |
            color = model.color,
            width = 12
        }

        enemyForm = if hero.rad > model.rad then
          (square model.rad |> outlined (solid model.color) |> move (model.x, model.y))
        else
          group [
            (circle model.rad |> outlined dottedEnemyLine |> move (model.x, model.y)),
            text (Text.style textStyle (Text.fromString "Alien")) |> move (model.x, model.y)
          ]

      in
        enemyForm

    ) enemies
    textScore = text (Text.style textStyle (Text.fromString ("Score " ++ toString score))) |> move (65 - areaW/2, 30-areaH/2)

    solidChar = dotted charcoal

    lineStyle = {
       solidChar |
        width = 9
    }

    pathForm = traced (lineStyle) (path [(-140, -140), (-200, 60), (-40, 100)])

  in
    container w h middle <|
    collage areaW areaH
      (concat [[background, pathForm, opponentForm, opponentName, heroForm, heroName, textScore], enemyForms])

-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update game input)

responsesAsObjects = Signal.map (\response -> 
 Result.withDefault {x = 0, y = 0} (Json.Decode.decodeString positionData response)
 ) responses.signal

input : Signal ( Time, { x : Int, y : Int }, { x : Int, y : Int } )
input =
  Signal.sampleOn delta (Signal.map3 (,,) delta Keyboard.arrows responsesAsObjects)

delta : Signal Time
delta =
  Signal.map (\t -> t / 20) (fps 60)

encodeKeyboard : PositionData -> String
encodeKeyboard {x, y} =
  Encode.encode 0 <| Encode.object
      [ ("x", Encode.int x),
        ("y", Encode.int y)
      ]

send x = 
  socket `andThen` emit "SELF_UPDATE" x

playerMove : Signal (Task x ())
playerMove = Signal.map (encodeKeyboard>>send) Keyboard.arrows 

port outgoing : Signal (Task a ())
port outgoing = playerMove 
