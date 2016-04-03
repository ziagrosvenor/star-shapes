module StarShapes where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Debug
import Time exposing (..)
import Window
import Result
import Task exposing (Task, andThen)
import Random
import Text
import Json.Encode 
import Json.Decode exposing ((:=))
import List exposing (..)
import SocketIO exposing (io, defaultOptions, emit, on)
import Enemies exposing (..)
import Input exposing (..)
import Movement exposing (..)
import Config exposing (..)

-- MODEL

type alias PositionData = {
  x: Int,
  y: Int
}

type alias Vec = (Float, Float)

socket = io serverUrl defaultOptions

port response : Task String () 
port response = socket `andThen` on "OPPONENT_UPDATE" responses.address

vecLen : Vec -> Float
vecLen (x, y) = sqrt(x * x + y * y)

vecSub : Vec -> Vec -> Vec
vecSub (ax, ay) (bx, by) = (ax - bx, ay - by)

type alias Model =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , rad: Float
  , color: Color
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
backgroundPos = (areaW, areaH)

hero : Model
hero =
  Model 0 0 0 0 40 heroColor

opponent : Model
opponent  =
  Model 0 0 0 0 40 opponentColor

type alias Game = {
  hero: Model,
  opponent: Model,
  enemiesState: Enemies,
  score: Float
}

game = {
  opponent = opponent,
  hero = hero,
  enemiesState = getEnemies 10 (4, 30) enemiesState,
  score = 0
 }

-- UPDATE

update : Input -> Game -> Game
update {dt, h, o} game =
  game
    |> updateEnemies
    |> newVelocity h o
    |> updatePosition dt
    |> detectCollision

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

updateHeroPos : Float -> Model -> Model
updateHeroPos dt hero =
  let
    {x, y, vx, vy} = hero

    updatedX = if (isAtBorder areaW x) then
      invertPosition dt areaW vx x
    else
      clamp (-areaW/2) (areaW/2) (x + (dt / 15) * vx)

    updatedY = if (isAtBorder areaH y) then
      invertPosition dt areaH vy y
    else
      clamp (-areaH/2) (areaH/2) (y + (dt / 15) * vy)
  in
    {
      hero |
        x = updatedX,
        y = updatedY 
    }

updatePosition : Float -> Game -> Game
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

getListOfCollidingEnemies : Model -> List Enemy -> List Enemy
getListOfCollidingEnemies hero enemies =
  filter (\{x, y, rad} ->
    (rad > hero.rad) && ((vecLen <| vecSub (hero.x, hero.y) (x, y)) < hero.rad + rad)
  ) enemies

isPlayerCollided player enemies =
  length (getListOfCollidingEnemies player enemies) > 0

detectCollision : Game -> Game
detectCollision game =
  let
    {enemiesState, score, hero, opponent} = game
    {enemies} = enemiesState

    enemiesToReturn = filter (\{x, y, rad} ->
      if (hero.rad > rad) then
        ( (vecLen <| vecSub (hero.x, hero.y) (x, y)) > hero.rad + rad  ) &&
        ( (vecLen <| vecSub (opponent.x, opponent.y) (x, y)) > opponent.rad + rad )
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
          opponent = {
            opponent |
              rad = if length enemies == length enemiesToReturn then opponent.rad else opponent.rad + 0.05,
              color = if isCollided then red else opponentColor
          },
          score = toFloat (floor (score + toFloat((length enemies)) - (toFloat(length enemiesToReturn) + (toFloat(length damageAgainstPlayer) / 100)))),
          enemiesState = {
            enemiesState |
              enemies = enemiesToReturn
          }
      }

-- VIEW

view : (Int,Int) -> Game -> Element
view (w,h) {hero, opponent, enemiesState, score} =
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

    background = toForm (image areaW areaH backgroundImageUrl)
    isPlayerCollidedWithEnemy = isPlayerCollided hero enemies

    heroForm = circle hero.rad |> outlined (dottedHeroLine) |> move (hero.x, hero.y)
    heroName = text (Text.style textStyle (Text.fromString "P1")) |> move (hero.x, hero.y)

    opponentForm = circle opponent.rad |> outlined (dottedOpponentLine) |> move (opponent.x, opponent.y)
    opponentName = text (Text.style textStyle (Text.fromString "P2")) |> move (opponent.x, opponent.y)

    enemyForms = map (\{color, rad, x, y} ->
      let
        dottedEnemyLine = {
          dottedHeroLine |
            color = color,
            width = 12
        }
        rock = toForm (image (round rad) (round rad) rockImageUrl)

        enemyForm = if hero.rad > rad then
          -- (square rad |> outlined (solid color) |> move (x, y)) --
          rock |> move (x, y)
        else
          group [
            (circle rad |> outlined dottedEnemyLine |> move (x, y)),
            text (Text.style textStyle (Text.fromString "Alien")) |> move (x, y)
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

encodeKeyboard : PositionData -> String
encodeKeyboard {x, y} =
  Json.Encode.encode 0 <| Json.Encode.object
      [ ("x", Json.Encode.int x),
        ("y", Json.Encode.int y)
      ]

send x = 
  socket `andThen` emit "SELF_UPDATE" x

playerMove : Signal (Task x ())
playerMove = Signal.map (encodeKeyboard>>send) Keyboard.arrows 

port outgoing : Signal (Task a ())
port outgoing = playerMove 

