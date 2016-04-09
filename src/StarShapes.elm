module StarShapes where

import Graphics.Input.Field exposing (Content, noContent)
import Graphics.Element exposing (Element)
import Color exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Task exposing (Task, andThen)
import List exposing (..)
import SocketIO exposing (io, defaultOptions, emit, on)

-- GAME MODULES
import Config exposing (..)
import InitialState exposing (..)
import GameTypes exposing (Model, Enemies, Enemy)
import Inputs exposing (..)
import Outputs exposing (..)
import Views exposing (..)
import Enemies exposing (..)
import Movement exposing (..)

type alias Game = {
  hero: Model,
  opponent: Model,
  enemiesState: Enemies,
  score: Float,
  name: Content,
  view: View
}

gameState = {
  opponent = opponent,
  hero = hero,
  enemiesState = initialEnemies,
  score = 0,
  name = noContent,
  view = PlayerNameForm
 }

main : Signal Element
main =
  Signal.map2 router Window.dimensions 
    (Signal.foldp update gameState input)

update : (Input, Content, Action) -> Game -> Game
update ({dt, h, o}, name, action) game =
  game
    |> updateEnemies
    |> newVelocity h o
    |> updatePosition dt
    |> detectCollision
    |> updateName name 
    |> selectView action

selectView : Action -> Game -> Game
selectView action game =
  case action of
    NoOp -> 
      {
        game |
          view = PlayerNameForm
      }
    SubmitName -> 
      {
        game |
          view = GameView
      }
    Quit -> 
      {
        gameState |
          view = PlayerNameForm,
          name = game.name
      }

port outgoing : Signal (Task a ())
port outgoing = playerMove 

port response : Task String () 
port response = socket `andThen` on "OPPONENT_UPDATE" responses.address

newVelocity : { x:Int, y:Int } -> { x:Int, y:Int } -> Game -> Game
newVelocity {x,y} opp game =
  let
    scale = 6
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

updateName : Content -> Game -> Game
updateName name game =
  {
    game |
      name = name
  }

