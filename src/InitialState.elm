module InitialState (..) where

import Enemies exposing (getEnemies)
import GameTypes exposing (Enemy, Model)
import Color exposing (..)
import Random

heroColor = white
opponentColor = purple

hero : Model
hero =
  Model 0 0 0 0 70 heroColor

opponent : Model
opponent  =
  Model 0 0 0 0 70 opponentColor

enemiesState = {
  enemies = [
    Enemy 50 50 {x = 1, y = 1} {x = 1, y = 1} 10 yellow
  ],

  seed = Random.initialSeed 20
 }
