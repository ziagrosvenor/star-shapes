module Views (..) where 

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input.Field exposing (..)
import Animation exposing (..)
import Graphics.Input exposing (..)
import Text
import List exposing (..)
import Inputs exposing (..)
import Outputs exposing (..)
import Enemies exposing (getListOfCollidingEnemies)
import Config exposing (..)

backgroundPos = (areaW, areaH)

type View
  = PlayerNameForm
  | GameView

router (w, h) state =
  let 
    { view } = state
  in
    case view of
      PlayerNameForm -> ( inputNameView (w, h) state )
      GameView -> ( gameView (w, h) state )

textStyle = { 
  typeface = [ "roboto", "sans-serif" ]
  , height   = Just 24
  , color    = white
  , bold     = True
  , italic   = False
  , line = Nothing
 }

scoreText : Float -> Form
scoreText score =
  text (
    Text.style textStyle (Text.fromString ("Score " ++ toString score))
  ) |> move (65 - areaW/2, 30-areaH/2)

enemyForm hero {color, rad, x, y} =
  let
    heroLineColor = dotted hero.color
    dottedHeroLine = {
      heroLineColor |
        width = 4
    }

    dottedEnemyLine = {
      dottedHeroLine |
        color = color,
        width = 12
    }
    rock = toForm (image (round rad) (round rad) rockImageUrl)

    enemyForm = if hero.rad > rad then
      rock |> move (x, y)
    else
      group [
        (circle rad |> outlined dottedEnemyLine |> move (x, y)),
        text
          (Text.style textStyle (Text.fromString "Alien")) |> 
          move (x, y)
      ]
  in
    enemyForm

enemyForms hero enemies =
  map (enemyForm hero) enemies

quitButton = move ( 520, -330 ) 
  (toForm (button (Signal.message actionsMailbox.address Quit) "QUIT"))

inputNameView (w, h) {name} =
  container w h middle <|
    collage areaW areaH
      [toForm (field defaultStyle (Signal.message nameMailbox.address) "Name" name), 
       moveX 200 (toForm (button (Signal.message actionsMailbox.address SubmitName) "CHOOSE"))
      ]

gameView (w,h) {hero, opponent, enemiesState, score, name, r, theta, clock} =
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

    angle = animate clock theta
    radius = animate clock r

    heroForm = group [
      circle hero.rad
        |> outlined (dottedHeroLine)
        |> move (hero.x, hero.y),
      text (Text.style textStyle (Text.fromString name.string)) 
        |> move (hero.x, hero.y)
        |> rotate angle
    ]

    opponentForm = circle opponent.rad |> outlined (dottedOpponentLine) |> move (opponent.x, opponent.y)
    opponentName = text (Text.style textStyle (Text.fromString "P2")) |> move (opponent.x, opponent.y)

  in
    container w h middle <|
    collage areaW areaH
      (concat [
        [ background,
          opponentForm, 
          opponentName,
          heroForm,
          (scoreText score)
        ],
        (enemyForms hero enemies),
        [quitButton]
      ])

isPlayerCollided player enemies =
  length (getListOfCollidingEnemies player enemies) > 0
