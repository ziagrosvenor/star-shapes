module Inputs (..) where

import Result
import Graphics.Input.Field exposing (..)
import Json.Decode exposing ((:=))
import Signal
import Debug
import Outputs exposing (KeyboardArrows)

type Action
  = NoOp
  | SubmitName
  | Quit

input = Signal.map3 (\response name action -> 
  let
    inputRecord = Result.withDefault initialResponse (Json.Decode.decodeString decodeInput response)

  in
    (inputRecord, name, action)
  ) responses.signal nameMailbox.signal actionsMailbox.signal

initialResponse =
    { h = {x = 0, y = 0}
    , dt = toFloat 40
    , o = {x = 0, y = 0}    
  }

nameMailbox : Signal.Mailbox Content
nameMailbox = Signal.mailbox noContent

actionsMailbox : Signal.Mailbox Action
actionsMailbox = Signal.mailbox NoOp

responses =
  Signal.mailbox "{dt:42,h:{x:0,y:0},o:{x:0,y:0}}"

type alias Input =
    { h : KeyboardArrows
    , dt : Float
    , o : KeyboardArrows
    }

decodeKeyboardArrows : Json.Decode.Decoder KeyboardArrows
decodeKeyboardArrows =
    Json.Decode.object2 KeyboardArrows
         ("y" := Json.Decode.int)
         ("x" := Json.Decode.int)

decodeInput : Json.Decode.Decoder Input
decodeInput =
    Json.Decode.object3 Input
         ("h" := decodeKeyboardArrows)
         ("dt" := Json.Decode.float)
         ("o" := decodeKeyboardArrows)
