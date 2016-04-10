module Inputs (..) where

import Result
import Graphics.Input.Field exposing (..)
import Json.Decode exposing ((:=))
import Signal
import Debug
import Keyboard
import Time exposing (..)

type Action
  = NoOp
  | SubmitName
  | Quit

type alias KeyboardArrows =
    { y : Int
    , x : Int
    }

combineInputs dt arrows (response, name, action) =  
  (dt, arrows, response, name, action)

input : Signal (Time, KeyboardArrows, Input, Content, Action)
input =
    Signal.sampleOn delta (Signal.map3 combineInputs delta Keyboard.arrows mailboxes)

delta : Signal Time
delta =
    Signal.map (\t -> t) (fps 60)

combineMailboxes : String -> Content -> Action -> (Input, Content, Action) 
combineMailboxes response name action =  
  let
    inputRecord = Result.withDefault initialResponse 
      (Json.Decode.decodeString decodeInput response)
  in
    (inputRecord, name, action)

mailboxes = Signal.map3 combineMailboxes 
  responses.signal
  nameMailbox.signal
  actionsMailbox.signal

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
