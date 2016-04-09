module Outputs (..) where

import Config exposing (..)
import Task exposing (Task, andThen)
import Json.Encode
import Signal
import SocketIO exposing (io, defaultOptions, emit, on)
import Keyboard

type alias KeyboardArrows =
    { y : Int
    , x : Int
    }

socket = io serverUrl defaultOptions
send x = 
  socket `andThen` emit "SELF_UPDATE" x

playerMove : Signal (Task x ())
playerMove = Signal.map (encodeKeyboardArrows>>send) Keyboard.arrows 

encodeKeyboardArrows record =
  Json.Encode.encode 0 (
    Json.Encode.object
        [ ("y", Json.Encode.int record.y)
        , ("x", Json.Encode.int record.x)
        ]
  )
