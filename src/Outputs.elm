module Outputs (..) where

import Config exposing (..)
import Task exposing (Task, andThen)
import Json.Encode
import Signal
import SocketIO exposing (io, defaultOptions, emit, on)
import Keyboard

import Inputs exposing (nameMailbox)

socket = io serverUrl defaultOptions
send x = 
  socket `andThen` emit "SELF_UPDATE" x

encodeDataAndSend : Signal (Task x ())
encodeDataAndSend = Signal.map (encodeData>>send) outputs 

outputs = (Signal.map2 (,) Keyboard.arrows nameMailbox.signal)

encodeData (record, name) =
  Json.Encode.encode 0 (
    Json.Encode.object
        [ ("y", Json.Encode.int record.y)
        , ("x", Json.Encode.int record.x)
        , ("name", Json.Encode.string name.string)
        ]
  )
