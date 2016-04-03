module Input (..) where

import Result
import Json.Encode
import Json.Decode exposing ((:=))
import Signal

input : Signal Input
input = responsesAsObjects

initialInput =
    { h = {x = 0, y = 0}
    , dt = toFloat 40
    , o = {x = 0, y = 0}    
  }

responsesAsObjects = Signal.map (\response -> 
    Result.withDefault initialInput (Json.Decode.decodeString decodeInput response)
 ) responses.signal

responses =
  Signal.mailbox "{dt:42,h:{x:0,y:0},o:{x:0,y:0}}"

type alias H =
    { y : Int
    , x : Int
    }

type alias O =
    { y : Int
    , x : Int
    }

type alias Input =
    { h : H
    , dt : Float
    , o : O
    }

decodeH : Json.Decode.Decoder H
decodeH =
    Json.Decode.object2 H
         ("y" := Json.Decode.int)
         ("x" := Json.Decode.int)
decodeO : Json.Decode.Decoder O
decodeO =
    Json.Decode.object2 O
         ("y" := Json.Decode.int)
         ("x" := Json.Decode.int)
decodeInput : Json.Decode.Decoder Input
decodeInput =
    Json.Decode.object3 Input
         ("h" := decodeH)
         ("dt" := Json.Decode.float)
         ("o" := decodeO)
encodeH : H -> Json.Encode.Value
encodeH record =
    Json.Encode.object
        [ ("y", Json.Encode.int record.y)
        , ("x", Json.Encode.int record.x)
        ]
encodeO : O -> Json.Encode.Value
encodeO record =
    Json.Encode.object
        [ ("y", Json.Encode.int record.y)
        , ("x", Json.Encode.int record.x)
        ]
encodeInput : Input -> Json.Encode.Value
encodeInput record =
    Json.Encode.object
        [ ("h", encodeH record.h)
        , ("dt", Json.Encode.float record.dt)
        , ("o", encodeO record.o)
        ]
