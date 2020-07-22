module Main exposing (main)

import Playground exposing (..)


main =
    game view update initial


type alias Mem =
    { width : Int
    , height : Int
    , pos : Pos
    }


type alias Pos =
    ( Int, Int )


initial : Mem
initial =
    { width = 10
    , height = 20
    , pos = ( 10, 10 )
    }


update : Computer -> Mem -> Mem
update _ m =
    m


view : Computer -> Mem -> List Shape
view _ mem =
    let
        cellWidth =
            20

        ( x, y ) =
            mem.pos

        sx =
            toFloat (cellWidth - mem.width * cellWidth + x * cellWidth)

        sy =
            toFloat (cellWidth - mem.height * cellWidth + y * cellWidth)
    in
    [ square blue cellWidth
        |> fade 0.8
        |> move sx sy
    ]
