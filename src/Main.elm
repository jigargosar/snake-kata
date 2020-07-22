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
        cw =
            20
    in
    [ square blue cw
        |> fade 0.8
        |> moveCell mem.width mem.height cw mem.pos
    ]


moveCell width height cw ( x, y ) =
    let
        sx =
            toFloat (cw - width * cw + x * cw)

        sy =
            toFloat (cw - height * cw + y * cw)
    in
    move sx sy
