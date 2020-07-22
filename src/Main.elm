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
view c mem =
    let
        cw =
            20

        ( x, y ) =
            mem.pos

        sx =
            toFloat (cw - mem.width * cw + x * cw)

        sy =
            toFloat (cw - mem.height * cw + y * cw)
    in
    [ square blue cw
        |> fade 0.8
        |> move sx sy
    ]
