module Main exposing (main)

import Playground exposing (..)


main =
    game view update initial


type alias Mem =
    { width : Int
    , height : Int
    , cellWidth : Int
    , pos : Pos
    }


type alias Pos =
    ( Int, Int )


initial : Mem
initial =
    { width = 10
    , height = 20
    , cellWidth = 25
    , pos = ( 10, 10 )
    }


update : Computer -> Mem -> Mem
update _ m =
    m


view : Computer -> Mem -> List Shape
view _ mem =
    [ viewGridCell mem mem.pos
    ]


viewGridCell : Mem -> Pos -> Shape
viewGridCell mem pos =
    square blue (toFloat mem.cellWidth)
        |> fade 0.8
        |> moveCell mem.width mem.height mem.cellWidth pos


moveCell : Int -> Int -> Int -> ( Int, Int ) -> Shape -> Shape
moveCell width height cw ( x, y ) =
    let
        sx =
            toFloat (cw - width * cw + x * cw)

        sy =
            toFloat (cw - height * cw + y * cw)
    in
    move sx sy
