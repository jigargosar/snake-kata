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
    , pos = ( 9, 19 )
    }


update : Computer -> Mem -> Mem
update _ m =
    m


view : Computer -> Mem -> List Shape
view _ mem =
    let
        cellWidth =
            100

        ( x, y ) =
            mem.pos

        cx =
            toFloat (x * cellWidth) + toFloat cellWidth * 0.5

        cy =
            toFloat (y * cellWidth) + toFloat cellWidth * 0.5

        gw =
            toFloat (mem.width * cellWidth)

        gh =
            toFloat (mem.height * cellWidth)
    in
    [ group
        [ rectangle gray gw gh
        , group
            [ square blue cellWidth
                |> fade 0.8
                |> move cx cy
            ]
            |> move (gw * -0.5) (gh * -0.5)
        ]
        |> scale 0.4
    ]
