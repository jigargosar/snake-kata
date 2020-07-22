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
    , pos = ( 0, 0 )
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

        gx =
            toFloat (mem.width * cellWidth) * -0.5

        gy =
            toFloat (mem.height * cellWidth) * -0.5
    in
    [ group
        [ group
            [ square blue cellWidth
                |> fade 0.8
                |> move cx cy
            ]
            |> move gx gy
        ]
        |> scale 0.25
    ]
