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
view c mem =
    let
        cellWidth =
            toCellWidth c.screen mem
    in
    [ viewGrid mem
        |> scale (cellWidth * 0.01)
    ]


viewGrid : Mem -> Shape
viewGrid mem =
    let
        ( x, y ) =
            mem.pos

        cx =
            toFloat (x * 100) + 50

        cy =
            toFloat (y * 100) + 50

        gw =
            toFloat (mem.width * 100)

        gh =
            toFloat (mem.height * 100)
    in
    group
        [ rectangle gray gw gh
        , group
            [ square blue 100
                |> fade 0.8
                |> move cx cy
            ]
            |> move (gw * -0.5) (gh * -0.5)
        ]


toCellWidth screen mem =
    let
        cellWidth =
            min (screen.width / toFloat mem.width) (screen.height / toFloat mem.height)
                * 0.9
    in
    cellWidth
