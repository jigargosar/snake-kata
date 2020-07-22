module Main exposing (main)

import Playground exposing (..)


main =
    game view update initial


type alias Mem =
    { width : Int
    , height : Int
    , pos : Pos
    , direction : Direction
    , ticks : Int
    }


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Pos =
    ( Int, Int )


initial : Mem
initial =
    { width = 10
    , height = 20
    , pos = ( 0, 0 )
    , direction = Right
    , ticks = 0
    }


update : Computer -> Mem -> Mem
update c mem =
    if modBy 30 mem.ticks == 0 then
        step mem
            |> updateDirection c.keyboard
            |> incTicks

    else
        updateDirection c.keyboard mem
            |> incTicks


step : Mem -> Mem
step mem =
    let
        ( x, y ) =
            mem.pos

        ( dx, dy ) =
            case mem.direction of
                Up ->
                    ( 0, 1 )

                Down ->
                    ( 0, -1 )

                Left ->
                    ( -1, 0 )

                Right ->
                    ( 1, 0 )
    in
    { mem | pos = ( x + dx |> modBy mem.width, y + dy |> modBy mem.height ) }


incTicks : Mem -> Mem
incTicks mem =
    { mem | ticks = mem.ticks + 1 }


updateDirection : Keyboard -> Mem -> Mem
updateDirection k mem =
    let
        existingVertical =
            mem.direction == Up || mem.direction == Down

        existingHorizontal =
            mem.direction == Left || mem.direction == Right

        newDirection =
            if k.left && existingVertical then
                Left

            else if k.right && existingVertical then
                Right

            else if k.up && existingHorizontal then
                Up

            else if k.down && existingHorizontal then
                Down

            else
                mem.direction
    in
    { mem | direction = newDirection }


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
