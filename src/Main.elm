module Main exposing (main)

import Playground exposing (..)


main =
    game view update initial


type alias Mem =
    { width : Int
    , height : Int
    , head : Pos
    , tail : List Pos
    , direction : Direction
    , inputDirection : Maybe Direction
    , ticks : Int
    }


type Direction
    = Up
    | Down
    | Left
    | Right


isHorizontal : Direction -> Bool
isHorizontal direction =
    direction == Left || direction == Right


isVertical : Direction -> Bool
isVertical direction =
    direction == Up || direction == Down


areOrthogonal : Direction -> Direction -> Bool
areOrthogonal d1 d2 =
    (isHorizontal d1 && isVertical d2)
        || (isVertical d1 && isHorizontal d2)


type alias Pos =
    ( Int, Int )


initial : Mem
initial =
    { width = 10
    , height = 20
    , head = ( 5, 5 )
    , tail = [ ( 4, 5 ), ( 3, 5 ) ]
    , direction = Right
    , inputDirection = Nothing
    , ticks = 0
    }


update : Computer -> Mem -> Mem
update c mem =
    if modBy 10 mem.ticks == 0 then
        mem
            |> updateDirectionFromCachedInputDirection
            --|> step
            |> cacheInputDirection c.keyboard
            |> incTicks

    else
        cacheInputDirection c.keyboard mem
            |> incTicks


step : Mem -> Mem
step mem =
    let
        ( x, y ) =
            mem.head

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
    { mem | head = ( x + dx |> modBy mem.width, y + dy |> modBy mem.height ) }


incTicks : Mem -> Mem
incTicks mem =
    { mem | ticks = mem.ticks + 1 }


cacheInputDirection : Keyboard -> Mem -> Mem
cacheInputDirection k mem =
    let
        inputDirection =
            if k.left then
                Just Left

            else if k.right then
                Just Right

            else if k.up then
                Just Up

            else if k.down then
                Just Down

            else
                mem.inputDirection
    in
    { mem | inputDirection = inputDirection }


updateDirectionFromCachedInputDirection : Mem -> Mem
updateDirectionFromCachedInputDirection mem =
    case mem.inputDirection of
        Just inputDirection ->
            if areOrthogonal inputDirection mem.direction then
                { mem | direction = inputDirection, inputDirection = Nothing }

            else
                mem

        Nothing ->
            mem


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
            mem.head

        gw =
            toFloat (mem.width * 100)

        gh =
            toFloat (mem.height * 100)
    in
    group
        [ rectangle gray gw gh
        , group
            (viewCell mem.head
                :: List.map viewCell mem.tail
            )
            |> move (gw * -0.5) (gh * -0.5)
        ]


viewCell ( x, y ) =
    let
        cx =
            toFloat (x * 100) + 50

        cy =
            toFloat (y * 100) + 50
    in
    group
        [ square blue 100
            |> fade 0.8
            |> move cx cy
            |> scale 0.95
        ]


toCellWidth screen mem =
    let
        cellWidth =
            min (screen.width / toFloat mem.width) (screen.height / toFloat mem.height)
                * 0.9
    in
    cellWidth
