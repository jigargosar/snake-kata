module Kata4.Grid.Direction exposing
    ( Direction(..)
    , fromArrowKey
    , opposite
    , random
    , toDegrees
    )

import Random exposing (Generator)


type Direction
    = Up
    | Down
    | Left
    | Right


toDegrees : Direction -> Int
toDegrees direction =
    case direction of
        Up ->
            -90

        Down ->
            90

        Left ->
            180

        Right ->
            0


fromArrowKey : String -> Maybe Direction
fromArrowKey key =
    case key of
        "ArrowUp" ->
            Just Up

        "ArrowDown" ->
            Just Down

        "ArrowLeft" ->
            Just Left

        "ArrowRight" ->
            Just Right

        _ ->
            Nothing


random : Generator Direction
random =
    Random.uniform Up [ Down, Left, Right ]


opposite : Direction -> Direction
opposite direction =
    case direction of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left
