module Kata4.Grid.Location exposing (..)

import Kata4.Grid.Direction exposing (Direction(..))
import Random


type alias Pos =
    ( Int, Int )


step : Direction -> Pos -> Pos
step direction ( x, y ) =
    case direction of
        Up ->
            ( x, y - 1 )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )


stepWarpPosition : Direction -> Int -> Int -> Pos -> Pos
stepWarpPosition d w h p =
    step d p |> warpPosition w h


warpPosition : Int -> Int -> Pos -> Pos
warpPosition w h ( x, y ) =
    ( modBy w x, modBy h y )


randomPosition : Int -> Int -> Random.Generator Pos
randomPosition w h =
    Random.pair (Random.int 0 (w - 1)) (Random.int 0 (h - 1))
