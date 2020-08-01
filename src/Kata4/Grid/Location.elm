module Kata4.Grid.Location exposing (..)

import Kata4.Grid.Direction exposing (Direction(..))
import Kata4.Grid.Size exposing (Size)
import Random


type alias Location =
    ( Int, Int )


step : Direction -> Location -> Location
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


stepWarp : Direction -> Size -> Location -> Location
stepWarp d sz p =
    step d p |> warp sz


warp : Size -> Location -> Location
warp sz ( x, y ) =
    ( modBy sz.width x, modBy sz.height y )


random : Size -> Random.Generator Location
random sz =
    Random.pair (Random.int 0 (sz.width - 1)) (Random.int 0 (sz.height - 1))
