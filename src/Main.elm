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
    { width = 30
    , height = 50
    , pos = ( 10, 10 )
    }


update : Computer -> Mem -> Mem
update _ m =
    m


view : Computer -> Mem -> List Shape
view c _ =
    let
        t =
            c.time
    in
    [ group [ square blue 50 |> fade 0.5, rectangle red 1 50 |> rotate 45 ] |> moveX (wave -200 200 4 t)
    , group [ rectangle red 1 400, rectangle red 400 1 ]
    ]
