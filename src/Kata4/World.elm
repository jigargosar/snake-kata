module Kata4.World exposing (World, generator)

import Kata4.Grid.Direction as Dir exposing (Direction)
import Kata4.Grid.Location as Loc exposing (Location)
import Kata4.Grid.Size exposing (Size)
import Kata4.More exposing (applyN)
import Random exposing (Generator, Seed)


type alias World =
    { size : Size
    , head : Location
    , direction : Direction
    , tail : List Location
    , fruit : Location
    , seed : Seed
    }


generator : Generator World
generator =
    let
        size =
            { width = 10, height = 20 }

        randomLocation =
            Loc.random size
    in
    Random.map4 (init size)
        randomLocation
        Dir.random
        randomLocation
        Random.independentSeed


init : Size -> Location -> Direction -> Location -> Seed -> World
init size head direction fruit seed =
    { size = size
    , head = head
    , direction = direction
    , tail = initTail size head direction
    , fruit = fruit
    , seed = seed
    }


initTail : Size -> Location -> Direction -> List Location
initTail size head direction =
    let
        tailHelp i =
            applyN (i + 1) (Loc.stepWarp (Dir.opposite direction) size)
    in
    List.repeat 5 head |> List.indexedMap tailHelp
