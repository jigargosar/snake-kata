module Kata5.World exposing (Response(..), World, changeDirection, generator, stepSnake)

import Kata5.Grid.Direction as Dir exposing (Direction)
import Kata5.Grid.Location as Loc exposing (Location)
import Kata5.Grid.Size exposing (Size)
import Kata5.More exposing (applyN, dropLast)
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


type Response
    = SnakeMoved World
    | SnakeDied


changeDirection : Direction -> World -> Maybe World
changeDirection direction world =
    if direction /= Dir.opposite world.direction then
        Just { world | direction = direction }

    else
        Nothing


stepSnake : World -> Response
stepSnake world =
    let
        newHead =
            Loc.stepWarp world.direction world.size world.head
    in
    if List.member newHead world.tail then
        SnakeDied

    else if newHead == world.fruit then
        let
            ( newFruit, newSeed ) =
                Random.step (Loc.random world.size) world.seed
        in
        SnakeMoved
            { world
                | seed = newSeed
                , fruit = newFruit
                , head = newHead
                , tail = world.head :: world.tail
            }

    else
        SnakeMoved
            { world
                | head = newHead
                , tail = world.head :: dropLast world.tail
            }
