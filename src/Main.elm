module Main exposing (main)

import GridHelper as GH exposing (GridHelper)
import Playground exposing (..)
import Random exposing (Seed)


main =
    game view update initial


type alias Mem =
    { -- WORLD
      width : Int
    , height : Int

    -- SNAKE
    , head : Pos
    , direction : Direction
    , tail : List Pos

    --
    , fruit : Pos

    --
    , inputDirection : Maybe Direction
    , ticks : Int
    , seed : Seed
    }


type Direction
    = Up
    | Down
    | Left
    | Right


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


type alias Pos =
    ( Int, Int )


initial : Mem
initial =
    let
        mem : Mem
        mem =
            { width = 10
            , height = 20
            , head = ( 5, 5 )
            , tail = [ ( 4, 5 ), ( 3, 5 ), ( 2, 5 ), ( 1, 5 ), ( 0, 5 ) ]
            , fruit = ( 3, 10 )
            , direction = Right
            , inputDirection = Nothing
            , ticks = 0
            , seed = Random.initialSeed 42
            }
    in
    mem
        |> randomizeFruit


fruitPositionGenerator : Mem -> Random.Generator Pos
fruitPositionGenerator mem =
    Random.pair (Random.int 0 (mem.width - 1)) (Random.int 0 (mem.height - 1))


randomizeFruit : Mem -> Mem
randomizeFruit mem =
    let
        ( fruit, seed ) =
            Random.step (fruitPositionGenerator mem) mem.seed
    in
    { mem | fruit = fruit, seed = seed }



-- UPDATE


update : Computer -> Mem -> Mem
update c mem =
    if modBy 10 mem.ticks == 0 then
        mem
            |> updateDirectionFromInputDirection
            |> updateSnake
            |> recordInputDirection c.keyboard
            |> incTicks

    else
        mem
            |> recordInputDirection c.keyboard
            |> incTicks



-- UPDATE SNAKE


updateSnake : Mem -> Mem
updateSnake mem =
    let
        newHead =
            mem.head
                |> stepPosition mem.direction
                |> warpPosition mem.width mem.height
    in
    if newHead == mem.fruit then
        { mem | head = newHead, tail = mem.head :: mem.tail }
            |> randomizeFruit

    else
        { mem | head = newHead, tail = mem.head :: dropLast mem.tail }


warpPosition : Int -> Int -> Pos -> Pos
warpPosition w h ( x, y ) =
    ( modBy w x, modBy h y )


stepPosition : Direction -> Pos -> Pos
stepPosition direction ( x, y ) =
    let
        ( dx, dy ) =
            case direction of
                Up ->
                    ( 0, 1 )

                Down ->
                    ( 0, -1 )

                Left ->
                    ( -1, 0 )

                Right ->
                    ( 1, 0 )
    in
    ( x + dx, y + dy )


dropLast : List a -> List a
dropLast =
    List.reverse >> List.drop 1 >> List.reverse



-- UPDATE TICKS


incTicks : Mem -> Mem
incTicks mem =
    { mem | ticks = mem.ticks + 1 }



-- UPDATE DIRECTION / INPUT DIRECTION


recordInputDirection : Keyboard -> Mem -> Mem
recordInputDirection k mem =
    case toDirection k of
        Nothing ->
            -- Preserve Last Direction
            mem

        d ->
            { mem | inputDirection = d }


toDirection : Keyboard -> Maybe Direction
toDirection k =
    if k.left then
        Just Left

    else if k.right then
        Just Right

    else if k.up then
        Just Up

    else if k.down then
        Just Down

    else
        Nothing


updateDirectionFromInputDirection : Mem -> Mem
updateDirectionFromInputDirection mem =
    case mem.inputDirection of
        Just requestedDirection ->
            if requestedDirection /= opposite mem.direction then
                { mem
                    | direction = requestedDirection
                    , inputDirection = Nothing
                }

            else
                mem

        Nothing ->
            mem



-- VIEW


view : Computer -> Mem -> List Shape
view c mem =
    let
        cellWidth =
            computeCellWidth c.screen mem
    in
    let
        gridHelper =
            GH.init mem.width mem.height cellWidth
    in
    [ viewGrid gridHelper mem
    ]


viewGrid : GridHelper -> Mem -> Shape
viewGrid gridHelper mem =
    group
        [ viewGridBackground gridHelper
        , group
            (viewHead gridHelper mem.direction mem.head
                :: viewFruit gridHelper mem.fruit
                :: List.map (viewTail gridHelper) mem.tail
                |> List.reverse
            )
        ]


viewGridBackground : GridHelper -> Shape
viewGridBackground gridHelper =
    rectangle gray (GH.width gridHelper) (GH.height gridHelper)


viewHead : GridHelper -> Direction -> ( Int, Int ) -> Shape
viewHead gridHelper direction ( x, y ) =
    let
        ( cx, cy ) =
            GH.toScreen x y gridHelper

        cellWidth =
            GH.cellWidth gridHelper

        ang =
            case direction of
                Up ->
                    0

                Down ->
                    180

                Left ->
                    90

                Right ->
                    -90
    in
    group
        [ square red cellWidth
            |> fade 0.8
        , triangle black (cellWidth / 3)
            |> rotate ang
        ]
        |> move cx cy
        |> scale 0.95


viewTail =
    viewCell blue


viewFruit =
    viewCell darkGreen


viewCell color gridHelper ( x, y ) =
    let
        ( cx, cy ) =
            GH.toScreen x y gridHelper

        cellWidth =
            GH.cellWidth gridHelper
    in
    group
        [ square color cellWidth
            |> fade 0.8
            |> move cx cy
            |> scale 0.95
        ]


computeCellWidth : Screen -> Mem -> Float
computeCellWidth screen mem =
    let
        cellWidth =
            min (screen.width / toFloat mem.width) (screen.height / toFloat mem.height)
                * 0.9
    in
    cellWidth
