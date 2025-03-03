module Kata1 exposing (main)

import Playground exposing (..)
import Random exposing (Generator, Seed)


main =
    game view update (init (Random.initialSeed 42))



-- DIRECTION


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


randomDirection =
    Random.uniform Up [ Down, Left, Right ]



-- POS


type alias Pos =
    ( Int, Int )


randomPosition : Int -> Int -> Random.Generator Pos
randomPosition w h =
    Random.pair (Random.int 0 (w - 1)) (Random.int 0 (h - 1))


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



-- World


type World
    = World Int Int Pos Direction (List Pos) Pos



-- Model


type Model
    = Running World Direction Int Seed
    | Over World Seed


init : Seed -> Model
init seed =
    Random.step modelGen seed |> Tuple.first


modelGen : Generator Model
modelGen =
    let
        width =
            10

        height =
            20
    in
    Random.map4
        (\head direction fruit ->
            Running
                (World
                    width
                    height
                    head
                    direction
                    (initTail width height head direction)
                    fruit
                )
                direction
                0
        )
        (randomPosition width height)
        randomDirection
        (randomPosition width height)
        Random.independentSeed


initTail : Int -> Int -> Pos -> Direction -> List Pos
initTail width height head headDirection =
    let
        tailLength =
            min width height // 2

        nextPosition : Pos -> Pos
        nextPosition pos =
            stepPosition (opposite headDirection) pos
                |> warpPosition width height
    in
    iterateN tailLength nextPosition head []


iterateN : Int -> (a -> a) -> a -> List a -> List a
iterateN n next seed reverseXS =
    if n <= 0 then
        List.reverse reverseXS

    else
        let
            x =
                next seed
        in
        iterateN (n - 1) next x (x :: reverseXS)



-- UPDATE


update : Computer -> Model -> Model
update { keyboard } model =
    case model of
        Running ((World w h head prevDir tail fruit) as world) inputDirection ticks seed ->
            let
                dir =
                    toDirection keyboard
                        |> Maybe.andThen
                            (\kDir ->
                                if kDir /= opposite prevDir then
                                    Just kDir

                                else
                                    Nothing
                            )
                        |> Maybe.withDefault inputDirection
            in
            if modBy 10 ticks == 0 then
                let
                    newHead =
                        head
                            |> stepPosition dir
                            |> warpPosition w h
                in
                if List.member newHead tail then
                    Over (World w h head dir tail fruit) seed

                else if newHead == fruit then
                    let
                        ( newFruit, newSeed ) =
                            Random.step (randomPosition w h) seed
                    in
                    Running (World w h newHead dir (head :: tail) newFruit) dir (ticks + 1) newSeed

                else
                    Running (World w h newHead dir (head :: dropLast tail) fruit) dir (ticks + 1) seed

            else
                Running world dir (ticks + 1) seed

        Over _ seed ->
            if keyboard.enter then
                init seed

            else
                model


view : Computer -> Model -> List Shape
view { screen } model =
    case model of
        Running (World w h head dir tail fruit) _ _ _ ->
            let
                cw =
                    computeCellWidth w h screen
            in
            [ renderGrid cw w h head dir tail fruit ]

        Over (World w h head dir tail fruit) _ ->
            let
                cw =
                    computeCellWidth w h screen
            in
            [ renderGrid cw w h head dir tail fruit
            , group
                [ words black "Game Over"
                    |> scale (cw / 16)
                , words black "Press ENTER"
                    |> scale (cw / 16 * 0.65)
                    |> moveDown (cw * 1.2)
                ]
            ]


computeCellWidth : Int -> Int -> Screen -> Float
computeCellWidth w h screen =
    min (screen.width / toFloat w) (screen.height / toFloat h)
        * 0.9


renderGrid : Float -> Int -> Int -> Pos -> Direction -> List Pos -> Pos -> Shape
renderGrid cw w h head dir tail fruit =
    group
        [ -- Background
          rectangle gray (toFloat w * cw) (toFloat h * cw)

        -- Tail
        , group
            (List.map
                (\tailPos ->
                    square blue cw
                        |> scaleCell
                        |> fadeCell
                        |> moveCell cw w h tailPos
                )
                tail
            )

        -- Fruit
        , square darkGreen cw
            |> scaleCell
            |> fadeCell
            |> moveCell cw w h fruit

        -- Head
        , headShape cw dir
            |> scaleCell
            |> fadeCell
            |> moveCell cw w h head
        ]


scaleCell =
    scale 0.95


fadeCell =
    fade 0.9


moveCell : Float -> Int -> Int -> Pos -> Shape -> Shape
moveCell cw w h ( x, y ) =
    move
        (toFloat x * cw + cw / 2 + (toFloat w * cw / -2))
        (toFloat y * cw + cw / 2 + (toFloat h * cw / -2))


headShape cw dir =
    group
        [ square red cw
        , let
            headAngle =
                case dir of
                    Up ->
                        0

                    Down ->
                        180

                    Left ->
                        90

                    Right ->
                        -90
          in
          triangle black (cw * 0.3) |> rotate headAngle
        ]



-- UTIL


dropLast : List a -> List a
dropLast =
    List.reverse >> List.drop 1 >> List.reverse


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
