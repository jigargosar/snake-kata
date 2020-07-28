module Main exposing (main)

import Playground exposing (..)
import Random exposing (Generator, Seed)


main =
    --game view update (init (Random.initialSeed 42))
    game view2 update2 (init2 (Random.initialSeed 42))



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



-- State


type State
    = Running Int Int Pos (List Pos) Direction Direction Pos Int Seed
    | Over Int Int Pos (List Pos) Direction Pos Seed


init2 : Seed -> State
init2 seed =
    let
        width =
            10
    in
    let
        height =
            20
    in
    let
        gen : Generator State
        gen =
            Random.map4
                (\head direction fruit ->
                    Running
                        width
                        height
                        head
                        (initTail width height head direction)
                        direction
                        direction
                        fruit
                        0
                )
                (randomPosition width height)
                randomDirection
                (randomPosition width height)
                Random.independentSeed
    in
    Random.step gen seed
        |> Tuple.first


update2 : Computer -> State -> State
update2 { keyboard } state =
    case state of
        Running w h head tail prevDir nextDir fruit ticks seed ->
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
                        |> Maybe.withDefault nextDir
            in
            if modBy 10 ticks == 0 then
                let
                    newHead =
                        head
                            |> stepPosition dir
                            |> warpPosition w h
                in
                if List.member newHead tail then
                    Over w h head tail dir fruit seed

                else if newHead == fruit then
                    let
                        ( newFruit, newSeed ) =
                            Random.step (randomPosition w h) seed
                    in
                    Running w h newHead (head :: tail) dir dir newFruit (ticks + 1) newSeed

                else
                    Running w h newHead (head :: dropLast tail) dir dir fruit (ticks + 1) seed

            else
                Running w h head tail prevDir dir fruit (ticks + 1) seed

        Over w h head tail direction fruit seed ->
            if keyboard.enter then
                init2 seed

            else
                state


view2 : Computer -> State -> List Shape
view2 { screen } state =
    let
        cellWidth w h =
            min (screen.width / toFloat w) (screen.height / toFloat h)
                * 0.9
    in
    case state of
        Running w h head tail dir _ fruit _ _ ->
            let
                cw =
                    cellWidth w h

                renderCellAt pos shape =
                    shape
                        |> scale 0.95
                        |> fade 0.9
                        |> moveCell cw w h pos

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
            [ -- Background
              renderBackground cw w h

            -- Tail
            , group (List.map (\pos -> square blue cw |> renderCellAt pos) tail)

            -- Fruit
            , square darkGreen cw |> renderCellAt fruit

            -- Head
            , group [ square red cw, triangle black (cw * 0.3) |> rotate headAngle ] |> renderCellAt head
            ]

        Over w h head tail dir fruit _ ->
            let
                cw =
                    cellWidth w h
            in
            [ group
                [ words black "Game Over"
                    |> scale (cw / 16)
                , words black "Press ENTER"
                    |> scale (cw / 16 * 0.65)
                    |> moveDown (cw * 1.2)
                ]
            ]


renderBackground : Float -> Int -> Int -> Shape
renderBackground cw w h =
    rectangle gray (toFloat w * cw) (toFloat h * cw)


moveCell : Float -> Int -> Int -> Pos -> Shape -> Shape
moveCell cw w h ( x, y ) =
    move
        (toFloat x * cw + cw / 2 + (toFloat w * cw / -2))
        (toFloat y * cw + cw / 2 + (toFloat h * cw / -2))


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


dropLast : List a -> List a
dropLast =
    List.reverse >> List.drop 1 >> List.reverse



-- UPDATE DIRECTION / INPUT DIRECTION


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
