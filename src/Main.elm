module Main exposing (main)

import GridHelper as GH exposing (GridHelper)
import Playground exposing (..)
import Random exposing (Generator, Seed)


main =
    game viewGameState updateGameState (initGameState (Random.initialSeed 42))



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



-- GAME STATE


type GameState
    = Running { inputDirection : Maybe Direction, ticks : Int } Mem
    | Over Mem


initGameState : Seed -> GameState
initGameState initialSeed =
    Running { inputDirection = Nothing, ticks = 0 } (initMem initialSeed)


updateGameState : Computer -> GameState -> GameState
updateGameState computer gameState =
    case gameState of
        Running runState mem ->
            let
                inputDirection =
                    updateInputDirectionFromKeyboard computer.keyboard runState.inputDirection
            in
            if modBy 10 runState.ticks == 0 then
                let
                    memWithUpdatedDirection =
                        updateDirectionFromInputDirection inputDirection mem
                in
                case memWithUpdatedDirection |> updateGameOnTick of
                    Nothing ->
                        Over memWithUpdatedDirection

                    Just runningMem ->
                        Running { inputDirection = Nothing, ticks = runState.ticks + 1 }
                            runningMem

            else
                Running
                    { inputDirection = inputDirection
                    , ticks = runState.ticks + 1
                    }
                    mem

        Over mem0 ->
            if computer.keyboard.enter then
                initGameState mem0.seed

            else
                gameState


updateInputDirectionFromKeyboard : Keyboard -> Maybe Direction -> Maybe Direction
updateInputDirectionFromKeyboard keyboard maybeDirection =
    case toDirection keyboard of
        Just newDirection ->
            Just newDirection

        Nothing ->
            maybeDirection


viewGameState : Computer -> GameState -> List Shape
viewGameState computer gameState =
    case gameState of
        Running _ mem ->
            view computer mem

        Over mem ->
            view computer mem



-- MEM


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
    , over : Bool

    --
    , seed : Seed
    }


initMem : Seed -> Mem
initMem initialSeed =
    Random.step memGenerator initialSeed
        |> Tuple.first


memGenerator : Generator Mem
memGenerator =
    let
        width =
            10
    in
    let
        height =
            20
    in
    Random.map4 (initMemHelp width height)
        (randomPosition width height)
        randomDirection
        (randomPosition width height)
        Random.independentSeed


initMemHelp : Int -> Int -> Pos -> Direction -> Pos -> Seed -> Mem
initMemHelp width height head direction fruit seed =
    { width = width
    , height = height
    , head = head
    , direction = direction
    , tail = initTail width height head direction
    , fruit = fruit
    , over = False
    , seed = seed
    }


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


updateGameOnTick : Mem -> Maybe Mem
updateGameOnTick mem =
    let
        newHead =
            stepSnakeHead mem
    in
    if List.member newHead mem.tail then
        -- Tail Collision
        Nothing

    else if newHead == mem.fruit then
        -- Fruit Collision
        mem
            |> growTail newHead
            |> generateNewFruit
            |> Just

    else
        mem
            |> moveSnake newHead
            |> Just


stepSnakeHead :
    { a | width : Int, height : Int, direction : Direction, head : Pos }
    -> Pos
stepSnakeHead mem =
    mem.head
        |> stepPosition mem.direction
        |> warpPosition mem.width mem.height


growTail :
    Pos
    -> { a | head : Pos, tail : List Pos }
    -> { a | head : Pos, tail : List Pos }
growTail newHead mem =
    { mem | head = newHead, tail = mem.head :: mem.tail }


generateNewFruit :
    { a | width : Int, height : Int, seed : Seed, fruit : Pos }
    -> { a | width : Int, height : Int, seed : Seed, fruit : Pos }
generateNewFruit mem =
    let
        ( fruit, seed ) =
            Random.step (randomPosition mem.width mem.height) mem.seed
    in
    { mem | fruit = fruit, seed = seed }


moveSnake :
    Pos
    -> { a | head : Pos, tail : List Pos }
    -> { a | head : Pos, tail : List Pos }
moveSnake newHead mem =
    { mem | head = newHead, tail = mem.head :: dropLast mem.tail }


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


updateDirectionFromInputDirection : Maybe Direction -> Mem -> Mem
updateDirectionFromInputDirection inputDirection mem =
    case inputDirection of
        Just requestedDirection ->
            if requestedDirection /= opposite mem.direction then
                { mem
                    | direction = requestedDirection
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
    [ viewGameGrid gridHelper mem
    , viewIf mem.over (viewGameOver cellWidth)
    ]


viewIf bool v =
    if bool then
        v

    else
        viewNothing


viewNothing =
    group []


viewGameOver cw =
    group
        [ words black "Game Over"
            |> scale (cw / 16)
        , words black "Press ENTER"
            |> scale (cw / 16 * 0.65)
            |> moveDown (cw * 1.2)
        ]


viewGameGrid : GridHelper -> Mem -> Shape
viewGameGrid gridHelper mem =
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
