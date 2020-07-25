module Main exposing (main)

import GridHelper as GH exposing (GridHelper)
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
    , inputDirection : Maybe Direction
    , ticks : Int
    , seed : Seed
    }


init : Seed -> Mem
init initialSeed =
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
    Random.map4 (initMem width height)
        (randomPosition width height)
        randomDirection
        (randomPosition width height)
        Random.independentSeed


initMem : Int -> Int -> Pos -> Direction -> Pos -> Seed -> Mem
initMem width height head direction fruit seed =
    { width = width
    , height = height
    , head = head
    , direction = direction
    , tail = initTail width height head direction
    , fruit = fruit
    , over = False
    , inputDirection = Nothing
    , ticks = 0
    , seed = seed
    }


initTail : Int -> Int -> Pos -> Direction -> List Pos
initTail width height head headDirection =
    let
        tailLength =
            min width height // 2

        next pos =
            stepPosition (opposite headDirection) pos
    in
    iterateN tailLength next head
        |> List.map (warpPosition width height)



-- UPDATE


update : Computer -> Mem -> Mem
update c mem =
    if mem.over then
        if c.keyboard.enter then
            init mem.seed

        else
            mem

    else if modBy 10 mem.ticks == 0 then
        mem
            |> updateDirectionFromInputDirection
            |> updateSnakeAndFruitAndGameOver
            |> recordInputDirection c.keyboard
            |> incTicks

    else
        mem
            |> recordInputDirection c.keyboard
            |> incTicks



-- UPDATE SNAKE & FRUIT
{-
   GridSize -> width height
   SnakeHead -> (x,y) always within grid size
   SnakeTail -> List (x,y) always within grid size
   Fruit -> (x,y) always within grid size.

   SnakeDirection -> used to step snake head. and display current orientation.
                  -> can never point to first position of tail.
                  -> can be changed by user.


   Behaviour

   onTick ->
       dependencies:  width, height, direction.
       updates/maps: head, tail, fruit, seed.

       * compute `newHead`, warp if out of bounds.
            needs: w,h,direction,head



       * match newHead:
           tail collision -> GameOver (no change in snake)
           fruit collision ->
                perform following two order independent updates
                * GrowSnake
                    needs: newHead, oldHead, oldTail
                    changes: head, tail.


                * GenerateNewFruit:
                    needs: w,h, Seed
                    changes: fruit, seed

           otherwise ->
                MoveSnake:
                    needs: newHead, oldHead, oldTail
                    changes: head, tail
-}


type alias TickContext a =
    { a
        | width : Int
        , height : Int
        , direction : Direction
    }


type alias TickState b =
    { b
        | head : Pos
        , tail : List Pos
        , fruit : Pos
    }


onGameTick : TickContext a -> TickState b -> Generator (TickState b)
onGameTick context state =
    let
        newHead =
            stepSnakeHead2 context state.head
    in
    Debug.todo "impl"


stepSnakeHead2 : TickContext a -> Pos -> Pos
stepSnakeHead2 context headPos =
    Debug.todo "impl"


updateSnakeAndFruitAndGameOver : Mem -> Mem
updateSnakeAndFruitAndGameOver mem =
    let
        newHead =
            stepSnakeHead mem.width mem.height mem.direction mem.head
    in
    if List.member newHead mem.tail then
        -- Check for game over
        { mem | over = True }

    else if newHead == mem.fruit then
        { mem | head = newHead, tail = mem.head :: mem.tail }
            |> generate (randomPosition mem.width mem.height)
            |> uncurry setFruit

    else
        { mem | head = newHead, tail = mem.head :: dropLast mem.tail }


stepSnakeHead : Int -> Int -> Direction -> Pos -> Pos
stepSnakeHead w h direction headPos =
    headPos
        |> stepPosition direction
        |> warpPosition w h


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


setFruit : Pos -> Mem -> Mem
setFruit fruit model =
    { model | fruit = fruit }


generate : Generator a -> { b | seed : Seed } -> ( a, { b | seed : Seed } )
generate gen seeded =
    Random.step gen seeded.seed
        |> Tuple.mapSecond (\seed -> { seeded | seed = seed })


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

        Just d ->
            { mem | inputDirection = Just d }


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



-- SAFE RECURSION / LOOPS


type Loop state result
    = Loop state
    | Done result


loop : (state -> Loop state result) -> state -> result
loop f state0 =
    case f state0 of
        Loop state ->
            loop f state

        Done result ->
            result


type Unfold seed result
    = Unfold seed result
    | Unfolded


unfold : (seed -> Unfold seed result) -> seed -> List result
unfold f seed0 =
    unfoldHelp f seed0 []


unfoldHelp f seed0 reverseResults =
    case f seed0 of
        Unfold seed result ->
            unfoldHelp f seed (result :: reverseResults)

        Unfolded ->
            List.reverse reverseResults


applyN : Int -> (a -> a) -> a -> a
applyN n f x =
    if n <= 0 then
        x

    else
        applyN (n - 1) f (f x)


iterateN : Int -> (a -> a) -> a -> List a
iterateN n f x =
    iterateNHelp n f x []


iterateNHelp : Int -> (a -> a) -> a -> List a -> List a
iterateNHelp n f x reverseXS =
    if n <= 0 then
        List.reverse reverseXS

    else
        let
            nextX =
                f x
        in
        iterateNHelp (n - 1) f nextX (nextX :: reverseXS)
