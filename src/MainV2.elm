module MainV2 exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Random exposing (Generator, Seed)
import Svg
import Svg.Attributes as SA



-- KATA 2: Without Elm Playground, using CSS GRID
-- DIRECTION


type Direction
    = Up
    | Down
    | Left
    | Right


toDegrees : Direction -> Int
toDegrees direction =
    case direction of
        Up ->
            -90

        Down ->
            90

        Left ->
            180

        Right ->
            0


randomDirection : Generator Direction
randomDirection =
    Random.uniform Up [ Down, Left, Right ]


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



-- POSITION


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


stepWarp : Direction -> Int -> Int -> Pos -> Pos
stepWarp d w h p =
    step d p |> warp w h


warp : Int -> Int -> Pos -> Pos
warp w h ( x, y ) =
    ( modBy w x, modBy h y )


randomPosition : Int -> Int -> Random.Generator Pos
randomPosition w h =
    Random.pair (Random.int 0 (w - 1)) (Random.int 0 (h - 1))


applyN n f x =
    if n <= 0 then
        x

    else
        applyN (n - 1) f (f x)



-- SNAKE


snakeGenerator : Generator Snake
snakeGenerator =
    let
        w =
            10

        h =
            20

        posGen =
            randomPosition w h
    in
    Random.map3 (initSnake w h)
        posGen
        randomDirection
        posGen


initSnake : Int -> Int -> Pos -> Direction -> Pos -> Snake
initSnake w h head dir fruit =
    let
        tail =
            List.repeat 5 head |> List.indexedMap tailHelp

        tailHelp i =
            applyN (i + 1) (step (opposite dir)) >> warp w h
    in
    Snake w h dir head tail fruit


type Snake
    = Snake Int Int Direction Pos (List Pos) Pos


type SnakeResult
    = SnakeAlive (Generator Snake)
    | SnakeDead


changeDirection : Direction -> Snake -> Maybe Snake
changeDirection direction (Snake w h d hd t f) =
    if direction /= opposite d then
        Snake w h direction hd t f |> Just

    else
        Nothing


moveSnake : Snake -> SnakeResult
moveSnake (Snake w h d hd t f) =
    let
        newHead =
            stepWarp d w h hd
    in
    if List.member newHead t then
        SnakeDead

    else if newHead == f then
        randomPosition w h
            |> Random.map (Snake w h d newHead (hd :: t))
            |> SnakeAlive

    else
        Snake w h d newHead (hd :: dropLast t) f
            |> Random.constant
            |> SnakeAlive


dropLast : List a -> List a
dropLast =
    List.reverse >> List.drop 1 >> List.reverse



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Model State (Maybe Direction) Int Seed


type State
    = Over Snake
    | Running Snake


init : Model
init =
    generateModel (Random.initialSeed 43)


generateModel : Seed -> Model
generateModel seed =
    let
        ( snake, newSeed ) =
            Random.step snakeGenerator seed
    in
    Model (Running snake) Nothing 0 newSeed


type Msg
    = Tick
    | OnKeyDown String


delay =
    20


update : Msg -> Model -> Model
update msg ((Model state inputDirection ticks seed) as model) =
    case msg of
        Tick ->
            case state of
                Running snake ->
                    updateOnTick snake inputDirection ticks seed

                _ ->
                    model

        OnKeyDown key ->
            case state of
                Running _ ->
                    case toDirection key of
                        Just newInputDirection ->
                            Model state (Just newInputDirection) ticks seed

                        Nothing ->
                            model

                Over _ ->
                    case key of
                        "Enter" ->
                            generateModel seed

                        _ ->
                            model


updateOnTick : Snake -> Maybe Direction -> Int -> Seed -> Model
updateOnTick snake inputDirection ticks seed =
    case
        inputDirection
            |> Maybe.andThen
                (\direction ->
                    changeDirection direction snake
                        |> Maybe.map (\newSnake -> stepSnake newSnake seed)
                )
    of
        Just model ->
            model

        Nothing ->
            if modBy delay ticks == 0 then
                stepSnake snake seed

            else
                Model (Running snake) inputDirection (ticks + 1) seed


stepSnake : Snake -> Seed -> Model
stepSnake snake seed =
    generateStateAndResetTicksAndInput (stepInCurrentDirection snake) seed


stepInCurrentDirection : Snake -> Generator State
stepInCurrentDirection snake =
    case moveSnake snake of
        SnakeAlive gen ->
            Random.map Running gen

        SnakeDead ->
            Over snake |> Random.constant


generateStateAndResetTicksAndInput : Generator State -> Seed -> Model
generateStateAndResetTicksAndInput generator seed =
    let
        ( newState, newSeed ) =
            Random.step generator seed
    in
    Model newState Nothing 1 newSeed


toDirection : String -> Maybe Direction
toDirection key =
    case key of
        "ArrowUp" ->
            Just Up

        "ArrowDown" ->
            Just Down

        "ArrowLeft" ->
            Just Left

        "ArrowRight" ->
            Just Right

        _ ->
            Nothing


subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (\_ -> Tick)
        , Browser.Events.onKeyDown (JD.field "key" JD.string |> JD.map OnKeyDown)
        ]



-- VIEW


view : Model -> Html Msg
view (Model state _ _ _) =
    case state of
        Running snake ->
            div
                [ style "display" "grid"
                , style "place-items" "center"
                ]
                [ h1 [] [ text "Running" ]
                , viewSnake snake
                ]

        Over snake ->
            div
                [ style "display" "grid"
                , style "place-items" "center"
                ]
                [ h1 [] [ text "Game Over" ]
                , viewSnake snake
                ]


viewSnake : Snake -> Html Msg
viewSnake (Snake w h dir head tail fruit) =
    let
        cw =
            40
    in
    div
        [ style "display" "grid"
        , style "place-items" "center"
        ]
        [ div
            [ style "display" "grid"
            , style "grid-template-rows" (" repeat(" ++ String.fromInt h ++ ", " ++ String.fromFloat cw ++ "px) ")
            , style "grid-template-columns" (" repeat(" ++ String.fromInt w ++ ", " ++ String.fromFloat cw ++ "px) ")
            , style "gap" "1px"
            , style "background-color" "#ddd"
            ]
            (viewGridBackgroundCells w h
                ++ [ viewFruit fruit ]
                ++ [ viewHead dir head ]
                ++ viewTail tail
            )
        ]


viewHead dir ( x, y ) =
    div
        [ gridRow (y + 1)
        , gridColumn (x + 1)
        , style "background-color" "hsl(0deg 85% 60%)"

        --, style "transform" ("rotate(" ++ String.fromInt (toDegrees dir) ++ "deg)")
        , style "z-index" "0"
        ]
        [ div
            [ style "width" "100%"
            , style "height" "100%"
            , style "transform" ("scale(0.8) rotate(" ++ String.fromInt (toDegrees dir) ++ "deg)")
            ]
            [ --triangleRightSvg "hsl(0deg 85% 60%)"
              triangleRightSvg "black"
            ]
        ]


triangleRightSvg color =
    Svg.svg
        [ style "width" "100%"
        , style "height" "100%"
        , SA.viewBox "-50 -50 100 100"
        , style "transform" "rotate(90deg)"
        ]
        [ Svg.polygon [ SA.points (toTrianglePoints 50), SA.stroke "none", SA.fill color ] [] ]


toTrianglePoints radius =
    toNgonPoints 0 3 radius ""


toNgonPoints : Int -> Int -> Float -> String -> String
toNgonPoints i n radius string =
    if i == n then
        string

    else
        let
            turnOffset =
                -0.25

            --0
            a =
                turns (toFloat i / toFloat n + turnOffset)

            x =
                radius * cos a

            y =
                radius * sin a
        in
        toNgonPoints (i + 1) n radius (string ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ " ")


viewFruit ( x, y ) =
    div
        [ gridRow (y + 1)
        , gridColumn (x + 1)
        , style "background-color" "hsl(110deg 85% 60%)"

        --, style "transform" "rotate(45deg)"
        , style "z-index" "0"
        ]
        []


viewTail =
    let
        viewTailCell ( x, y ) =
            div
                [ gridRow (y + 1)
                , gridColumn (x + 1)
                , style "background-color" "hsl(206deg 85% 60%)"
                , style "z-index" "1"
                ]
                []
    in
    List.map viewTailCell


viewGridBackgroundCells w h =
    let
        positions =
            List.range 0 (w - 1)
                |> List.concatMap (\x -> List.range 0 (h - 1) |> List.map (Tuple.pair x))

        viewXYCell ( x, y ) =
            div
                [ style "display" "grid"
                , style "place-items" "center"
                , style "overflow" "hidden"
                , gridRow (y + 1)
                , gridColumn (x + 1)
                , style "background-color" "#444"
                , style "color" "#fff"
                , style "color" "transparent"

                --, style "border-radius" "20%"
                , style "font-size" "75%"
                ]
                [ text "("
                , text (String.fromInt x)
                , text ","
                , text (String.fromInt y)
                , text ")"
                ]
    in
    List.map viewXYCell positions


gridRow y =
    style "grid-row" (String.fromInt y)


gridColumn x =
    style "grid-column" (String.fromInt x)
