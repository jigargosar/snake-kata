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


applyN n f x =
    if n <= 0 then
        x

    else
        applyN (n - 1) f (f x)



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
    = Running Snake Int Seed
    | Over Snake Seed


init : Model
init =
    let
        seed =
            Random.initialSeed 43
    in
    let
        ( snake, newSeed ) =
            Random.step snakeGen seed
    in
    Running snake 0 newSeed


snakeGen : Generator Snake
snakeGen =
    let
        w =
            10

        h =
            20
    in
    randomPosition w h
        |> Random.map (initSnake w h)


initSnake w h head =
    let
        dir =
            Right

        tail =
            List.repeat 5 head |> List.indexedMap tailHelp

        tailHelp i =
            applyN (i + 1) (step (opposite dir)) >> warp w h

        fruit =
            ( 7, 8 )
    in
    Snake w h dir dir head tail fruit


type Snake
    = Snake Int Int Direction Direction Pos (List Pos) Pos


type SnakeResult
    = SnakeAlive (Generator Snake)
    | SnakeDead


moveSnake : Snake -> SnakeResult
moveSnake (Snake w h _ d hd t f) =
    let
        newHead =
            stepWarp d w h hd
    in
    if List.member newHead t then
        SnakeDead

    else if newHead == f then
        randomPosition w h
            |> Random.map (Snake w h d d newHead (hd :: t))
            |> SnakeAlive

    else
        Snake w h d d newHead (hd :: dropLast t) f
            |> Random.constant
            |> SnakeAlive


randomPosition : Int -> Int -> Random.Generator Pos
randomPosition w h =
    Random.pair (Random.int 0 (w - 1)) (Random.int 0 (h - 1))


dropLast : List a -> List a
dropLast =
    List.reverse >> List.drop 1 >> List.reverse


type Msg
    = Tick
    | OnKeyDown String


delay =
    20


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            case model of
                Running snake ticks seed ->
                    if modBy delay ticks == 0 then
                        case moveSnake snake of
                            SnakeAlive snakeGenerator ->
                                let
                                    ( newSnake, newSeed ) =
                                        Random.step snakeGenerator seed
                                in
                                Running newSnake (ticks + 1) newSeed

                            SnakeDead ->
                                Over snake seed

                    else
                        Running snake (ticks + 1) seed

                _ ->
                    model

        OnKeyDown key ->
            case model of
                Running snake ticks seed ->
                    case
                        toDirection key
                            |> Maybe.andThen (\d -> setNextDirection d snake)
                    of
                        Just newSnake ->
                            Running newSnake ticks seed

                        Nothing ->
                            model

                Over _ seed ->
                    case key of
                        "Enter" ->
                            let
                                ( snake, newSeed ) =
                                    Random.step snakeGen seed
                            in
                            Running snake 0 newSeed

                        _ ->
                            model


setNextDirection : Direction -> Snake -> Maybe Snake
setNextDirection nextDir (Snake w h currentDir _ hd t f) =
    if nextDir /= opposite currentDir then
        Just (Snake w h currentDir nextDir hd t f)

    else
        Nothing


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


view : Model -> Html Msg
view model =
    case model of
        Running snake _ _ ->
            div
                [ style "display" "grid"
                , style "place-items" "center"
                ]
                [ h1 [] [ text "Running" ]
                , viewBoard snake
                ]

        Over snake _ ->
            div
                [ style "display" "grid"
                , style "place-items" "center"
                ]
                [ h1 [] [ text "Game Over" ]
                , viewBoard snake
                ]


viewBoard : Snake -> Html Msg
viewBoard (Snake w h _ nextDir head tail fruit) =
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
                ++ [ viewHead nextDir head ]
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
