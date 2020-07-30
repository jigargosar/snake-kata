module MainV2 exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Random exposing (Generator, Seed)



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
    = Running Snake Direction Int Seed
    | Over Snake Seed


init : Model
init =
    let
        w =
            10

        h =
            20

        head =
            ( 6, 9 ) |> warp w h

        dir =
            Right

        tail =
            List.repeat 5 head |> List.indexedMap tailHelp

        tailHelp i =
            applyN (i + 1) (step (opposite dir)) >> warp w h

        fruit =
            ( 7, 8 )
    in
    Running (Snake w h dir head tail fruit) dir 0 (Random.initialSeed 43)


type Snake
    = Snake Int Int Direction Pos (List Pos) Pos


moveSnake : Direction -> Snake -> Maybe (Generator Snake)
moveSnake d (Snake w h _ hd t f) =
    let
        newHead =
            stepWarp d w h hd
    in
    if List.member newHead t then
        Nothing

    else if newHead == f then
        randomPosition w h
            |> Random.map (Snake w h d newHead (hd :: t))
            |> Just

    else
        Snake w h d newHead (hd :: dropLast t) f
            |> Random.constant
            |> Just


randomPosition : Int -> Int -> Random.Generator Pos
randomPosition w h =
    Random.pair (Random.int 0 (w - 1)) (Random.int 0 (h - 1))


dropLast : List a -> List a
dropLast =
    List.reverse >> List.drop 1 >> List.reverse


type Msg
    = Tick
    | OnKeyDown String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            case model of
                Running snake nextDir ticks seed ->
                    if modBy 10 ticks == 0 then
                        case moveSnake nextDir snake of
                            Just snakeGenerator ->
                                let
                                    ( newSnake, newSeed ) =
                                        Random.step snakeGenerator seed
                                in
                                Running newSnake nextDir (ticks + 1) newSeed

                            Nothing ->
                                Debug.todo "impl"

                    else
                        Running snake nextDir (ticks + 1) seed

                _ ->
                    model

        OnKeyDown key ->
            case model of
                Running snake nextDir ticks seed ->
                    let
                        dir =
                            toDirection key
                                |> Maybe.andThen (validateDirection snake)
                                |> Maybe.withDefault nextDir
                    in
                    Running snake dir ticks seed

                _ ->
                    model


validateDirection : Snake -> Direction -> Maybe Direction
validateDirection (Snake _ _ currentDir _ _ _) nextDir =
    if nextDir /= opposite currentDir then
        Just nextDir

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
view (Running (Snake w h _ head tail fruit) _ _ _) =
    let
        cw =
            40
    in
    div
        [ style "display" "grid"
        , style "place-items" "center"
        ]
        [ h1 [] [ text "View: css grid layout" ]
        , div
            [ style "display" "grid"
            , style "grid-template-rows" (" repeat(" ++ String.fromInt h ++ ", " ++ String.fromFloat cw ++ "px) ")
            , style "grid-template-columns" (" repeat(" ++ String.fromInt w ++ ", " ++ String.fromFloat cw ++ "px) ")
            , style "gap" "1px"
            , style "background-color" "#ddd"
            ]
            (viewGridBackgroundCells w h
                ++ [ viewFruit fruit ]
                ++ [ viewHead head ]
                ++ viewTail tail
            )
        ]


viewHead ( x, y ) =
    div
        [ gridRow (y + 1)
        , gridColumn (x + 1)
        , style "background-color" "hsl(0deg 85% 60%)"
        , style "transform" "rotate(45deg)"
        , style "z-index" "0"
        ]
        []


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



-- HELPERS
