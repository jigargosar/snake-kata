module MainV2 exposing (main)

import Browser
import Browser.Events
import Html exposing (div, h1, text)
import Html.Attributes exposing (style)



-- KATA 2: Without Elm Playground


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { ticks = 0 }, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = \_ -> Browser.Events.onAnimationFrameDelta (\_ -> Tick)
        , view = view
        }


type alias Model =
    { ticks : Int }


type Msg
    = Tick


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            { model | ticks = model.ticks + 1 }


type Snake
    = Snake Int Int Direction Pos (List Pos)


initialSnake : Int -> Int -> Snake
initialSnake w h =
    let
        initialHead =
            ( 6, 9 ) |> warp w h

        initialDirection =
            Right

        initialTail =
            List.repeat 5 initialHead
                |> List.indexedMap (\i -> stepN (i + 1) (opposite initialDirection) >> warp w h)
    in
    Snake w h initialDirection initialHead initialTail


moveSnake : Snake -> Snake
moveSnake (Snake w h d hd t) =
    Snake w h d (stepWarp d w h hd) (List.map (stepWarp d w h) t)


stepWarp d w h p =
    step d p |> warp w h


warp w h ( x, y ) =
    ( modBy w x, modBy h y )


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


stepN : Int -> Direction -> Pos -> Pos
stepN n d p =
    if n <= 0 then
        p

    else
        stepN (n - 1) d (step d p)


applyN n f x =
    if n <= 0 then
        x

    else
        applyN (n - 1) f (f x)


view model =
    let
        w =
            10

        h =
            20

        cw =
            40

        dt =
            model.ticks // 10

        (Snake _ _ _ head tail) =
            initialSnake w h
                |> applyN dt moveSnake
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
