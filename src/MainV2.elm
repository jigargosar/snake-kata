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
    = Snake Direction Pos (List Pos)


initialSnake : Snake
initialSnake =
    let
        initialHead =
            ( 5, 9 )

        initialDirection =
            Right

        initialTail =
            List.repeat 5 initialHead
                |> List.indexedMap (\i -> stepN (i + 1) (opposite initialDirection))
    in
    Snake initialDirection initialHead initialTail


moveSnake : Int -> Int -> Snake -> Snake
moveSnake w h (Snake d hd t) =
    let
        warp ( x, y ) =
            ( modBy w x + 1, modBy h y + 1 )
    in
    Snake d (step d hd |> warp) (List.map (step d >> warp) t)


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


view model =
    let
        w =
            10

        h =
            20

        cw =
            40

        dt =
            model.ticks // 30

        (Snake _ head tail) =
            initialSnake
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
        [ style "grid-row" (String.fromInt y)
        , style "grid-column" (String.fromInt x)
        , style "background-color" "hsl(0deg 85% 60%)"
        , style "transform" "rotate(45deg)"
        , style "z-index" "0"
        ]
        []


viewTail =
    let
        viewTailCell ( x, y ) =
            div
                [ style "grid-row" (String.fromInt y)
                , style "grid-column" (String.fromInt x)
                , style "background-color" "hsl(206deg 85% 60%)"
                , style "z-index" "1"
                ]
                []
    in
    List.map viewTailCell


viewGridBackgroundCells w h =
    let
        positions =
            List.range 1 w
                |> List.concatMap (\x -> List.range 1 h |> List.map (Tuple.pair x))

        viewXYCell ( x, y ) =
            div
                [ style "display" "grid"
                , style "place-items" "center"
                , style "overflow" "hidden"
                , style "grid-row" (String.fromInt y)
                , style "grid-column" (String.fromInt x)
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
