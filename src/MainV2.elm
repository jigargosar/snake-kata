module MainV2 exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (style)
import Json.Decode as JD



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
    = Model Snake Direction Int


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
    in
    Model (Snake w h dir head tail) dir 0


type Snake
    = Snake Int Int Direction Pos (List Pos)


initSnake : Int -> Int -> Snake
initSnake w h =
    let
        head =
            ( 6, 9 ) |> warp w h

        dir =
            Right

        tail =
            List.repeat 5 head |> List.indexedMap tailHelp

        tailHelp i =
            applyN (i + 1) (step (opposite dir)) >> warp w h
    in
    Snake w h dir head tail


moveSnake : Snake -> Snake
moveSnake (Snake w h d hd t) =
    Snake w h d (stepWarp d w h hd) (List.map (stepWarp d w h) t)


type Msg
    = Tick
    | OnKeyDown String


update : Msg -> Model -> Model
update msg ((Model snake nextDir ticks) as model) =
    case msg of
        Tick ->
            if modBy 10 ticks == 0 then
                Model (moveSnake snake) nextDir (ticks + 1)

            else
                Model snake nextDir (ticks + 1)

        OnKeyDown key ->
            let
                dir =
                    toDirection key
                        |> Maybe.andThen (validateDirection snake)
                        |> Maybe.withDefault nextDir
            in
            Model snake dir ticks


validateDirection : Snake -> Direction -> Maybe Direction
validateDirection (Snake _ _ currentDir _ _) nextDir =
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
view (Model (Snake w h _ head tail) _ _) =
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



-- HELPERS
