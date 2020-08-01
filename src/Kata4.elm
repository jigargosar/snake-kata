module Kata4 exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Kata4.Counter as Counter exposing (Counter)
import Kata4.Grid.Direction as Dir exposing (Direction(..))
import Kata4.Grid.Location exposing (Location)
import Kata4.Grid.Size exposing (Size)
import Kata4.World as World exposing (Response(..), World)
import Random exposing (Generator, Seed)
import Svg
import Svg.Attributes as SA



-- KATA 4


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { world : World
    , state : State
    , seed : Seed
    }


type State
    = Over
    | Running { autoStepCounter : Counter, inputDirection : Maybe Direction }


init : Model
init =
    generate (Random.initialSeed 43)


generate : Seed -> Model
generate seed0 =
    let
        ( world, seed ) =
            Random.step World.generator seed0
    in
    { world = world
    , state =
        Running
            { inputDirection = Nothing
            , autoStepCounter = Counter.upto autoStepDelay
            }
    , seed = seed
    }


restart : Model -> Model
restart model =
    generate model.seed


type Msg
    = Tick
    | OnKeyDown String


autoStepDelay =
    20


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            case model.state of
                Over ->
                    model

                Running { inputDirection, autoStepCounter } ->
                    updateRunningOnTick inputDirection autoStepCounter model

        OnKeyDown key ->
            case model.state of
                Running r ->
                    case Dir.fromArrowKey key of
                        Just newInputDirection ->
                            { model | state = Running { r | inputDirection = Just newInputDirection } }

                        Nothing ->
                            model

                Over ->
                    case key of
                        "Enter" ->
                            restart model

                        _ ->
                            model


updateRunningOnTick : Maybe Direction -> Counter -> Model -> Model
updateRunningOnTick inputDirection autoStepCounter model =
    case
        firstOf
            [ stepInInputDirection inputDirection
            , autoStep autoStepCounter
            ]
            model.world
    of
        Just (SnakeMoved world) ->
            { model
                | state =
                    Running
                        { inputDirection = Nothing
                        , autoStepCounter = Counter.reset autoStepCounter
                        }
                , world = world
            }

        Just SnakeDied ->
            { model | state = Over }

        Nothing ->
            { model
                | state =
                    Running
                        { inputDirection = inputDirection
                        , autoStepCounter = Counter.step autoStepCounter
                        }
            }


firstOf : List (a -> Maybe b) -> a -> Maybe b
firstOf fns a =
    case fns of
        [] ->
            Nothing

        fmb :: rest ->
            case fmb a of
                Just b ->
                    Just b

                Nothing ->
                    firstOf rest a


autoStep : Counter -> World -> Maybe World.Response
autoStep autoStepCounter world =
    if Counter.done autoStepCounter then
        Just (World.stepSnake world)

    else
        Nothing


stepInInputDirection : Maybe Direction -> World -> Maybe World.Response
stepInInputDirection inputDirection model =
    inputDirection
        |> Maybe.andThen
            (\direction -> World.changeDirection direction model)
        |> Maybe.map World.stepSnake


subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (\_ -> Tick)
        , Browser.Events.onKeyDown (JD.field "key" JD.string |> JD.map OnKeyDown)
        ]



-- VIEW


view : Model -> Html Msg
view model =
    case model.state of
        Running _ ->
            div
                [ style "display" "grid"
                , style "place-items" "center"
                ]
                [ h2 [] [ text "Move with Arrow keys" ]
                , viewBoard model.world
                ]

        Over ->
            div
                [ style "display" "grid"
                , style "place-items" "center"
                ]
                [ h2 [] [ text "Game Over : Press Enter" ]
                , viewBoard model.world
                ]


viewBoard : World -> Html msg
viewBoard model =
    viewBoardHelp
        model.size
        model.direction
        model.head
        model.tail
        model.fruit


viewBoardHelp : Size -> Direction -> Location -> List Location -> Location -> Html msg
viewBoardHelp sz dir head tail fruit =
    let
        cw =
            40

        ( w, h ) =
            ( sz.width, sz.height )
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


viewHead : Direction -> Location -> Html msg
viewHead dir ( x, y ) =
    div
        [ gridRow (y + 1)
        , gridColumn (x + 1)
        , style "background-color" "hsl(0deg 85% 60%)"
        , style "z-index" "0"
        ]
        [ div
            [ style "width" "100%"
            , style "height" "100%"
            , style "transform" ("scale(0.8) rotate(" ++ String.fromInt (Dir.toDegrees dir) ++ "deg)")
            ]
            [ triangleRightSvg "black"
            ]
        ]


triangleRightSvg : String -> Html msg
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



-- Copied from elm-playground


toNgonPoints : Int -> Int -> Float -> String -> String
toNgonPoints i n radius string =
    if i == n then
        string

    else
        let
            turnOffset =
                -0.25

            a =
                turns (toFloat i / toFloat n + turnOffset)

            x =
                radius * cos a

            y =
                radius * sin a
        in
        toNgonPoints (i + 1) n radius (string ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ " ")


viewFruit : Location -> Html msg
viewFruit ( x, y ) =
    div
        [ gridRow (y + 1)
        , gridColumn (x + 1)
        , style "background-color" "hsl(110deg 85% 60%)"
        , style "z-index" "2"
        ]
        []


viewTail : List Location -> List (Html msg)
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


viewGridBackgroundCells : Int -> Int -> List (Html msg)
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
