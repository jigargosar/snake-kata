module Kata4 exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Kata4.Grid.Direction as Dir exposing (Direction(..))
import Kata4.Grid.Location as Loc exposing (Location)
import Kata4.Grid.Size exposing (Size)
import Kata4.More exposing (applyN, dropLast)
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
    { size : Size
    , head : Location
    , direction : Direction
    , tail : List Location
    , fruit : Location
    , state : State
    , seed : Seed
    }


type State
    = Over
    | Running { autoStepCounter : Int, inputDirection : Maybe Direction }


type alias World a =
    { a
        | size : Size
        , head : Location
        , direction : Direction
        , tail : List Location
        , fruit : Location
        , seed : Seed
    }


init : Model
init =
    generateModel (Random.initialSeed 43)


generateModel : Seed -> Model
generateModel seed =
    Random.step modelGenerator seed |> Tuple.first


modelGenerator : Generator Model
modelGenerator =
    let
        size =
            { width = 10, height = 20 }

        randomLocation =
            Loc.random size
    in
    Random.map4 (initModelHelp size)
        randomLocation
        Dir.random
        randomLocation
        Random.independentSeed


initModelHelp : Size -> Location -> Direction -> Location -> Seed -> Model
initModelHelp size head direction fruit seed =
    { size = size
    , head = head
    , direction = direction
    , tail = initTail size head direction
    , fruit = fruit
    , state = Running { inputDirection = Nothing, autoStepCounter = 0 }
    , seed = seed
    }


initTail : Size -> Location -> Direction -> List Location
initTail size head direction =
    let
        tailHelp i =
            applyN (i + 1) (Loc.stepWarp (Dir.opposite direction) size)
    in
    List.repeat 5 head |> List.indexedMap tailHelp


type Msg
    = Tick
    | OnKeyDown String


autoStepSnakeDelay =
    20


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            updateOnTick model

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
                            generateModel model.seed

                        _ ->
                            model


updateOnTick : Model -> Model
updateOnTick model =
    case model.state of
        Over ->
            model

        Running { inputDirection, autoStepCounter } ->
            case
                firstOf
                    [ stepInInputDirection inputDirection
                    , autoStep autoStepCounter
                    ]
                    model
            of
                Just (SnakeMoved newModel) ->
                    { newModel | state = Running { inputDirection = Nothing, autoStepCounter = autoStepSnakeDelay } }

                Just SnakeDied ->
                    { model | state = Over }

                Nothing ->
                    { model | state = Running { inputDirection = inputDirection, autoStepCounter = autoStepCounter - 1 } }


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


autoStep : Int -> World a -> Maybe (StepWorld a)
autoStep autoStepCounter world =
    if autoStepCounter <= 0 then
        Just (stepSnake world)

    else
        Nothing


stepInInputDirection : Maybe Direction -> World a -> Maybe (StepWorld a)
stepInInputDirection inputDirection model =
    inputDirection
        |> Maybe.andThen
            (\direction -> changeDirection direction model)
        |> Maybe.map stepSnake


type StepWorld a
    = SnakeMoved (World a)
    | SnakeDied


changeDirection : Direction -> World a -> Maybe (World a)
changeDirection direction world =
    if direction /= Dir.opposite world.direction then
        Just { world | direction = direction }

    else
        Nothing


stepSnake : World a -> StepWorld a
stepSnake world =
    let
        newHead =
            Loc.stepWarp world.direction world.size world.head
    in
    if List.member newHead world.tail then
        SnakeDied

    else if newHead == world.fruit then
        let
            ( newFruit, newSeed ) =
                Random.step (Loc.random world.size) world.seed
        in
        SnakeMoved
            { world
                | seed = newSeed
                , fruit = newFruit
                , head = newHead
                , tail = world.head :: world.tail
            }

    else
        SnakeMoved
            { world
                | head = newHead
                , tail = world.head :: dropLast world.tail
            }


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
                , viewBoard model
                ]

        Over ->
            div
                [ style "display" "grid"
                , style "place-items" "center"
                ]
                [ h2 [] [ text "Game Over : Press Enter" ]
                , viewBoard model
                ]


viewBoard : Model -> Html msg
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
