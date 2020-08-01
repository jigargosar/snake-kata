module Kata3 exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Random exposing (Generator, Seed)
import Svg
import Svg.Attributes as SA



-- KATA 3: Using type alias for model, flattening it
-- DIRECTION


type Direction
    = Up
    | Down
    | Left
    | Right


directionToDegrees : Direction -> Int
directionToDegrees direction =
    case direction of
        Up ->
            -90

        Down ->
            90

        Left ->
            180

        Right ->
            0


keyToDirection : String -> Maybe Direction
keyToDirection key =
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


randomDirection : Generator Direction
randomDirection =
    Random.uniform Up [ Down, Left, Right ]


oppositeDirection : Direction -> Direction
oppositeDirection direction =
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


stepWarpPosition : Direction -> Int -> Int -> Pos -> Pos
stepWarpPosition d w h p =
    step d p |> warpPosition w h


warpPosition : Int -> Int -> Pos -> Pos
warpPosition w h ( x, y ) =
    ( modBy w x, modBy h y )


randomPosition : Int -> Int -> Random.Generator Pos
randomPosition w h =
    Random.pair (Random.int 0 (w - 1)) (Random.int 0 (h - 1))



-- UTIL


applyN : Int -> (a -> a) -> a -> a
applyN n f x =
    if n <= 0 then
        x

    else
        applyN (n - 1) f (f x)


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


type alias Model =
    { width : Int
    , height : Int
    , head : Pos
    , direction : Direction
    , tail : List Pos
    , fruit : Pos
    , inputDirection : Maybe Direction
    , state : State
    , ticks : Int
    , seed : Seed
    }


type State
    = Over
    | Running


init : Model
init =
    generateModel (Random.initialSeed 43)


generateModel : Seed -> Model
generateModel seed =
    Random.step modelGenerator seed |> Tuple.first


modelGenerator : Generator Model
modelGenerator =
    let
        width =
            10

        height =
            20

        gridPositionGenerator =
            randomPosition width height
    in
    Random.map4 (initModelHelp width height)
        gridPositionGenerator
        randomDirection
        gridPositionGenerator
        Random.independentSeed


initModelHelp : Int -> Int -> Pos -> Direction -> Pos -> Seed -> Model
initModelHelp w h head direction fruit seed =
    { width = w
    , height = h
    , head = head
    , direction = direction
    , tail = initTail w h head direction
    , fruit = fruit
    , inputDirection = Nothing
    , state = Running
    , ticks = 0
    , seed = seed
    }


initTail : Int -> Int -> Pos -> Direction -> List Pos
initTail w h head direction =
    let
        tailHelp i =
            applyN (i + 1) (step (oppositeDirection direction)) >> warpPosition w h
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
                Running ->
                    case keyToDirection key of
                        Just newInputDirection ->
                            { model | inputDirection = Just newInputDirection }

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
    case
        model.inputDirection
            |> Maybe.andThen
                (\direction ->
                    updateOnTickWithDirection direction model
                )
    of
        Just newModel ->
            newModel

        Nothing ->
            if modBy autoStepSnakeDelay model.ticks == 0 then
                stepSnake model

            else
                { model | ticks = model.ticks + 1 }


updateOnTickWithDirection : Direction -> Model -> Maybe Model
updateOnTickWithDirection direction model =
    if direction /= oppositeDirection model.direction then
        { model | direction = direction, inputDirection = Nothing }
            |> stepSnake
            |> Just

    else
        Nothing


stepSnake : Model -> Model
stepSnake model =
    let
        newHead =
            stepWarpPosition model.direction model.width model.height model.head
    in
    if List.member newHead model.tail then
        { model | state = Over }

    else if newHead == model.fruit then
        let
            ( newFruit, newSeed ) =
                Random.step (randomPosition model.width model.height) model.seed
        in
        { model
            | seed = newSeed
            , fruit = newFruit
            , head = newHead
            , tail = model.head :: model.tail
            , ticks = 1
        }

    else
        { model | head = newHead, tail = model.head :: dropLast model.tail, ticks = 1 }


subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (\_ -> Tick)
        , Browser.Events.onKeyDown (JD.field "key" JD.string |> JD.map OnKeyDown)
        ]



-- VIEW


view : Model -> Html Msg
view model =
    case model.state of
        Running ->
            div
                [ style "display" "grid"
                , style "place-items" "center"
                ]
                [ h1 [] [ text "Running" ]
                , viewBoard model.width model.height model.direction model.head model.tail model.fruit
                ]

        Over ->
            div
                [ style "display" "grid"
                , style "place-items" "center"
                ]
                [ h1 [] [ text "Game Over" ]
                , viewBoard model.width model.height model.direction model.head model.tail model.fruit
                ]


viewBoard : Int -> Int -> Direction -> Pos -> List Pos -> Pos -> Html Msg
viewBoard w h dir head tail fruit =
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
            , style "transform" ("scale(0.8) rotate(" ++ String.fromInt (directionToDegrees dir) ++ "deg)")
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
