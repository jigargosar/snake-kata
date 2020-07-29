module MainV2 exposing (main)

import Browser
import Html exposing (div, h1, text)
import Html.Attributes exposing (style)



-- KATA 2: Without Elm Playground


main : Program () Model msg
main =
    Browser.element
        { init = \_ -> ( { ticks = 0 }, Cmd.none )
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { ticks : Int }


view _ =
    let
        w =
            10

        h =
            20

        cw =
            40

        head =
            ( 5, 9 )

        tail =
            [ ( 5, 10 )
            , ( 5, 11 )
            , ( 5, 12 )
            , ( 5, 13 )
            ]
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
