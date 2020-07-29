module MainV2 exposing (main)

import Html exposing (div, h1, text)
import Html.Attributes exposing (style)



-- KATA 2: Without Elm Playground


main =
    let
        w =
            10

        h =
            20

        cw =
            50
    in
    div []
        [ h1 [] [ text "View: css grid layout" ]
        , div
            [ style "display" "grid"
            , style "grid-template-rows" (" repeat(" ++ String.fromInt h ++ ", " ++ String.fromFloat cw ++ "px) ")
            , style "grid-template-columns" (" repeat(" ++ String.fromInt w ++ ", " ++ String.fromFloat cw ++ "px) ")
            , style "gap" "1px"
            , style "padding" "20px"
            ]
            --(viewNCells (w * h))
            (viewGridCells w h)
        ]


viewGridCells w h =
    let
        positions =
            List.range 1 w
                |> List.concatMap (\x -> List.range 1 h |> List.map (Tuple.pair x))

        viewXYCell ( x, y ) =
            div
                [ style "overflow" "hidden"
                , style "grid-row" (String.fromInt y)
                , style "grid-column" (String.fromInt x)
                , style "background-color" "#444"
                , style "color" "#fff"
                , style "border-radius" "20%"
                ]
                [ text (String.fromInt x ++ ", " ++ String.fromInt y)
                ]
    in
    List.map viewXYCell positions


viewNCells n =
    let
        viewNthCell nth =
            div
                [ style "border" "1px dashed"
                , style "overflow" "hidden"
                ]
                [ text (String.fromInt nth)
                ]
    in
    List.range 1 n
        |> List.map viewNthCell
