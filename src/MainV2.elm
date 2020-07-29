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
    in
    div []
        [ h1 [] [ text "View: css grid layout" ]
        , div
            [ style "display" "grid"
            , style "grid-template" " repeat(20, 50px) / repeat(10, 50px) "
            , style "gap" "20px"
            , style "padding" "20px"
            ]
            (viewNCells (w * h))
        ]


viewNCells n =
    let
        viewNthCell nth =
            div
                [ style "border" "1px dashed"
                , style "overflow" "hidden"
                ]
                [ text ("Cell " ++ String.fromInt nth)
                , text "More text to check overflow and sizing"
                ]
    in
    List.range 1 n
        |> List.map viewNthCell
