module MainV2 exposing (main)

import Html exposing (div, h1, text)
import Html.Attributes exposing (style)



-- KATA 2: Without Elm Playground


main =
    div []
        [ h1 [] [ text "View: css grid layout" ]
        , div
            [ style "display" "grid"
            , style "grid-template" "1fr 1fr / 1fr 1fr"
            , style "gap" "20px"
            , style "padding" "20px"
            ]
            (viewNCells 10)
        ]


viewNCells n =
    let
        viewNthCell nth =
            div [ style "border" "1px dashed" ] [ text ("Cell " ++ String.fromInt nth) ]
    in
    List.range 1 n
        |> List.map viewNthCell
