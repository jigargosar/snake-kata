module MainV2 exposing (main)

import Html exposing (div, h1, text)
import Html.Attributes exposing (style)



-- KATA 2: Without Elm Playground


main =
    div []
        [ h1 [] [ text "View: css grid layout" ]
        , div [ style "display" "grid", style "grid-template" "1fr" ]
            [ text "Cell 1"
            , text "Cell 2"
            ]
        ]
