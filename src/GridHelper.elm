module GridHelper exposing (GridHelper, init, size)


type alias GridHelper =
    { width : Int
    , height : Int
    , cellWidth : Float
    }


init : Int -> Int -> Float -> GridHelper
init w h cw =
    { width = w
    , height = h
    , cellWidth = cw
    }


size : GridHelper -> { width : Float, height : Float }
size gg =
    { width = toFloat gg.width * gg.cellWidth
    , height = toFloat gg.height * gg.cellWidth
    }
