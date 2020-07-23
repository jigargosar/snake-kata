module GridHelper exposing
    ( GridHelper
    , height
    , init
    , size
    , width
    )


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
size g =
    { width = width g
    , height = height g
    }


width : GridHelper -> Float
width g =
    toFloat g.width * g.cellWidth


height : GridHelper -> Float
height g =
    toFloat g.height * g.cellWidth
