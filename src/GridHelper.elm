module GridHelper exposing
    ( GridHelper
    , height
    , init
    , toScreen
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


width : GridHelper -> Float
width g =
    toFloat g.width * g.cellWidth


height : GridHelper -> Float
height g =
    toFloat g.height * g.cellWidth


toScreen : Int -> Int -> GridHelper -> ( Float, Float )
toScreen x y g =
    ( toFloat x * g.cellWidth + g.cellWidth / 2
    , toFloat y * g.cellWidth + g.cellWidth / 2
    )
