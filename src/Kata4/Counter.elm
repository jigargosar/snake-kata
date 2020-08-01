module Kata4.Counter exposing (Counter, init)


type alias Counter =
    { limit : Int
    , current : Int
    }


init : Int -> Counter
init limit =
    { limit = limit
    , current = 0
    }
