module Kata4.Counter exposing (Counter, done, restart, start, step)


type alias Counter =
    { limit : Int
    , current : Int
    }


start : Int -> Counter
start limit =
    { limit = limit
    , current = 0
    }


step : Counter -> Counter
step counter =
    { counter | current = counter.current + 1 }


done : Counter -> Bool
done counter =
    counter.current >= counter.limit


restart : Counter -> Counter
restart counter =
    { counter | current = 0 }
