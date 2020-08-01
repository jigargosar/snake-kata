module Kata4.Counter exposing (Counter, done, reset, step, upto)


type alias Counter =
    { limit : Int
    , current : Int
    }


upto : Int -> Counter
upto limit =
    { limit = limit
    , current = 0
    }


step : Counter -> Counter
step counter =
    { counter | current = counter.current + 1 }


done : Counter -> Bool
done counter =
    counter.current >= counter.limit


reset : Counter -> Counter
reset counter =
    { counter | current = 0 }
