module More exposing (..)


applyN : Int -> (a -> a) -> a -> a
applyN n f x =
    if n <= 0 then
        x

    else
        applyN (n - 1) f (f x)


dropLast : List a -> List a
dropLast =
    List.reverse >> List.drop 1 >> List.reverse
