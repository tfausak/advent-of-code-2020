module Elf exposing (..)


around : String -> String -> String -> String
around before after string =
    before ++ string ++ after


listToTuple : List a -> Maybe ( a, a )
listToTuple l =
    case l of
        [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing


mapBoth : (a -> c) -> (b -> d) -> ( a, b ) -> ( c, d )
mapBoth f g ( x, y ) =
    ( f x, g y )


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( x, y ) =
    f x y
