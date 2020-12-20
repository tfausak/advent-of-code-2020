module Elf exposing (..)


around : String -> String -> String -> String
around before after string =
    before ++ string ++ after


decrement : number -> number
decrement x =
    x - 1


findMap : (a -> Maybe b) -> List a -> Maybe b
findMap f xs =
    case xs of
        [] ->
            Nothing

        x :: ys ->
            case f x of
                Nothing ->
                    findMap f ys

                Just y ->
                    Just y


increment : number -> number
increment x =
    x + 1


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


mapFirst : (a -> c) -> ( a, b ) -> ( c, b )
mapFirst f =
    mapBoth f identity


mapSecond : (b -> c) -> ( a, b ) -> ( a, c )
mapSecond f =
    mapBoth identity f


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( x, y ) =
    f x y
