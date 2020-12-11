module Day10 exposing (part1, part2)

import Dict exposing (Dict)
import Dict.Extra
import List.Extra


part1 : String -> String
part1 string =
    string
        |> handleInput
        |> Dict.Extra.frequencies
        |> (\x -> Maybe.map2 (*) (Dict.get 1 x) (Dict.get 3 x))
        |> Maybe.map String.fromInt
        |> Maybe.withDefault ":("


handleInput : String -> List Int
handleInput string =
    string
        |> String.lines
        |> List.filterMap String.toInt
        |> List.sort
        |> addBounds
        |> pairs
        |> List.map (\( lo, hi ) -> hi - lo)


addBounds : List Int -> List Int
addBounds xs =
    case List.maximum xs of
        Nothing ->
            xs

        Just x ->
            0 :: xs ++ [ x + 3 ]


pairs : List a -> List ( a, a )
pairs list =
    List.map2 Tuple.pair list (List.drop 1 list)


part2 : String -> String
part2 string =
    string
        |> handleInput
        |> List.Extra.group
        |> List.filter (\( x, _ ) -> x == 1)
        |> List.map (\( _, xs ) -> tribonacci (List.length xs + 1))
        |> List.product
        |> String.fromInt


tribonacci : Int -> Int
tribonacci n =
    if n < 0 then
        0

    else if n < 2 then
        1

    else
        tribonacci (n - 1) + tribonacci (n - 2) + tribonacci (n - 3)
