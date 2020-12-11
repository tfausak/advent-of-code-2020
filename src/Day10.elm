module Day10 exposing (part1, part2)

import Dict exposing (Dict)
import Dict.Extra
import List.Extra


part1 : String -> String
part1 string =
    string
        |> String.lines
        |> List.filterMap String.toInt
        |> List.sort
        |> (::) 0
        |> pairs
        |> List.map (\( lo, hi ) -> hi - lo)
        |> Dict.Extra.frequencies
        |> solve
        |> Maybe.map String.fromInt
        |> Maybe.withDefault ":("


part2 : String -> String
part2 string =
    string
        |> String.lines
        |> List.filterMap String.toInt
        |> List.sort
        |> (::) 0
        |> pairs
        |> List.map (\( lo, hi ) -> hi - lo)
        |> List.Extra.group
        |> List.filter (\( x, _ ) -> x == 1)
        |> List.map (\( _, xs ) -> List.length xs + 1)
        |> List.map (\x -> tribonacci (x + 1))
        |> List.product
        |> String.fromInt


tribonacci : Int -> Int
tribonacci n =
    if n < 1 then
        0

    else if n < 3 then
        1

    else
        tribonacci (n - 1) + tribonacci (n - 2) + tribonacci (n - 3)


solve : Dict Int Int -> Maybe Int
solve differences =
    Maybe.map2
        (\ones threes -> ones * (threes + 1))
        (Dict.get 1 differences)
        (Dict.get 3 differences)


pairs : List a -> List ( a, a )
pairs list =
    List.map2 Tuple.pair list (List.drop 1 list)
