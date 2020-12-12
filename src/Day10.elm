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
        |> addBounds
        |> pairs
        |> List.map (\( lo, hi ) -> hi - lo)
        |> Dict.Extra.frequencies
        |> (\x -> Maybe.map2 (*) (Dict.get 1 x) (Dict.get 3 x))
        |> Maybe.map String.fromInt
        |> Maybe.withDefault ":("


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
        |> String.lines
        |> List.filterMap String.toInt
        |> List.sort
        |> List.foldl step (Dict.singleton 0 1)
        |> Dict.values
        |> List.Extra.last
        |> Maybe.map String.fromInt
        |> Maybe.withDefault ":("


step : Int -> Dict Int Int -> Dict Int Int
step n dict =
    let
        at d =
            Dict.get (n - d) dict
                |> Maybe.withDefault 0
    in
    Dict.insert n (at 1 + at 2 + at 3) dict
