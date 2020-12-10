module Day10 exposing (part1, part2)

import Dict exposing (Dict)
import Dict.Extra


part1 : String -> String
part1 string =
    string
        |> String.lines
        |> List.filterMap String.toInt
        |> List.sort
        |> (\jolts -> 0 :: jolts)
        |> pairs
        |> List.map (\( lo, hi ) -> hi - lo)
        |> Dict.Extra.frequencies
        |> solve
        |> Maybe.map String.fromInt
        |> Maybe.withDefault ":("


part2 : String -> String
part2 _ =
    "TODO day 10 part 2"


solve : Dict Int Int -> Maybe Int
solve differences =
    Maybe.map2
        (\ones threes -> ones * (threes + 1))
        (Dict.get 1 differences)
        (Dict.get 3 differences)


pairs : List a -> List ( a, a )
pairs list =
    List.map2 Tuple.pair list (List.drop 1 list)
