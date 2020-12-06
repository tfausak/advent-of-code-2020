module Day01 exposing (part1, part2)

import List.Extra
import Maybe.Extra


part1 : String -> String
part1 input =
    input
        |> parse
        |> Maybe.map List.Extra.uniquePairs
        |> Maybe.andThen (List.Extra.find (\( x, y ) -> x + y == 2020))
        |> Maybe.map (\( x, y ) -> x * y)
        |> Maybe.Extra.unwrap "unknown" String.fromInt


parse : String -> Maybe (List Int)
parse input =
    input
        |> String.lines
        |> List.Extra.filterNot String.isEmpty
        |> Maybe.Extra.traverse String.toInt


part2 : String -> String
part2 input =
    input
        |> parse
        |> Maybe.map uniqueTriples
        |> Maybe.andThen (List.Extra.find (\( x, y, z ) -> x + y + z == 2020))
        |> Maybe.map (\( x, y, z ) -> x * y * z)
        |> Maybe.Extra.unwrap "unknown" String.fromInt


uniqueTriples : List a -> List ( a, a, a )
uniqueTriples xs =
    case xs of
        [] ->
            []

        x :: ys ->
            List.map (\( y, z ) -> ( x, y, z )) (List.Extra.uniquePairs ys)
                ++ uniqueTriples ys
