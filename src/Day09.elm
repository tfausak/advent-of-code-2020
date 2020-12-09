module Day09 exposing (part1, part2)

import Array exposing (Array)
import List.Extra


part1 : String -> String
part1 string =
    string
        |> String.lines
        |> List.filterMap String.toInt
        |> Array.fromList
        |> solveAt 25
        |> Maybe.map String.fromInt
        |> Maybe.withDefault ":("


solveAt : Int -> Array Int -> Maybe Int
solveAt index all =
    Array.get index all
        |> Maybe.andThen
            (\number ->
                if isGood number (Array.slice (index - 25) index all) then
                    solveAt (index + 1) all

                else
                    Just number
            )


isGood : Int -> Array Int -> Bool
isGood target numbers =
    Array.toList numbers
        |> List.Extra.uniquePairs
        |> List.any (\( x, y ) -> x + y == target)


part2 : String -> String
part2 _ =
    "d9p2"
