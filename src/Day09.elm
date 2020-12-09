module Day09 exposing (part1, part2)

import Array exposing (Array)
import List.Extra


part1 : String -> String
part1 string =
    string
        |> String.lines
        |> List.filterMap String.toInt
        |> Array.fromList
        |> findInvalidNumber preambleSize
        |> Maybe.map String.fromInt
        |> Maybe.withDefault ":("


preambleSize : Int
preambleSize =
    25


findInvalidNumber : Int -> Array Int -> Maybe Int
findInvalidNumber index all =
    Array.get index all
        |> Maybe.andThen
            (\number ->
                if isGood number (Array.slice (index - preambleSize) index all) then
                    findInvalidNumber (index + 1) all

                else
                    Just number
            )


isGood : Int -> Array Int -> Bool
isGood target numbers =
    Array.toList numbers
        |> List.Extra.uniquePairs
        |> List.any (\( x, y ) -> x + y == target)


part2 : String -> String
part2 string =
    let
        numbers =
            string
                |> String.lines
                |> List.filterMap String.toInt
                |> Array.fromList
    in
    findInvalidNumber preambleSize numbers
        |> Maybe.andThen (\target -> findEncryptionWeakness target numbers)
        |> Maybe.map (\( lo, hi ) -> String.fromInt (lo + hi))
        |> Maybe.withDefault ":("


findEncryptionWeakness : Int -> Array Int -> Maybe ( Int, Int )
findEncryptionWeakness =
    findEncryptionWeaknessWith 0 1


findEncryptionWeaknessWith : Int -> Int -> Int -> Array Int -> Maybe ( Int, Int )
findEncryptionWeaknessWith from to target numbers =
    let
        slice =
            Array.toList (Array.slice from to numbers)
    in
    case compare (List.sum slice) target of
        LT ->
            findEncryptionWeaknessWith from (to + 1) target numbers

        EQ ->
            Maybe.map2 Tuple.pair (List.minimum slice) (List.maximum slice)

        GT ->
            findEncryptionWeaknessWith (from + 1) (from + 2) target numbers
