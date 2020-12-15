module Day15 exposing (part1, part2)

import IntDict exposing (IntDict)
import List.Extra


part1 : String -> String
part1 =
    solveWith 2020


part2 : String -> String
part2 =
    -- TODO: Too slow! Have 300,000 but want 30,000,000.
    solveWith 300000


solveWith : Int -> String -> String
solveWith limit string =
    string
        |> String.trim
        |> String.split ","
        |> List.filterMap String.toInt
        |> List.indexedMap (\index number -> ( number, ( index + 1, Nothing ) ))
        |> IntDict.fromList
        |> playGame limit
        |> Maybe.map String.fromInt
        |> Maybe.withDefault ":("


playGame : Int -> IntDict ( Int, Maybe Int ) -> Maybe Int
playGame limit seen =
    seen
        |> IntDict.toList
        |> List.Extra.maximumBy (Tuple.second >> Tuple.first)
        |> Maybe.map (playGameWith limit seen)


playGameWith : Int -> IntDict ( Int, Maybe Int ) -> ( Int, ( Int, Maybe Int ) ) -> Int
playGameWith limit seen ( number, ( turn, _ ) ) =
    takeTurn limit number (turn + 1) seen


takeTurn : Int -> Int -> Int -> IntDict ( Int, Maybe Int ) -> Int
takeTurn limit number turn seen =
    let
        nextNumber =
            case IntDict.get number seen of
                Just ( x, Just y ) ->
                    x - y

                _ ->
                    0

        nextSeen =
            IntDict.update
                nextNumber
                (\m -> Just ( turn, Maybe.map Tuple.first m ))
                seen
    in
    if turn > limit then
        number

    else
        takeTurn limit nextNumber (turn + 1) nextSeen
