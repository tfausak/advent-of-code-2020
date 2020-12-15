module Day15 exposing (part1, part2)

import Dict exposing (Dict)
import List.Extra


part1 : String -> String
part1 =
    solveWith 2020


part2 : String -> String
part2 =
    solveWith 30000000


solveWith : Int -> String -> String
solveWith limit string =
    string
        |> String.trim
        |> String.split ","
        |> List.filterMap String.toInt
        |> List.indexedMap (\index number -> ( number, [ index + 1 ] ))
        |> Dict.fromList
        |> playGame limit
        |> Maybe.map String.fromInt
        |> Maybe.withDefault ":("


playGame : Int -> Dict Int (List Int) -> Maybe Int
playGame limit seen =
    seen
        |> Dict.toList
        |> List.Extra.maximumBy Tuple.second
        |> Maybe.andThen (playGameWith limit seen)


playGameWith : Int -> Dict Int (List Int) -> ( Int, List Int ) -> Maybe Int
playGameWith limit seen ( number, turns ) =
    case turns of
        [] ->
            Nothing

        turn :: _ ->
            Just (takeTurn limit number (turn + 1) seen)


takeTurn : Int -> Int -> Int -> Dict Int (List Int) -> Int
takeTurn limit number turn seen =
    let
        nextNumber =
            case Dict.get number seen of
                Just (x :: y :: _) ->
                    x - y

                _ ->
                    0

        nextSeen =
            Dict.update nextNumber (Maybe.withDefault [] >> (::) turn >> List.take 2 >> Just) seen
    in
    if turn > limit then
        number

    else
        takeTurn limit nextNumber (turn + 1) nextSeen
