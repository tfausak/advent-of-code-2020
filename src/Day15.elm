module Day15 exposing (part1, part2)

import Dict exposing (Dict)
import List.Extra


part1 : String -> String
part1 string =
    string
        |> String.trim
        |> String.split ","
        |> List.filterMap String.toInt
        |> List.indexedMap (\index number -> ( number, [ index + 1 ] ))
        |> Dict.fromList
        |> playGame
        |> Maybe.map String.fromInt
        |> Maybe.withDefault ":("


playGame : Dict Int (List Int) -> Maybe Int
playGame seen =
    seen
        |> Dict.toList
        |> List.Extra.maximumBy Tuple.second
        |> Maybe.andThen (playGameWith seen)


playGameWith : Dict Int (List Int) -> ( Int, List Int ) -> Maybe Int
playGameWith seen ( number, turns ) =
    case turns of
        [] ->
            Nothing

        turn :: _ ->
            Just (takeTurn number (turn + 1) seen)


takeTurn : Int -> Int -> Dict Int (List Int) -> Int
takeTurn number turn seen =
    let
        nextNumber =
            case Dict.get number seen of
                Just (x :: y :: _) ->
                    x - y

                _ ->
                    0

        nextSeen =
            Dict.update nextNumber (Maybe.withDefault [] >> (::) turn >> Just) seen
    in
    if turn > 2020 then
        number

    else
        takeTurn nextNumber (turn + 1) nextSeen


part2 : String -> String
part2 _ =
    "TODO day 15 part 2"
