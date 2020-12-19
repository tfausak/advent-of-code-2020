module Day19 exposing (part1, part2)

import Dict exposing (Dict)
import Elf
import List.Extra
import Maybe.Extra
import Regex exposing (Regex)


type alias Input =
    { matches : Dict Int Match
    , messages : List String
    }


type alias Rule =
    { number : Int
    , match : Match
    }


type Match
    = Single Char
    | Multiple (List (List Int))


part1 : String -> String
part1 string =
    string
        |> String.split "\n\n"
        |> Elf.listToTuple
        |> Maybe.andThen parseInput
        |> Maybe.map
            (\input ->
                let
                    regex =
                        toRegex 10 input.matches 0
                in
                input.messages
                    |> List.Extra.count (Regex.contains regex)
                    |> String.fromInt
            )
        |> Maybe.withDefault "Failed to parse input!"


part2 : String -> String
part2 string =
    string
        |> String.split "\n\n"
        |> Elf.listToTuple
        |> Maybe.andThen parseInput
        |> Maybe.map
            (\input ->
                let
                    matches =
                        input.matches
                            |> Dict.insert 8 (Multiple [ [ 42 ], [ 42, 8 ] ])
                            |> Dict.insert 11 (Multiple [ [ 42, 31 ], [ 42, 11, 31 ] ])

                    regex =
                        toRegex 14 matches 0
                in
                input.messages
                    |> List.Extra.count (Regex.contains regex)
                    |> String.fromInt
            )
        |> Maybe.withDefault "Failed to parse input!"


toRegex : Int -> Dict Int Match -> Int -> Regex
toRegex limit matches number =
    toString limit 0 matches number
        |> Maybe.andThen
            (\string ->
                string
                    |> Elf.around "^" "$"
                    |> Regex.fromString
            )
        |> Maybe.withDefault Regex.never


toString : Int -> Int -> Dict Int Match -> Int -> Maybe String
toString limit depth matches number =
    if depth > limit then
        Just ""

    else
        case Dict.get number matches of
            Nothing ->
                Nothing

            Just (Single char) ->
                Just (String.fromChar char)

            Just (Multiple groups) ->
                groups
                    |> Maybe.Extra.traverse
                        (\numbers ->
                            numbers
                                |> Maybe.Extra.traverse (toString limit (depth + 1) matches)
                                |> Maybe.map String.concat
                        )
                    |> Maybe.map
                        (\string ->
                            string
                                |> String.join "|"
                                |> Elf.around "(" ")"
                        )


parseInput : ( String, String ) -> Maybe Input
parseInput ( rules, messages ) =
    Maybe.map2 Input
        (parseRules rules)
        (parseMessages messages)


parseRules : String -> Maybe (Dict Int Match)
parseRules string =
    string
        |> String.lines
        |> Maybe.Extra.traverse parseRule
        |> Maybe.map toMatches


toMatches : List Rule -> Dict Int Match
toMatches rules =
    rules
        |> List.map (\rule -> ( rule.number, rule.match ))
        |> Dict.fromList


parseRule : String -> Maybe Rule
parseRule string =
    string
        |> String.split ": "
        |> Elf.listToTuple
        |> Maybe.andThen parseRuleWith


parseRuleWith : ( String, String ) -> Maybe Rule
parseRuleWith ( number, match ) =
    Maybe.map2 Rule
        (String.toInt number)
        (parseMatch match)


parseMatch : String -> Maybe Match
parseMatch string =
    case String.toList string of
        [ '"', char, '"' ] ->
            Just (Single char)

        _ ->
            string
                |> String.split " | "
                |> Maybe.Extra.traverse parseSubRules
                |> Maybe.map Multiple


parseSubRules : String -> Maybe (List Int)
parseSubRules string =
    string
        |> String.words
        |> Maybe.Extra.traverse String.toInt


parseMessages : String -> Maybe (List String)
parseMessages string =
    string
        |> String.lines
        |> List.filter (\line -> not (String.isEmpty line))
        |> Just
