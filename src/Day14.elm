module Day14 exposing (part1, part2)

import Dict
import UInt64 as U64 exposing (UInt64)


part1 : String -> String
part1 string =
    string
        |> String.lines
        |> List.filterMap stringToInstruction
        |> List.foldl
            (\instruction ( mask, memory ) ->
                case instruction of
                    Mask newMask ->
                        ( newMask, memory )

                    Mem address value ->
                        ( mask, Dict.insert address (mask value) memory )
            )
            ( identity, Dict.empty )
        |> Tuple.second
        |> Dict.values
        |> List.foldl U64.add U64.zero
        |> U64.toString


type Instruction
    = Mask (UInt64 -> UInt64)
    | Mem Int UInt64


stringToInstruction : String -> Maybe Instruction
stringToInstruction string =
    case String.words string of
        [ "mask", "=", mask ] ->
            Maybe.map Mask
                (stringToMask mask)

        [ address, "=", value ] ->
            Maybe.map2 Mem
                (String.toInt (String.slice 4 -1 address))
                (Maybe.map U64.fromInt (String.toInt value))

        _ ->
            Nothing


stringToMask : String -> Maybe (UInt64 -> UInt64)
stringToMask string =
    string
        |> String.reverse
        |> String.toList
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( index, char ) maybeBitmask ->
                case ( char, maybeBitmask ) of
                    ( 'X', _ ) ->
                        maybeBitmask

                    ( '1', Just bitmask ) ->
                        Just (bitmask >> U64.setBit index 1)

                    ( '0', Just bitmask ) ->
                        Just (bitmask >> U64.setBit index 0)

                    _ ->
                        Nothing
            )
            (Just identity)


part2 : String -> String
part2 _ =
    "TODO day 14 part 2"
