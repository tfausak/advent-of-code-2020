module Day14Part1 exposing (solve)

import Dict exposing (Dict)
import UInt64 as U64 exposing (UInt64)


solve : String -> String
solve string =
    string
        |> String.lines
        |> List.filterMap stringToInstruction
        |> List.foldl handleInstruction ( identity, Dict.empty )
        |> Tuple.second
        |> Dict.values
        |> List.foldl U64.add U64.zero
        |> U64.toString


type Instruction
    = Mask Mask
    | Mem Int UInt64


type alias Mask =
    UInt64 -> UInt64


type alias Mem =
    Dict Int UInt64


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


stringToMask : String -> Maybe Mask
stringToMask string =
    string
        |> String.toList
        |> List.foldr charToMask ( 0, Just identity )
        |> Tuple.second


charToMask : Char -> ( Int, Maybe Mask ) -> ( Int, Maybe Mask )
charToMask char ( index, maybeMask ) =
    case ( char, maybeMask ) of
        ( '0', Just mask ) ->
            ( index + 1, Just (mask >> U64.setBit index 0) )

        ( '1', Just mask ) ->
            ( index + 1, Just (mask >> U64.setBit index 1) )

        _ ->
            ( index + 1, maybeMask )


handleInstruction : Instruction -> ( Mask, Mem ) -> ( Mask, Mem )
handleInstruction instruction ( mask, memory ) =
    case instruction of
        Mask newMask ->
            ( newMask, memory )

        Mem address value ->
            ( mask, Dict.insert address (mask value) memory )
