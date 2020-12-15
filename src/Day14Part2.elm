module Day14Part2 exposing (solve)

import Dict exposing (Dict)
import UInt64 exposing (UInt64)


solve : String -> String
solve string =
    string
        |> String.lines
        |> List.filterMap stringToInstruction
        |> List.foldl handleInstruction ( [], Dict.empty )
        |> Tuple.second
        |> Dict.values
        |> List.foldl UInt64.add UInt64.zero
        |> UInt64.toString


type Instruction
    = Mask Mask
    | Mem UInt64 UInt64


type alias Mask =
    List ( Int, Bit )


type Bit
    = B0
    | B1
    | BX


type alias Memory =
    Dict String UInt64


stringToInstruction : String -> Maybe Instruction
stringToInstruction line =
    case String.words line of
        [ "mask", "=", mask ] ->
            mask
                |> String.reverse
                |> String.toList
                |> List.indexedMap Tuple.pair
                |> List.filterMap (mapSecondMaybe charToBit)
                |> Mask
                |> Just

        [ address, "=", value ] ->
            Maybe.map2 Mem
                (UInt64.fromString (String.slice 4 -1 address))
                (UInt64.fromString value)

        _ ->
            Nothing


mapSecondMaybe : (b -> Maybe c) -> ( a, b ) -> Maybe ( a, c )
mapSecondMaybe f ( x, y ) =
    Maybe.map (Tuple.pair x) (f y)


charToBit : Char -> Maybe Bit
charToBit char =
    case char of
        '0' ->
            Just B0

        '1' ->
            Just B1

        'X' ->
            Just BX

        _ ->
            Nothing


handleInstruction : Instruction -> ( Mask, Memory ) -> ( Mask, Memory )
handleInstruction instruction ( mask, memory ) =
    case instruction of
        Mask newMask ->
            ( newMask, memory )

        Mem address value ->
            ( mask
            , List.foldl
                (\key -> Dict.insert (UInt64.toHexString key) value)
                memory
                (applyMask mask address)
            )


applyMask : List ( Int, Bit ) -> UInt64 -> List UInt64
applyMask mask startingBase =
    mask
        |> List.foldl handleBit ( [], startingBase )
        |> makeAddresses


handleBit : ( Int, Bit ) -> ( List Int, UInt64 ) -> ( List Int, UInt64 )
handleBit ( index, bit ) ( indexes, base ) =
    case bit of
        B0 ->
            ( indexes, base )

        B1 ->
            ( indexes, UInt64.setBit index 1 base )

        BX ->
            ( index :: indexes, base )


makeAddresses : ( List Int, UInt64 ) -> List UInt64
makeAddresses ( indexes, base ) =
    makeAddressesWith indexes [ base ]


makeAddressesWith : List Int -> List UInt64 -> List UInt64
makeAddressesWith indexes addresses =
    case indexes of
        [] ->
            addresses

        index :: rest ->
            makeAddressesWith rest (List.concatMap (setBits index) addresses)


setBits : Int -> UInt64 -> List UInt64
setBits index address =
    List.map (\value -> UInt64.setBit index value address) [ 0, 1 ]
