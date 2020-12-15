module Day14Part2 exposing (solve)
import Dict
import UInt64 exposing (UInt64)

solve : String -> String
solve string =
    string
        |> String.lines
        |> List.filterMap (\ line -> case String.words line of
            [ "mask", "=", mask ] -> mask
                |> String.reverse
                |> String.toList
                |> List.indexedMap Tuple.pair
                |> List.filterMap (\ (index, char) -> case char of
                    '0' -> Just (index, B0)
                    '1' -> Just (index, B1)
                    'X' -> Just (index, BX)
                    _ -> Nothing)
                |> Mask
                |> Just
            [ address, "=", value ] -> Maybe.map2 Mem
                (UInt64.fromString (String.slice 4 -1 address))
                (UInt64.fromString value)
            _ -> Nothing)
        |> List.foldl
            (\ instruction (mask, memory) -> case instruction of
                Mask newMask -> ( newMask, memory )
                Mem address value ->
                    ( mask, List.foldl
                        (\ key -> Dict.insert (UInt64.toHexString key) value)
                        memory
                        (f1 mask address) ))
            ([], Dict.empty)
        |> Tuple.second
        |> Debug.log "TODO"
        |> Dict.values
        |> List.foldl UInt64.add UInt64.zero
        |> UInt64.toString

type Instruction
    = Mask (List (Int, Bit))
    | Mem UInt64 UInt64

type Bit = B0 | B1 | BX

f1 : List (Int, Bit) -> UInt64 -> List UInt64
f1 bits startingBase = bits
    |> List.foldl (\ (index, bit) (indexes, base) -> case bit of
        B0 -> ( indexes, base )
        B1 -> ( indexes, UInt64.setBit index 1 base )
        BX -> ( index :: indexes, base ))
        ([], startingBase)
    |> f2

f2 : (List Int, UInt64) -> List UInt64
f2 ( indexes, base ) = f3 indexes [base]

f3 : List Int -> List UInt64 -> List UInt64
f3 indexes addresses = case indexes of
    [] -> addresses
    index :: rest -> f3 rest (List.concatMap
        (\ address -> List.map (\ value -> UInt64.setBit index value address) [0, 1])
        addresses)
