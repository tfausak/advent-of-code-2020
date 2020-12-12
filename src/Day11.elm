module Day11 exposing (part1, part2)

import Array exposing (Array)
import List.Extra


part1 : String -> String
part1 =
    solveWith stepper1


part2 : String -> String
part2 =
    solveWith stepper2


solveWith : (Grid Seat -> Int -> Int -> Seat -> Seat) -> String -> String
solveWith stepper string =
    string
        |> String.words
        |> List.map
            (\line ->
                line
                    |> String.toList
                    |> List.filterMap charToSeat
                    |> Array.fromList
            )
        |> Array.fromList
        |> fixedPoint (stepWith stepper)
        |> Array.map
            (\row ->
                row
                    |> Array.toList
                    |> List.Extra.count isOccupied
            )
        |> Array.toList
        |> List.sum
        |> String.fromInt


charToSeat : Char -> Maybe Seat
charToSeat char =
    case char of
        '.' ->
            Just Floor

        'L' ->
            Just Empty

        '#' ->
            Just Occupied

        _ ->
            Nothing


fixedPoint : (a -> a) -> a -> a
fixedPoint f old =
    let
        new =
            f old
    in
    if new == old then
        new

    else
        fixedPoint f new


stepWith : (Grid Seat -> Int -> Int -> Seat -> Seat) -> Grid Seat -> Grid Seat
stepWith stepper seats =
    Array.indexedMap (\index -> Array.indexedMap (stepper seats index)) seats


isOccupied : Seat -> Bool
isOccupied seat =
    seat == Occupied


stepper1 : Grid Seat -> Int -> Int -> Seat -> Seat
stepper1 seats row column seat =
    let
        neighbors () =
            getNeighbors seats row column (always True)
    in
    case seat of
        Floor ->
            Floor

        Empty ->
            if List.any isOccupied (neighbors ()) then
                Empty

            else
                Occupied

        Occupied ->
            if List.Extra.count isOccupied (neighbors ()) >= 4 then
                Empty

            else
                Occupied


stepper2 : Grid Seat -> Int -> Int -> Seat -> Seat
stepper2 seats row column seat =
    let
        neighbors () =
            getNeighbors seats row column ((/=) Floor)
    in
    case seat of
        Floor ->
            Floor

        Empty ->
            if List.any isOccupied (neighbors ()) then
                Empty

            else
                Occupied

        Occupied ->
            if List.Extra.count isOccupied (neighbors ()) >= 5 then
                Empty

            else
                Occupied


getNeighbors : Grid a -> Int -> Int -> (a -> Bool) -> List a
getNeighbors grid row column p =
    List.filterMap (findFirst grid row column p) directions


findFirst : Grid a -> Int -> Int -> (a -> Bool) -> Direction -> Maybe a
findFirst grid row column p direction =
    let
        ( newRow, newColumn ) =
            move row column direction
    in
    getAt grid newRow newColumn
        |> Maybe.andThen
            (\x ->
                if p x then
                    Just x

                else
                    findFirst grid newRow newColumn p direction
            )


move : Int -> Int -> Direction -> ( Int, Int )
move row column direction =
    case direction of
        N ->
            ( row + 1, column )

        NE ->
            ( row + 1, column + 1 )

        E ->
            ( row, column + 1 )

        SE ->
            ( row - 1, column + 1 )

        S ->
            ( row - 1, column )

        SW ->
            ( row - 1, column - 1 )

        W ->
            ( row, column - 1 )

        NW ->
            ( row + 1, column - 1 )


getAt : Grid a -> Int -> Int -> Maybe a
getAt grid row column =
    grid
        |> Array.get row
        |> Maybe.andThen (Array.get column)


directions : List Direction
directions =
    [ N, NE, E, SE, S, SW, W, NW ]


type alias Grid a =
    Array (Array a)


type Seat
    = Floor
    | Empty
    | Occupied


type Direction
    = N
    | NE
    | E
    | SE
    | S
    | SW
    | W
    | NW
