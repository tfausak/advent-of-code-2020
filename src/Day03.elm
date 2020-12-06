module Day03 exposing (part1, part2)

import Array exposing (Array)


part1 : String -> String
part1 =
    toGrid >> solve ( 3, 1 ) >> String.fromInt


toGrid : String -> Array (Array Char)
toGrid =
    toLines >> List.map stringToArray >> Array.fromList


toLines : String -> List String
toLines =
    String.trimRight >> String.lines


stringToArray : String -> Array Char
stringToArray =
    String.toList >> Array.fromList


solve : ( Int, Int ) -> Array (Array Char) -> Int
solve =
    solveWith ( 0, 0 ) 0


solveWith : ( Int, Int ) -> Int -> ( Int, Int ) -> Array (Array Char) -> Int
solveWith ( x, y ) trees ( dx, dy ) grid =
    if y > Array.length grid then
        trees

    else
        let
            newTrees =
                case getAt x y grid of
                    Just '#' ->
                        trees + 1

                    _ ->
                        trees
        in
        solveWith ( x + dx, y + dy ) newTrees ( dx, dy ) grid


getAt : Int -> Int -> Array (Array a) -> Maybe a
getAt x y g =
    Array.get y g
        |> Maybe.andThen
            (\line ->
                Array.get (modBy (Array.length line) x) line
            )


part2 : String -> String
part2 string =
    let
        grid =
            toGrid string
    in
    [ ( 1, 1 ), ( 3, 1 ), ( 5, 1 ), ( 7, 1 ), ( 1, 2 ) ]
        |> List.map (\slope -> solve slope grid)
        |> List.product
        |> String.fromInt
