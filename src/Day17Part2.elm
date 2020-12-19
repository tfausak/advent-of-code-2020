module Day17Part2 exposing (solve)

import Set exposing (Set)


type alias Point =
    ( ( Int, Int ), ( Int, Int ) )


solve : String -> String
solve string =
    string
        |> parseGrid
        |> apply step 6
        |> Set.size
        |> String.fromInt


parseGrid : String -> Set Point
parseGrid string =
    string
        |> String.words
        |> List.indexedMap parseRow
        |> List.concat
        |> Set.fromList


parseRow : Int -> String -> List Point
parseRow y string =
    string
        |> String.toList
        |> List.indexedMap (parseCell y)
        |> List.filterMap identity


parseCell : Int -> Int -> Char -> Maybe Point
parseCell y x char =
    if char == '#' then
        Just ( ( x, y ), ( 0, 0 ) )

    else
        Nothing


apply : (a -> a) -> Int -> a -> a
apply f n x =
    if n < 1 then
        x

    else
        apply f (n - 1) (f x)


step : Set Point -> Set Point
step points =
    points
        |> candidates
        |> Set.filter (isActive points)


candidates : Set Point -> Set Point
candidates points =
    points
        |> Set.toList
        |> List.concatMap neighbors
        |> Set.fromList
        |> Set.union points


neighbors : Point -> List Point
neighbors ( ( x, y ), ( z, w ) ) =
    let
        ds =
            [ -1, 0, 1 ]

        fw dx dy dz dw =
            if dx == 0 && dy == 0 && dz == 0 && dw == 0 then
                Nothing

            else
                Just ( ( x + dx, y + dy ), ( z + dz, w + dw ) )

        fz dx dy dz =
            List.filterMap (fw dx dy dz) ds

        fy dx dy =
            List.concatMap (fz dx dy) ds

        fx dx =
            List.concatMap (fy dx) ds
    in
    List.concatMap fx ds


isActive : Set Point -> Point -> Bool
isActive points point =
    let
        count =
            point
                |> neighbors
                |> Set.fromList
                |> Set.intersect points
                |> Set.size
    in
    if Set.member point points then
        2 <= count && count <= 3

    else
        count == 3
