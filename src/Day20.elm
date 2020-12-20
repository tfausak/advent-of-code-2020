module Day20 exposing (part1, part2)

import Array exposing (Array)
import Dict exposing (Dict)
import Elf
import Maybe.Extra
import String.Extra
import UInt64 exposing (UInt64)


part1 : String -> String
part1 string =
    string
        |> parseTiles
        |> Maybe.andThen assembleImage
        |> Maybe.andThen multiplyCorners
        |> Maybe.map UInt64.toString
        |> Maybe.withDefault ":("


part2 : String -> String
part2 _ =
    "TODO day 20 part 2"


type alias Tile =
    { number : Int
    , image : Square Bool
    }


type alias Square a =
    { size : Int
    , value : Array a -- Entries stored in row major order.
    }


type alias Point =
    ( Int, Int )


multiplyCorners : Dict Point Tile -> Maybe UInt64
multiplyCorners tiles =
    tiles
        |> Dict.keys
        |> getBounds
        |> Maybe.andThen
            (\( ( x0, y0 ), ( x1, y1 ) ) ->
                [ ( x0, y0 ), ( x0, y1 ), ( x1, y0 ), ( x1, y1 ) ]
                    |> Maybe.Extra.traverse (\point -> Dict.get point tiles)
                    |> Maybe.map
                        (\ts ->
                            ts
                                |> List.map (\t -> UInt64.fromInt t.number)
                                |> List.foldl UInt64.mul UInt64.one
                        )
            )


getBounds : List Point -> Maybe ( Point, Point )
getBounds points =
    let
        ( xs, ys ) =
            List.unzip points
    in
    Maybe.map2 Tuple.pair
        (Maybe.map2 Tuple.pair
            (List.minimum xs)
            (List.minimum ys)
        )
        (Maybe.map2 Tuple.pair
            (List.maximum xs)
            (List.maximum ys)
        )


assembleImage : List Tile -> Maybe (Dict Point Tile)
assembleImage =
    assembleImageWith Dict.empty []


assembleImageWith : Dict Point Tile -> List Tile -> List Tile -> Maybe (Dict Point Tile)
assembleImageWith assembled retry tiles =
    case tiles of
        [] ->
            case retry of
                [] ->
                    -- We have successfully assembled the image!
                    Just assembled

                _ ->
                    -- We're out of tiles, so let's retry some of them. I can't
                    -- prove that this will terminate, but it probably will.
                    assembleImageWith assembled [] (List.reverse retry)

        tile :: rest ->
            if Dict.isEmpty assembled then
                -- Arbitrarily put an image at the origin.
                assembleImageWith (Dict.insert ( 0, 0 ) tile assembled) retry rest

            else
                case findCandidates tile assembled of
                    [] ->
                        -- This one didn't fit, so try it again later.
                        assembleImageWith assembled (tile :: retry) rest

                    candidates ->
                        case insertOneCandidate assembled candidates of
                            Nothing ->
                                -- None of the candidates fit.
                                Nothing

                            Just newAssembled ->
                                -- Keep assembling the rest of the tiles.
                                assembleImageWith newAssembled retry rest


findCandidates : Tile -> Dict Point Tile -> List ( Point, Tile )
findCandidates tile tiles =
    tile.image
        |> getOrientations
        |> List.filterMap
            (\image ->
                findPoint image tiles
                    |> Maybe.map (\point -> ( point, { tile | image = image } ))
            )


findPoint : Square Bool -> Dict Point Tile -> Maybe Point
findPoint image tiles =
    tiles
        |> Dict.toList
        |> Elf.findMap
            (\( ( x, y ), tile ) ->
                if getTop tile.image == getBottom image then
                    Just ( x, y + 1 )

                else if getRight tile.image == getLeft image then
                    Just ( x + 1, y )

                else if getBottom tile.image == getTop image then
                    Just ( x, y - 1 )

                else if getLeft tile.image == getRight image then
                    Just ( x - 1, y )

                else
                    Nothing
            )


insertOneCandidate : Dict Point Tile -> List ( Point, Tile ) -> Maybe (Dict Point Tile)
insertOneCandidate tiles candidates =
    case candidates of
        [] ->
            Nothing

        ( point, tile ) :: rest ->
            case insertCandidate tiles point tile of
                Nothing ->
                    insertOneCandidate tiles rest

                Just newTiles ->
                    Just newTiles


insertCandidate : Dict Point Tile -> Point -> Tile -> Maybe (Dict Point Tile)
insertCandidate tiles point tile =
    let
        edgesMatch adjust f g =
            case Dict.get (adjust point) tiles of
                Nothing ->
                    True

                Just t ->
                    f tile.image == g t.image
    in
    if
        edgesMatch (\( x, y ) -> ( x, y + 1 )) getTop getBottom
            && edgesMatch (\( x, y ) -> ( x + 1, y )) getRight getLeft
            && edgesMatch (\( x, y ) -> ( x, y - 1 )) getBottom getTop
            && edgesMatch (\( x, y ) -> ( x - 1, y )) getLeft getRight
    then
        Just (Dict.insert point tile tiles)

    else
        Nothing


parseTiles : String -> Maybe (List Tile)
parseTiles string =
    string
        -- For some reason my input has an extra newline at the end.
        |> String.trimRight
        |> String.split "\n\n"
        |> Maybe.Extra.traverse parseTile


parseTile : String -> Maybe Tile
parseTile string =
    case String.words string of
        "Tile" :: number :: image ->
            Maybe.map2 Tile
                (parseNumber number)
                (parseImage image)

        _ ->
            Nothing


parseNumber : String -> Maybe Int
parseNumber string =
    string
        |> String.Extra.leftOf ":"
        |> String.toInt


parseImage : List String -> Maybe (Square Bool)
parseImage list =
    list
        |> List.map
            (\string ->
                string
                    |> String.toList
                    |> List.map (\char -> char == '#')
            )
        |> makeSquare


makeSquare : List (List a) -> Maybe (Square a)
makeSquare lists =
    let
        size =
            List.length lists
    in
    if List.all (\list -> List.length list == size) lists then
        Just { size = size, value = Array.fromList (List.concat lists) }

    else
        Nothing


getAt : Square a -> Int -> Int -> Maybe a
getAt square row column =
    Array.get ((row * square.size) + column) square.value


getRow : Square a -> Int -> Maybe (List a)
getRow square row =
    List.range 0 (square.size - 1)
        |> Maybe.Extra.traverse (\column -> getAt square row column)


getColumn : Square a -> Int -> Maybe (List a)
getColumn square column =
    List.range 0 (square.size - 1)
        |> Maybe.Extra.traverse (\row -> getAt square row column)


getTop : Square a -> Maybe (List a)
getTop square =
    getRow square 0


getRight : Square a -> Maybe (List a)
getRight square =
    getColumn square (square.size - 1)


getBottom : Square a -> Maybe (List a)
getBottom square =
    getRow square (square.size - 1)


getLeft : Square a -> Maybe (List a)
getLeft square =
    getColumn square 0


flipHorizontal : Square a -> Maybe (Square a)
flipHorizontal square =
    List.range 0 (square.size - 1)
        |> Maybe.Extra.traverse (\row -> getRow square row)
        |> Maybe.map (List.map List.reverse)
        |> Maybe.andThen makeSquare


flipVertical : Square a -> Maybe (Square a)
flipVertical square =
    List.range 0 (square.size - 1)
        |> Maybe.Extra.traverse (\row -> getRow square row)
        |> Maybe.map List.reverse
        |> Maybe.andThen makeSquare


rotateCounterclockwise : Square a -> Maybe (Square a)
rotateCounterclockwise square =
    List.range 0 (square.size - 1)
        |> Maybe.Extra.traverse (\column -> getColumn square column)
        |> Maybe.map List.reverse
        |> Maybe.andThen makeSquare


getOrientations : Square a -> List (Square a)
getOrientations square =
    let
        h =
            flipHorizontal

        v =
            flipVertical

        r =
            rotateCounterclockwise

        m =
            Maybe.andThen
    in
    Maybe.Extra.values
        [ square |> Just
        , square |> h
        , square |> v
        , square |> r
        , square |> r |> m h
        , square |> r |> m v
        , square |> r |> m r
        , square |> r |> m r |> m r
        ]
