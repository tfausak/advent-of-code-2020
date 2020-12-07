module Day07 exposing (part1, part2)

import Dict exposing (Dict)
import Graph exposing (Graph)
import IntDict
import Set


rulesToGraph : List ( String, List ( Int, String ) ) -> ( Dict String Int, Graph String Int )
rulesToGraph rules =
    let
        bagIds =
            rules
                |> List.indexedMap (\index ( bag, _ ) -> ( bag, index ))
                |> Dict.fromList

        nodes =
            bagIds
                |> Dict.toList
                |> List.map (\( bag, id ) -> { id = id, label = bag })

        toEdge outerBagId count innerBagId =
            { from = outerBagId
            , to = innerBagId
            , label = count
            }

        toMaybeEdge outerBagId ( count, innerBag ) =
            Maybe.map (toEdge outerBagId count) (Dict.get innerBag bagIds)

        toEdges ( outerBag, contents ) =
            case Dict.get outerBag bagIds of
                Nothing ->
                    []

                Just outerBagId ->
                    List.filterMap (toMaybeEdge outerBagId) contents

        edges =
            List.concatMap toEdges rules
    in
    ( bagIds, Graph.fromNodesAndEdges nodes edges )


parseRules : String -> List ( String, List ( Int, String ) )
parseRules string =
    string
        |> String.lines
        |> List.filterMap parseRule


parseRule : String -> Maybe ( String, List ( Int, String ) )
parseRule string =
    case String.words string of
        adjective :: color :: "bags" :: "contain" :: rest ->
            Maybe.map (Tuple.pair (toBag adjective color)) (parseContents rest)

        _ ->
            Nothing


toBag : String -> String -> String
toBag adjective color =
    String.join " " [ adjective, color ]


parseContents : List String -> Maybe (List ( Int, String ))
parseContents =
    parseContentsWith []


parseContentsWith : List ( Int, String ) -> List String -> Maybe (List ( Int, String ))
parseContentsWith contents words =
    case words of
        [ "no", "other", "bags." ] ->
            Just contents

        rawCount :: adjective :: color :: bags :: rest ->
            let
                next count =
                    let
                        newContents =
                            ( count, toBag adjective color ) :: contents
                    in
                    case ( count, bags ) of
                        ( 1, "bag," ) ->
                            parseContentsWith newContents rest

                        ( _, "bags," ) ->
                            parseContentsWith newContents rest

                        ( 1, "bag." ) ->
                            Just newContents

                        ( _, "bags." ) ->
                            Just newContents

                        _ ->
                            Nothing
            in
            Maybe.andThen next (String.toInt rawCount)

        _ ->
            Nothing


part1 : String -> String
part1 string =
    let
        ( ids, graph ) =
            rulesToGraph (parseRules string)
    in
    case Dict.get "shiny gold" ids of
        Nothing ->
            "no shiny gold bag ID"

        Just id ->
            case Graph.get id graph of
                Nothing ->
                    "no shiny gold bag in graph"

                Just ctx ->
                    ctx
                        |> allIncomingNodes graph
                        |> List.map .id
                        |> Set.fromList
                        |> Set.size
                        |> (\x -> x - 1)
                        |> String.fromInt


allIncomingNodes : Graph n e -> Graph.NodeContext n e -> List (Graph.Node n)
allIncomingNodes graph nodeContext =
    nodeContext.incoming
        |> IntDict.keys
        |> List.concatMap
            (\nodeId ->
                Graph.get nodeId graph
                    |> Maybe.map (allIncomingNodes graph)
                    |> Maybe.withDefault []
            )
        |> (\xs -> nodeContext.node :: xs)


part2 : String -> String
part2 _ =
    "TODO day 7 part 2"
