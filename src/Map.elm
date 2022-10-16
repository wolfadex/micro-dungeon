module Map exposing (..)

import Ansi.Color exposing (Color, Location(..))
import Array exposing (Array)
import Map.Pnt exposing (Pnt)
import Map.Rect exposing (Rect)
import Random exposing (Seed)
import Random.Extra


type Map a
    = Map (Array (Array a))


type alias Entity =
    { position : Pnt
    , symbol : String
    , color : Color
    }


type alias Tile =
    { walkable : Bool
    , transparent : Bool
    , symbol : String
    , color : Color
    }


floor : Tile
floor =
    { walkable = True
    , transparent = True
    , symbol = " "
    , color = gray
    }


wall : Tile
wall =
    { walkable = False
    , transparent = False
    , symbol = "█"
    , color = gray
    }


gray : Color
gray =
    Ansi.Color.rgb { red = 205, green = 205, blue = 205 }


init : { columns : Int, rows : Int } -> (Pnt -> a) -> Map a
init size initFn =
    Array.initialize size.columns
        (\column ->
            Array.initialize size.rows
                (\row ->
                    initFn { column = column + 1, row = row + 1 }
                )
        )
        |> Map


get : Pnt -> Map a -> Maybe a
get pnt (Map m) =
    case Array.get (pnt.column - 1) m of
        Nothing ->
            Nothing

        Just column ->
            Array.get (pnt.row - 1) column


set : Pnt -> a -> Map a -> Map a
set pnt a (Map m) =
    case Array.get (pnt.column - 1) m of
        Nothing ->
            Map m

        Just column ->
            Map (Array.set (pnt.column - 1) (Array.set (pnt.row - 1) a column) m)


draw : (Pnt -> a -> String) -> Map a -> String
draw drawTile (Map m) =
    Array.toIndexedList m
        |> List.foldl
            (\( column, columnData ) columnResult ->
                columnResult
                    ++ List.foldl
                        (\( row, tile ) rowResult ->
                            rowResult
                                ++ drawTile
                                    { column = column + 1, row = row + 1 }
                                    tile
                        )
                        ""
                        (Array.toIndexedList columnData)
            )
            ""


addRoom : Rect -> Map Tile -> Map Tile
addRoom r m =
    Map.Rect.foldl
        (\pnt -> set pnt floor)
        m
        (Map.Rect.inner r)


type alias Tunnel =
    { start : Pnt
    , end : Pnt
    }


addTunnel : Seed -> Tunnel -> Map Tile -> ( Map Tile, Seed )
addTunnel seed t m =
    Random.step
        (Random.Extra.bool
            |> Random.map
                (\rowFirst ->
                    if rowFirst then
                        let
                            across : Map Tile
                            across =
                                List.foldl
                                    (\column -> set { column = column, row = t.start.row } floor)
                                    m
                                    (List.range
                                        (min t.start.column t.end.column)
                                        (max t.start.column t.end.column)
                                    )
                        in
                        List.foldl
                            (\row -> set { column = t.end.column, row = row } floor)
                            across
                            (List.range
                                (min t.start.row t.end.row)
                                (max t.start.row t.end.row)
                            )

                    else
                        let
                            down : Map Tile
                            down =
                                List.foldl
                                    (\row -> set { column = t.start.column, row = row } floor)
                                    m
                                    (List.range
                                        (min t.start.row t.end.row)
                                        (max t.start.row t.end.row)
                                    )
                        in
                        List.foldl
                            (\column -> set { column = column, row = t.end.row } floor)
                            down
                            (List.range
                                (min t.start.column t.end.column)
                                (max t.start.column t.end.column)
                            )
                )
        )
        seed


generate :
    { columns : Int
    , rows : Int
    , roomExtents :
        { maxSize : Int
        , minSize : Int
        }
    , roomAttempts : Int
    }
    -> Seed
    -> ( Map Tile, Seed )
generate config seed =
    let
        baseMap : Map Tile
        baseMap =
            init
                { columns = config.columns
                , rows = config.rows
                }
                (\_ -> wall)

        ( rooms, nextSeed ) =
            generateRooms config seed []

        mapWithRooms =
            List.foldl addRoom baseMap rooms

        ( finalMap, finalSeed, _ ) =
            List.foldr
                (\room ( m, s, previousRoom ) ->
                    case previousRoom of
                        Nothing ->
                            ( m, s, Just room )

                        Just r ->
                            let
                                ( withTunnel, ts ) =
                                    addTunnel s
                                        { start = Map.Rect.center r
                                        , end = Map.Rect.center room
                                        }
                                        m
                            in
                            ( withTunnel, ts, Just room )
                )
                ( mapWithRooms, nextSeed, Nothing )
                rooms
    in
    ( finalMap, finalSeed )


generateRooms :
    { columns : Int
    , rows : Int
    , roomExtents :
        { maxSize : Int
        , minSize : Int
        }
    , roomAttempts : Int
    }
    -> Seed
    -> List Rect
    -> ( List Rect, Seed )
generateRooms config seed rooms =
    if config.roomAttempts > 0 then
        let
            ( room, nextSeed ) =
                Random.step
                    (Random.map2 Tuple.pair
                        (Random.int config.roomExtents.minSize config.roomExtents.maxSize)
                        (Random.int config.roomExtents.minSize config.roomExtents.maxSize)
                        |> Random.andThen
                            (\( width, height ) ->
                                Random.map2
                                    (\column row ->
                                        { p1 = { column = column, row = row }
                                        , p2 = { column = column + width, row = row + height }
                                        }
                                    )
                                    (Random.int 1 (config.columns - width))
                                    (Random.int 1 (config.rows - height))
                            )
                    )
                    seed
        in
        generateRooms { config | roomAttempts = config.roomAttempts - 1 }
            nextSeed
            (if List.any (Map.Rect.intersect room) rooms then
                rooms

             else
                room :: rooms
            )

    else
        ( rooms, seed )
