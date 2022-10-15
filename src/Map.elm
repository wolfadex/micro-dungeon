module Map exposing (..)

import Array exposing (Array)


type alias Pnt =
    { column : Int
    , row : Int
    }


type Map a
    = Map (Array (Array a))


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


type alias Rect =
    { p1 : Pnt
    , p2 : Pnt
    }


rectCenter : Rect -> Pnt
rectCenter r =
    { column = (r.p1.column + r.p2.column) // 2
    , row = (r.p1.row + r.p2.row) // 2
    }


rectInner : Rect -> Rect
rectInner r =
    { p1 =
        { column = r.p1.column + 1
        , row = r.p1.row + 1
        }
    , p2 =
        { column = r.p2.column - 1
        , row = r.p2.row - 1
        }
    }


{-| Iterate left to right, top to bottom across a Rect
-}
rectFoldl : (Pnt -> a -> a) -> a -> Rect -> a
rectFoldl fn a r =
    List.foldl
        (\row res ->
            List.foldl
                (\column result ->
                    fn { column = column, row = row } result
                )
                res
                (List.range r.p1.column r.p2.column)
        )
        a
        (List.range r.p1.row r.p2.row)
