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
