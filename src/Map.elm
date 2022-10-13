module Map exposing (..)

import Array exposing (Array)


type alias Pnt =
    { column : Int
    , row : Int
    }


type Map a
    = Map (Array (Array a))


init : { width : Int, height : Int } -> (Pnt -> a) -> Map a
init size initFn =
    Array.initialize size.width
        (\column ->
            Array.initialize size.height
                (\row ->
                    initFn { column = column, row = row }
                )
        )
        |> Map


get : Pnt -> Map a -> Maybe a
get pnt (Map m) =
    case Array.get pnt.column m of
        Nothing ->
            Nothing

        Just column ->
            Array.get pnt.row column


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
                                    { column = column, row = row }
                                    tile
                        )
                        ""
                        (Array.toIndexedList columnData)
            )
            ""
