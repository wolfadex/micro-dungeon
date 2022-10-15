module Map.Rect exposing (..)

import Map.Pnt exposing (Pnt)


type alias Rect =
    { p1 : Pnt
    , p2 : Pnt
    }


center : Rect -> Pnt
center r =
    { column = (r.p1.column + r.p2.column) // 2
    , row = (r.p1.row + r.p2.row) // 2
    }


inner : Rect -> Rect
inner r =
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
foldl : (Pnt -> a -> a) -> a -> Rect -> a
foldl fn a r =
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


intersect : Rect -> Rect -> Bool
intersect left right =
    ((left.p2.column < right.p1.column)
        || (left.p1.column > right.p2.column)
        || (left.p1.row > right.p2.row)
        || (left.p2.row < right.p1.row)
    )
        |> not
