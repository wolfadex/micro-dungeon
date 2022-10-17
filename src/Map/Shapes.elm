module Map.Shapes exposing
    ( circle
    , ellipse
    , line
    )

import Map.Pnt exposing (Pnt)


line : Pnt -> Pnt -> List Pnt
line p0 p1 =
    let
        ( statics, error ) =
            initLine p1 p0
    in
    lineLoop statics error p0 []


circle : Int -> Pnt -> List Pnt
circle diameter =
    ellipse { width = diameter, height = diameter }


ellipse : { width : Int, height : Int } -> Pnt -> List Pnt
ellipse ({ width, height } as size) center =
    let
        firstHalf =
            ellipseLoopFirst
                center
                (Pnt 0 height)
                (2 * (width ^ 2) + (height ^ 2) * (1 - 2 * height))
                size
                []

        secondHalf =
            ellipseLoopSecond
                center
                (Pnt width 0)
                (2 * (width ^ 2) + (height ^ 2) * (1 - 2 * width))
                size
                []
    in
    firstHalf ++ secondHalf


ellipseLoopFirst : Pnt -> Pnt -> Int -> { width : Int, height : Int } -> List Pnt -> List Pnt
ellipseLoopFirst c { column, row } sigma { width, height } positions =
    if column * (height ^ 2) <= row * (width ^ 2) then
        let
            ( dy, dSigma ) =
                if sigma >= 0 then
                    ( 1, (4 * (width ^ 2)) * (1 - row) )

                else
                    ( 0, 0 )
        in
        ellipseLoopFirst
            c
            (Pnt (column + 1) (row - dy))
            (sigma + dSigma + ((height ^ 2) * ((4 * column) + 6)))
            { width = width, height = height }
            (addPoints c.column c.row column row positions)

    else
        positions


ellipseLoopSecond : Pnt -> Pnt -> Int -> { width : Int, height : Int } -> List Pnt -> List Pnt
ellipseLoopSecond c { column, row } sigma { width, height } positions =
    if row * (width ^ 2) <= column * (height ^ 2) then
        let
            ( dx, dSigma ) =
                if sigma >= 0 then
                    ( 1, (4 * (height ^ 2)) * (1 - column) )

                else
                    ( 0, 0 )
        in
        ellipseLoopSecond
            c
            (Pnt (column - dx) (row + 1))
            (sigma + dSigma + ((width ^ 2) * ((4 * row) + 6)))
            { width = width, height = height }
            (addPoints c.column c.row column row positions)

    else
        positions


addPoints : Int -> Int -> Int -> Int -> List Pnt -> List Pnt
addPoints cx cy x y points =
    points
        ++ [ Pnt (cx + x) (cy + y)
           , Pnt (cx - x) (cy + y)
           , Pnt (cx + x) (cy - y)
           , Pnt (cx - x) (cy - y)
           ]


lineLoop : LineStatics -> Float -> Pnt -> List Pnt -> List Pnt
lineLoop statics error ({ column, row } as p) positions =
    if (column == statics.fx) && (row == statics.fy) then
        p :: positions

    else
        let
            ( error_, q ) =
                calcError statics error p
        in
        lineLoop statics error_ q (p :: positions)


type alias LineStatics =
    { fx : Int
    , fy : Int
    , sx : Int
    , sy : Int
    , dx : Float
    , dy : Float
    }


calcError : LineStatics -> Float -> Pnt -> ( Float, Pnt )
calcError { sx, sy, dx, dy } error p =
    let
        ( errX, x ) =
            if error > -dx then
                ( -dy, sx + p.column )

            else
                ( 0, p.column )

        ( errY, y ) =
            if error < dy then
                ( dx, sy + p.row )

            else
                ( 0, p.row )
    in
    ( error + errX + errY, Pnt x y )


initLine : Pnt -> Pnt -> ( LineStatics, Float )
initLine p q =
    let
        dx =
            (toFloat << abs) (p.column - q.column)

        dy =
            (toFloat << abs) (p.row - q.row)

        sx =
            if p.column > q.column then
                1

            else
                -1

        sy =
            if p.row > q.row then
                1

            else
                -1

        error =
            if dx > dy then
                dx / 2

            else
                -dy / 2
    in
    ( LineStatics p.column p.row sx sy dx dy, error )
