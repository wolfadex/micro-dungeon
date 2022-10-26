port module Main exposing (main)

import Ansi
import Ansi.Color exposing (Location(..))
import Ansi.Cursor
import Ansi.Font
import Ansi.Parse
import AssocSet
import Map exposing (..)
import Map.Pnt exposing (Pnt)
import Map.Rect exposing (Rect)
import Map.Shapes
import Random exposing (Seed)
import Random.Extra


possibleSymbols =
    """
֎
۞
๏
ᚙ
᳀
⁜
░
▒
▓
▩
◯
◦
◉
◢
◣
◤
◥
█
"""


main : Program Int Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { player : Entity
    , enemies : List Enemy
    , seed : Seed
    , gameMap : Map Tile
    , hasSeen : AssocSet.Set Pnt
    , canSee : AssocSet.Set Pnt
    }


type Enemy
    = Troll Entity
    | Orc Entity


boardMax : Pnt
boardMax =
    { column = 80
    , row = 45
    }


init : Int -> ( Model, Cmd Msg )
init randomSeedStarter =
    let
        ( gameMap, seed, rooms ) =
            Map.generate
                { columns = boardMax.column
                , rows = boardMax.row
                , roomAttempts = 30
                , roomExtents = { minSize = 6, maxSize = 10 }
                }
                (Random.initialSeed randomSeedStarter)

        maxMonstersPerRoom =
            2

        ( startPos, enemyRooms ) =
            case rooms of
                [] ->
                    ( { column = boardMax.column // 2
                      , row = boardMax.row // 2
                      }
                    , []
                    )

                firstRoom :: rest ->
                    ( Map.Rect.center firstRoom, rest )

        ( trolls, orcs, finalSeed ) =
            generateEnemies maxMonstersPerRoom seed enemyRooms

        player =
            { position = startPos
            , symbol = "☺"
            , color = Ansi.Color.green
            }

        baseModel =
            { player = player
            , enemies =
                List.concatMap identity
                    [ List.map (\p -> Troll { position = p, symbol = "@", color = Ansi.Color.red }) trolls
                    , List.map (\p -> Orc { position = p, symbol = "◉", color = Ansi.Color.red }) orcs
                    ]
            , seed = finalSeed
            , gameMap = gameMap
            , hasSeen = AssocSet.empty
            , canSee = AssocSet.empty
            }

        initialSeen =
            calculateVision baseModel
    in
    render
        { baseModel
            | canSee = initialSeen
            , hasSeen = initialSeen
        }


generateEnemies : Int -> Seed -> List Rect -> ( List Pnt, List Pnt, Seed )
generateEnemies maxMonstersPerRoom seed enemyRooms =
    List.foldl
        (\room ( trls, ocs, s ) ->
            let
                ( ens, s2 ) =
                    Random.step
                        (Random.int 0 maxMonstersPerRoom
                            |> Random.andThen
                                (\monsterCount ->
                                    Random.list monsterCount
                                        (Random.map3
                                            (\column row isTroll ->
                                                if isTroll then
                                                    Left { column = column, row = row }

                                                else
                                                    Right { column = column, row = row }
                                            )
                                            (Random.int (room.p1.column + 1) (room.p2.column - 1))
                                            (Random.int (room.p1.row + 1) (room.p2.row - 1))
                                            (Random.Extra.oneIn 8)
                                        )
                                )
                        )
                        s
            in
            ( List.foldr
                (\either res ->
                    case either of
                        Left t ->
                            if List.member t res then
                                res

                            else
                                t :: res

                        Right _ ->
                            res
                )
                trls
                ens
            , List.foldr
                (\either res ->
                    case either of
                        Left _ ->
                            ocs

                        Right o ->
                            if List.member o res then
                                res

                            else
                                o :: res
                )
                ocs
                ens
            , s2
            )
        )
        ( [], [], seed )
        enemyRooms


type Either a b
    = Left a
    | Right b


calculateVision : Model -> AssocSet.Set Pnt
calculateVision model =
    Map.Shapes.circle 8 model.player.position
        |> List.concatMap
            (\edgePnt ->
                Map.Shapes.line model.player.position edgePnt
                    |> keepUntil model.gameMap []
            )
        |> AssocSet.fromList


keepUntil : Map Tile -> List Pnt -> List Pnt -> List Pnt
keepUntil m result input =
    case input of
        [] ->
            List.reverse result

        next :: rest ->
            case Map.get next m of
                Nothing ->
                    keepUntil m (next :: result) []

                Just t ->
                    if t.transparent then
                        keepUntil m (next :: result) rest

                    else
                        keepUntil m (next :: result) []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ stdin Stdin
        ]


port stdin : (String -> msg) -> Sub msg


port stdout : String -> Cmd msg


port exit : Int -> Cmd msg


type Msg
    = Stdin String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Stdin input ->
            -- ESC or Ctrl+C
            if input == "\u{001B}" || input == "\u{0003}" then
                ( model, exit 0 )

            else
                { model
                    | player =
                        model.player
                            |> moveBy model.gameMap
                                (if Ansi.Parse.isUpArrow input then
                                    { column = 0, row = -1 }

                                 else if Ansi.Parse.isDownArrow input then
                                    { column = 0, row = 1 }

                                 else if Ansi.Parse.isLeftArrow input then
                                    { column = -1, row = 0 }

                                 else if Ansi.Parse.isRightArrow input then
                                    { column = 1, row = 0 }

                                 else
                                    { column = 0, row = 0 }
                                )
                }
                    |> setSeen
                    -- |> reRender model
                    |> render


setSeen : Model -> Model
setSeen model =
    let
        canSee =
            calculateVision model
    in
    { model
        | canSee = canSee
        , hasSeen = AssocSet.union model.hasSeen canSee
    }


moveBy : Map Tile -> Pnt -> Entity -> Entity
moveBy m amount ({ position } as ent) =
    let
        nextPnt =
            { column =
                (position.column + amount.column)
                    |> max 1
                    |> min boardMax.column
            , row =
                (position.row + amount.row)
                    |> max 1
                    |> min boardMax.row
            }
    in
    case Map.get nextPnt m of
        Nothing ->
            ent

        Just tile ->
            if tile.walkable then
                { ent | position = nextPnt }

            else
                ent


render : Model -> ( Model, Cmd Msg )
render model =
    ( model
    , [ Ansi.Font.resetAll
      , Ansi.clearScreen
      , Ansi.Cursor.hide
      , Ansi.setTitle "Micro Dungeon"
      , Map.draw
            { hasSeen = model.hasSeen
            , canSee = model.canSee
            }
            model.gameMap
      , model.enemies
            |> List.filterMap
                (\enemy ->
                    case enemy of
                        Troll ent ->
                            if AssocSet.member ent.position model.canSee then
                                Just (drawEntity ent ent)

                            else
                                Nothing

                        Orc ent ->
                            if AssocSet.member ent.position model.canSee then
                                Just (drawEntity ent ent)

                            else
                                Nothing
                )
            |> String.concat
      , model.player
            |> drawEntity model.player
      ]
        |> String.concat
        |> stdout
    )

    )


drawAt : { row : Int, column : Int } -> String -> String
drawAt loc str =
    Ansi.Cursor.moveTo loc ++ str


drawEntity : Entity -> Entity -> String
drawEntity oldEnt newEnt =
    drawAt oldEnt.position " "
        ++ (newEnt.symbol
                |> Ansi.Color.fontColor newEnt.color
                |> drawAt newEnt.position
           )
