module AssocSet exposing
    ( Set
    , empty
    , fromList
    , member
    , union
    )

import AssocList
import Html exposing (a)


type Set a
    = Set (AssocList.Dict a ())


empty : Set a
empty =
    Set AssocList.empty


member : a -> Set a -> Bool
member a (Set s) =
    AssocList.member a s


fromList : List a -> Set a
fromList list =
    Set (AssocList.fromList (List.map (\a -> ( a, () )) list))


union : Set a -> Set a -> Set a
union (Set left) (Set right) =
    Set (AssocList.union left right)
