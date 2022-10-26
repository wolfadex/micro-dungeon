module AssocSet exposing
    ( Set
    , diff
    , empty
    , foldl
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


diff : Set a -> Set a -> Set a
diff (Set left) (Set right) =
    Set (AssocList.diff left right)


foldl : (a -> b -> b) -> b -> Set a -> b
foldl fn initB (Set set) =
    AssocList.foldl
        (\a () b -> fn a b)
        initB
        set
