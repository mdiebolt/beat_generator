module Utilities exposing (..)


updateIf : (a -> Bool) -> (a -> a) -> List a -> List a
updateIf predicate update list =
    List.map
        (\item ->
            if predicate item then
                update item
            else
                item
        )
        list


matches : (a -> b) -> a -> a -> Bool
matches prop r toCompare =
    (prop r) == (prop toCompare)


reposition : List { a | position : Int } -> List { a | position : Int }
reposition list =
    let
        updatePosition i a =
            { a | position = i + 1 }
    in
        List.indexedMap updatePosition list


fillWith :
    Int
    -> { a | position : Int }
    -> List { a | position : Int }
    -> List { a | position : Int }
fillWith length whenEmpty list =
    let
        newItems =
            List.repeat length whenEmpty
    in
        (list ++ newItems)
            |> reposition
            |> List.take length
