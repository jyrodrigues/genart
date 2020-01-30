module ListExtra exposing
    ( appendIf
    , dropIndex
    , dropLast
    , floatsToSpacedString
    , getAt
    , getLast
    , pushLast
    )


dropLast : List a -> List a
dropLast list =
    dropIndex (List.length list - 1) list


dropIndex : Int -> List a -> List a
dropIndex index list =
    if List.length list <= index then
        list

    else
        List.take index list ++ List.drop (index + 1) list


getLast : List a -> Maybe a
getLast list =
    list
        |> List.reverse
        |> List.head


pushLast : a -> List a -> List a
pushLast elem list =
    List.reverse (elem :: List.reverse list)


floatsToSpacedString : List Float -> String
floatsToSpacedString list =
    String.join " " <| List.map String.fromFloat list


getAt : Int -> List a -> Maybe a
getAt index list =
    list
        |> List.drop index
        |> List.head


appendIf : Bool -> List a -> List a -> List a
appendIf shouldAppend listToBeAppended listBase =
    if shouldAppend then
        listBase ++ listToBeAppended

    else
        listBase
