module Auxiliary exposing (dropLast, floatsToSpacedString, getAt, getLast)


dropLast : List a -> List a
dropLast list =
    list
        |> List.reverse
        |> List.drop 1
        |> List.reverse


getLast : List a -> Maybe a
getLast list =
    list
        |> List.reverse
        |> List.head


floatsToSpacedString : List Float -> String
floatsToSpacedString list =
    String.join " " <| List.map String.fromFloat list


getAt : Int -> List a -> Maybe a
getAt index list =
    list
        |> List.drop index
        |> List.head
