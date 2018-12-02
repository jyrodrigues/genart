module Auxiliary exposing (dropLast, floatsToSpacedString)


dropLast : List a -> List a
dropLast list =
    list
        |> List.reverse
        |> List.drop 1
        |> List.reverse


floatsToSpacedString : List Float -> String
floatsToSpacedString list =
    String.join " " <| List.map String.fromFloat list
