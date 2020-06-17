module Utils exposing (..)

import Process
import Task



-- DELAY


delay : Float -> msg -> Cmd msg
delay milliseconds msg =
    Process.sleep milliseconds
        |> Task.perform (\_ -> msg)



-- MATH


floatModBy : Float -> Float -> Float
floatModBy modulus x =
    x - modulus * toFloat (floor (x / modulus))


type alias MinMaxCenterAt =
    ( ( Float, Float ), ( Float, Float ) )


linearExponentialTransform : MinMaxCenterAt -> ( Float -> Float, Float -> Float )
linearExponentialTransform ( ( minValue, maxValue ), ( centerValue, centerAt ) ) =
    let
        {--
            Convert from linear (0 ~ 1) to exponential (0.000001 ~ 4)

            Take f(x) = a*b^x;
            Where f(centerAt) = centerValue;
            And f(1) = maxValue;
        --}
        exponent =
            if centerAt == 1 then
                1 / 1.0e-8

            else
                1 / (1 - centerAt)

        b =
            (maxValue / centerValue) ^ exponent

        a =
            maxValue ^ (1 - exponent) * centerValue ^ exponent

        toExponential zeroToOne =
            a * b ^ zeroToOne

        fromExponential value =
            logBase b (value / a)

        {--
            Linearly adjusting from e.g. (0.000001 ~ 4) into (0.01 ~ 4)

            N.B. `fromExponential` isn't defined for values below or equal to 0.
            *But* we restrict it even further with `toExponential 0`.

            TODO More explanation needed.
        --}
        magicN =
            fromExponential minValue

        magicAdjust value =
            (value * (1 - magicN)) + magicN

        magicReverse value =
            (value - magicN) / (1 - magicN)
    in
    ( magicAdjust >> toExponential, fromExponential >> magicReverse )



-- LIST EXTRAS


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



-- PAIR / TUPLE EXTRA


pairExec : (a -> b -> c) -> ( b, b ) -> ( a, a ) -> ( c, c )
pairExec op secondArg firstArg =
    ( op (Tuple.first firstArg) (Tuple.first secondArg)
    , op (Tuple.second firstArg) (Tuple.second secondArg)
    )


pairMap : (a -> b) -> ( a, a ) -> ( b, b )
pairMap op pair =
    ( op (Tuple.first pair)
    , op (Tuple.second pair)
    )



-- MAYBE EXTRA


maybeMap6 : (a -> b -> c -> d -> e -> f -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe f -> Maybe value
maybeMap6 func ma mb mc md me mf =
    case ma of
        Nothing ->
            Nothing

        Just a ->
            case mb of
                Nothing ->
                    Nothing

                Just b ->
                    case mc of
                        Nothing ->
                            Nothing

                        Just c ->
                            case md of
                                Nothing ->
                                    Nothing

                                Just d ->
                                    case me of
                                        Nothing ->
                                            Nothing

                                        Just e ->
                                            case mf of
                                                Nothing ->
                                                    Nothing

                                                Just f ->
                                                    Just (func a b c d e f)
