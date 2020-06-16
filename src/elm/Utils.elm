module Utils exposing (..)

import Process
import Task


delay : Float -> msg -> Cmd msg
delay milliseconds msg =
    Process.sleep milliseconds
        |> Task.perform (\_ -> msg)


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

