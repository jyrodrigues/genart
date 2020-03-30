module Midi exposing (..)


inputRange : { min : Float, max : Float, step : Float, baseValue : Float }
inputRange =
    { min = 0
    , max = 127
    , step = 1
    , baseValue = 127
    }


adjustExponential : ( Float, Float ) -> Float -> Float
adjustExponential ( base, multiplier ) value =
    {--Exponential function in range 0-127:

        Choose
            f(1) = startValue
            f(finalStep) = finalValue

            f(x) = a . (b^x)

            b = (finalValue . startValue^-1) ^ (1 / (finalStep - 1));
            a = startValue / b;

        Here we chose
            startValue = 0.00001; (1e-5)
            finalStep = 127;
            finalValue = 2;
    --}
    base ^ value * multiplier


adjustIntoRange : ( Float, Float ) -> Float -> Float
adjustIntoRange ( base, multiplier ) value =
    logBase base (value / multiplier)


intoRangeForStrokeWidth : Float -> Float
intoRangeForStrokeWidth =
    adjustIntoRange magicNumberFor.strokeWidth


adjustInputForStrokeWidth : Float -> Float
adjustInputForStrokeWidth =
    adjustExponential magicNumberFor.strokeWidth


adjustInputForChangeAngleVelocity : Float -> Float
adjustInputForChangeAngleVelocity =
    adjustExponential magicNumberFor.changeAngleVelocity


magicNumberFor =
    { strokeWidth = ( 1.10172109893, 0.0000090767 )

    -- TODO
    , changeAngleVelocity = ( 1.10172109893, 0.0000090767 )
    }
