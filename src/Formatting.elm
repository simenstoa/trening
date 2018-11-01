module Formatting exposing (formatDistance, formatHours, formatTime)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (frenchLocale)


formatDistance : Float -> String
formatDistance distance =
    format { frenchLocale | decimals = 1 } <| distance / 1000.0


formatHours : Int -> String
formatHours seconds =
    (toFloat seconds / (60 * 60)) |> round |> String.fromInt


formatTime : Int -> String
formatTime seconds =
    let
        seconds_float =
            toFloat seconds

        hours =
            (seconds_float / (60 * 60)) |> floor |> modBy 60

        min =
            (seconds_float / 60.0) |> floor |> modBy 60

        sec =
            modBy 60 seconds
    in
    (if hours > 0 then
        String.fromInt hours ++ ":"

     else
        ""
    )
        ++ (if hours > 0 then
                String.padLeft 2 '0' <|
                    String.fromInt min

            else
                String.fromInt min
           )
        ++ ":"
        ++ (String.padLeft 2 '0' <|
                String.fromInt sec
           )
