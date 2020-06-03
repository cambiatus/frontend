module DateDistance exposing (viewDateDistance)

{-|


# Time.Distance

@docs inWords

-}

import Time exposing (Posix)


second : Int
second =
    1000


minute : Int
minute =
    second * 60


hour : Int
hour =
    minute * 60


day : Int
day =
    hour * 24


month : Int
month =
    day * 30


year : Int
year =
    day * 365


toS : Int -> String
toS =
    String.fromInt


{-| Returns the distance between two times in words.
time1 = Posix.fromMillis 1530403200000
time2 = Posix.fromMillis 1530403205000
-}
viewDateDistance : Posix -> Posix -> String
viewDateDistance posix1 posix2 =
    let
        time1 =
            Time.posixToMillis posix1

        time2 =
            Time.posixToMillis posix2

        diff =
            time1 - time2

        absDiff =
            abs diff

        diffInSeconds =
            absDiff // second

        diffInMinutes =
            absDiff // minute

        diffInHours =
            absDiff // hour

        diffInDays =
            absDiff // day

        diffInMonths =
            absDiff // month

        diffInYear =
            absDiff // year
    in
    if diffInSeconds < 60 then
        toS diffInSeconds ++ "s"

    else if diffInMinutes < 60 then
        toS diffInMinutes ++ "min"

    else if diffInHours < 24 then
        toS diffInHours ++ "h"

    else if diffInDays < 30 then
        toS diffInDays ++ "d"

    else if diffInMonths < 12 then
        toS diffInMonths ++ "m"

    else
        toS diffInYear ++ "y"
