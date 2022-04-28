module OptionalResult exposing
    ( OptionalResult(..), fromResult
    , map, map2, map3, mapError
    )

{-| Combine Result and Maybe into one! You can have an error, a valid (Ok) value,
or Nothing. This module provides the type to do that, along with some helper
functions.

@docs OptionalResult, fromResult

@docs map, map2, map3, mapError

-}


type OptionalResult error success
    = OptErr error
    | OptOk success
    | OptNothing


{-| Get an `OptionalResult` from a regular `Result`
-}
fromResult : Result error success -> OptionalResult error success
fromResult result =
    case result of
        Ok success ->
            OptOk success

        Err error ->
            OptErr error


{-| Apply a function to the `OptOk` value
-}
map : (a -> b) -> OptionalResult error a -> OptionalResult error b
map fn optionalResult =
    case optionalResult of
        OptOk a ->
            OptOk (fn a)

        OptErr error ->
            OptErr error

        OptNothing ->
            OptNothing


{-| Apply a function to two `OptOk` values
-}
map2 : (a -> b -> c) -> OptionalResult error a -> OptionalResult error b -> OptionalResult error c
map2 fn first second =
    case ( first, second ) of
        ( OptOk firstOk, OptOk secondOk ) ->
            OptOk (fn firstOk secondOk)

        ( OptErr error, _ ) ->
            OptErr error

        ( _, OptErr error ) ->
            OptErr error

        _ ->
            OptNothing


{-| Apply a function to three `OptOk` values
-}
map3 :
    (a -> b -> c -> d)
    -> OptionalResult error a
    -> OptionalResult error b
    -> OptionalResult error c
    -> OptionalResult error d
map3 fn first second third =
    case ( first, second, third ) of
        ( OptOk firstOk, OptOk secondOk, OptOk thirdOk ) ->
            OptOk (fn firstOk secondOk thirdOk)

        ( OptErr error, _, _ ) ->
            OptErr error

        ( _, OptErr error, _ ) ->
            OptErr error

        ( _, _, OptErr error ) ->
            OptErr error

        _ ->
            OptNothing


{-| Apply a function to the `OptErr` value
-}
mapError : (error -> mappedError) -> OptionalResult error success -> OptionalResult mappedError success
mapError fn optionalResult =
    case optionalResult of
        OptErr error ->
            OptErr (fn error)

        OptOk success ->
            OptOk success

        OptNothing ->
            OptNothing
