module Utils exposing
    ( decodeEnterKeyDown
    , decodeTimestamp
    , errorToString
    , formatFloat
    , onClickNoBubble
    , onClickPreventAll
    , posixDateTime
    )

import Cambiatus.Scalar exposing (DateTime(..))
import Graphql.Http
import Graphql.Http.GraphqlError
import Html
import Html.Events
import Iso8601
import Json.Decode as Decode
import Time exposing (Posix)


posixDateTime : Maybe DateTime -> Posix
posixDateTime maybedt =
    case maybedt of
        Nothing ->
            Time.millisToPosix 0

        Just (DateTime s) ->
            case Iso8601.toTime s of
                Ok posix ->
                    posix

                Err _ ->
                    Time.millisToPosix 0


{-| Format a float to separate thousands, and use `,` as a separator for
decimals
-}
formatFloat : Float -> Int -> Bool -> String
formatFloat number decimalCases useSeparator =
    let
        addThousandsSeparator : String -> String
        addThousandsSeparator floatWithoutSeparator =
            if not useSeparator then
                floatWithoutSeparator

            else
                let
                    sign =
                        String.filter (not << Char.isDigit) floatWithoutSeparator
                in
                floatWithoutSeparator
                    |> String.filter Char.isDigit
                    |> String.foldr
                        (\currChar ( currCount, currString ) ->
                            if currCount == 3 then
                                ( 1, currChar :: '.' :: currString )

                            else
                                ( currCount + 1, currChar :: currString )
                        )
                        ( 0, [] )
                    |> Tuple.second
                    |> String.fromList
                    |> (\withThousands -> sign ++ withThousands)

        newSeparator =
            if useSeparator then
                ","

            else
                "."
    in
    case String.fromFloat number |> String.split "." of
        [] ->
            String.fromFloat number

        [ withoutSeparator ] ->
            if decimalCases <= 0 then
                addThousandsSeparator withoutSeparator

            else
                addThousandsSeparator withoutSeparator
                    ++ newSeparator
                    ++ String.repeat decimalCases "0"

        beforeSeparator :: afterSeparator :: _ ->
            if decimalCases <= 0 then
                addThousandsSeparator beforeSeparator

            else
                let
                    paddedSeparator =
                        String.left decimalCases afterSeparator
                            ++ String.repeat
                                (max 0 (decimalCases - String.length afterSeparator))
                                "0"
                in
                String.join newSeparator [ addThousandsSeparator beforeSeparator, paddedSeparator ]


decodeTimestamp : Decode.Decoder Posix
decodeTimestamp =
    Decode.int
        |> Decode.map Time.millisToPosix


decodeEnterKeyDown : Decode.Decoder Bool
decodeEnterKeyDown =
    let
        isEnter code =
            code == "Enter"
    in
    Decode.field "key" Decode.string
        |> Decode.map isEnter


{-| Click event listener that stops propagation, but doesn't prevent default
-}
onClickNoBubble : msg -> Html.Attribute msg
onClickNoBubble message =
    Html.Events.custom "click"
        (Decode.succeed
            { message = message
            , stopPropagation = True
            , preventDefault = False
            }
        )


{-| Click event listener that stops propagation and prevents default
-}
onClickPreventAll : msg -> Html.Attribute msg
onClickPreventAll message =
    Html.Events.custom "click"
        (Decode.succeed
            { message = message
            , stopPropagation = True
            , preventDefault = True
            }
        )


errorToString : Graphql.Http.Error parsedData -> String
errorToString errorData =
    case errorData of
        Graphql.Http.GraphqlError _ graphqlErrors ->
            graphqlErrors
                |> List.map graphqlErrorToString
                |> String.join "\n"

        Graphql.Http.HttpError _ ->
            "Http Error"


graphqlErrorToString : Graphql.Http.GraphqlError.GraphqlError -> String
graphqlErrorToString error =
    error.message
