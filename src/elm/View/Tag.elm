module View.Tag exposing (TagStatus(..), view)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)


type TagStatus
    = PENDING
    | DISAPPROVED
    | APPROVED


view : TagStatus -> Html msg
view status =
    let
        ( msg, textColor ) =
            case status of
                PENDING ->
                    ( "pending", "text-black" )

                DISAPPROVED ->
                    ( "disapproval", "text-red" )

                APPROVED ->
                    ( "approval", "text-green" )
    in
    div
        [ class "bg-gray-100 flex items-center justify-center h-6 w-32" ]
        [ p
            [ class ("font-sans text-caption uppercase " ++ textColor) ]
            [ text msg ]
        ]
