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
        ( msg, color ) =
            case status of
                PENDING ->
                    ( "pending", "text-black" )

                DISAPPROVED ->
                    ( "disapproval", "text-de-york" )

                APPROVED ->
                    ( "approval", "text-fire-engine-red" )
    in
    div
        [ class "bg-white-smoke flex items-center justify-center h-6 w-32" ]
        [ p
            [ class ("font-sans text-caption uppercase " ++ color) ]
            [ text msg ]
        ]
