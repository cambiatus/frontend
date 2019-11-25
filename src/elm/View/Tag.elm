module View.Tag exposing (TagStatus(..), view)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import I18Next exposing (Translations)


type TagStatus
    = PENDING
    | DISAPPROVED
    | APPROVED


view : TagStatus -> Translations -> Html msg
view status translations =
    let
        t s =
            I18Next.t translations s

        ( msg, textColor ) =
            case status of
                PENDING ->
                    ( t "verify_claim.pending", "text-black" )

                DISAPPROVED ->
                    ( t "verify_claim.disapproved", "text-red" )

                APPROVED ->
                    ( t "verify_claim.approval", "text-green" )
    in
    div
        [ class "bg-gray-100 flex items-center justify-center h-6 w-32" ]
        [ p
            [ class ("font-sans text-caption uppercase " ++ textColor) ]
            [ text msg ]
        ]
