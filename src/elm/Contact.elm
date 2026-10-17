module Contact exposing (Valid, circularIconWithGrayBg, selectionSet, toHref, toLabel)

import Cambiatus.Enum.ContactType as ContactType exposing (ContactType)
import Cambiatus.Object
import Cambiatus.Object.Contact
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Attributes.Aria exposing (ariaLabel)
import Icons
import Translation



-- VIEWS


circularIconWithGrayBg : List (Html.Attribute Never) -> Translation.Translators -> Valid -> Html msg
circularIconWithGrayBg attrs translators (Valid valid) =
    circularIconWithGrayBgInternal attrs translators valid.type_


circularIconWithGrayBgInternal : List (Html.Attribute Never) -> Translation.Translators -> ContactType -> Html msg
circularIconWithGrayBgInternal attrs translators type_ =
    let
        defaultClass =
            case type_ of
                ContactType.Whatsapp ->
                    "fill-current text-green "

                ContactType.Phone ->
                    "fill-current text-orange-500"

                _ ->
                    ""
    in
    div
        ((class "p-2 flex-shrink-0 bg-gray-100 rounded-full flex items-center justify-center"
            :: ariaLabel (typeToAriaLabel translators type_)
            :: attrs
         )
            |> List.map (Html.Attributes.map never)
        )
        [ typeToIconInverted defaultClass type_ ]


typeToIconInverted : String -> ContactType -> Html msg
typeToIconInverted class_ type_ =
    case type_ of
        ContactType.Phone ->
            Icons.phoneInverted class_

        ContactType.Instagram ->
            Icons.instagram class_

        ContactType.Telegram ->
            Icons.telegramInverted class_

        ContactType.Whatsapp ->
            Icons.whatsappInverted class_

        ContactType.Email ->
            Icons.mail class_

        ContactType.Link ->
            Icons.link class_



-- HELPERS


{-| Transform a `Valid` contact into a string that can be used as an `href` on
an anchor tag.

Usually, you will want to pair this with `Html.Attributes.target "blank"`

-}
toHref : Valid -> String
toHref (Valid valid) =
    case valid.type_ of
        ContactType.Email ->
            "mailto:" ++ valid.value

        ContactType.Instagram ->
            valid.value

        ContactType.Link ->
            valid.value

        ContactType.Phone ->
            "tel:" ++ valid.value

        ContactType.Telegram ->
            valid.value

        ContactType.Whatsapp ->
            "https://api.whatsapp.com/send?phone="
                ++ String.filter Char.isAlphaNum valid.value


toLabel : Translation.Translators -> Valid -> String
toLabel translators (Valid valid) =
    valid.label
        |> Maybe.withDefault (typeToString translators valid.type_)


typeToString : Translation.Translators -> ContactType -> String
typeToString translators type_ =
    ContactType.toString type_
        |> String.toLower
        |> (\asString -> "contact_form.types." ++ asString)
        |> translators.t


typeToAriaLabel : Translation.Translators -> ContactType -> String
typeToAriaLabel { t } type_ =
    case type_ of
        ContactType.Phone ->
            t "contact_form.reach_out.phone"

        ContactType.Instagram ->
            t "contact_form.reach_out.instagram"

        ContactType.Telegram ->
            t "contact_form.reach_out.telegram"

        ContactType.Whatsapp ->
            t "contact_form.reach_out.whatsapp"

        ContactType.Email ->
            t "contact_form.reach_out.email"

        ContactType.Link ->
            t "contact_form.reach_out.link"



-- FORM
-- TYPES


type Valid
    = Valid
        { type_ : ContactType
        , label : Maybe String
        , value : String
        }



-- FORM EVENT HANDLERS
-- FORM VALIDATION
-- GRAPHQL


selectionSet : SelectionSet (Maybe Valid) Cambiatus.Object.Contact
selectionSet =
    SelectionSet.succeed
        (\maybeType_ maybeExternalId label ->
            Maybe.map2
                (\type_ externalId ->
                    Valid
                        { type_ = type_
                        , value = externalId
                        , label = label
                        }
                )
                maybeType_
                maybeExternalId
        )
        |> SelectionSet.with Cambiatus.Object.Contact.type_
        |> SelectionSet.with Cambiatus.Object.Contact.externalId
        |> SelectionSet.with Cambiatus.Object.Contact.label
