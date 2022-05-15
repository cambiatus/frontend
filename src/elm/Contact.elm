module Contact exposing (FormInput, Valid, circularIconWithGrayBg, form, initFormInput, selectionSet, toGraphqlInput, toHref, toLabel)

import Cambiatus.Enum.ContactType as ContactType exposing (ContactType)
import Cambiatus.InputObject
import Cambiatus.Object
import Cambiatus.Object.Contact
import Dict exposing (Dict)
import Form
import Form.Text
import Form.Validate
import Graphql.OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, button, div, li, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Attributes.Aria exposing (ariaHidden, ariaLabel)
import Html.Events exposing (on, onClick)
import Icons
import Json.Decode as Decode
import PhoneNumber
import PhoneNumber.Countries
import Regex exposing (Regex)
import Translation
import Url
import View.Components
import View.Modal as Modal



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


toGraphqlInput : Valid -> Cambiatus.InputObject.ContactInput
toGraphqlInput (Valid valid) =
    { externalId = Graphql.OptionalArgument.Present valid.value
    , label = Graphql.OptionalArgument.fromMaybe valid.label
    , type_ = Graphql.OptionalArgument.Present valid.type_
    }


typeToString : Translation.Translators -> ContactType -> String
typeToString translators type_ =
    ContactType.toString type_
        |> String.toLower
        |> (\asString -> "contact_form.types." ++ asString)
        |> translators.t


typeToPlaceholder : ContactType -> String
typeToPlaceholder type_ =
    case type_ of
        ContactType.Email ->
            "hola@cambiatus.com"

        ContactType.Instagram ->
            "cambiatus"

        ContactType.Link ->
            "cambiatus.com"

        ContactType.Phone ->
            "+55 11 91234 5678"

        ContactType.Telegram ->
            "cambiatus"

        ContactType.Whatsapp ->
            "+55 11 91234 5678"


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


type DeletionStatus
    = NotDeleted
    | Deleting
    | Deleted


type alias ContactFormInput =
    { type_ : ContactType
    , label : String
    , value : String
    , deletionStatus : DeletionStatus
    }


type FormInput
    = FormInput
        { inputs : Dict Int ContactFormInput
        , lastId : Int
        , isTypeModalOpen : Bool
        }


type Valid
    = Valid
        { type_ : ContactType
        , label : Maybe String
        , value : String
        }


initFormInput : List Valid -> FormInput
initFormInput valids =
    let
        validToInput : Valid -> ContactFormInput
        validToInput (Valid valid) =
            { type_ = valid.type_
            , label =
                valid.label
                    |> Maybe.withDefault ""
            , value =
                case valid.type_ of
                    ContactType.Email ->
                        valid.value

                    ContactType.Instagram ->
                        -- 22 == String.length "https://instagram.com/"
                        String.dropLeft 22 valid.value

                    ContactType.Link ->
                        if String.startsWith "http://" valid.value then
                            -- 7 == String.length "http://"
                            String.dropLeft 7 valid.value

                        else if String.startsWith "https://" valid.value then
                            -- 8 == String.length "https://"
                            String.dropLeft 8 valid.value

                        else
                            valid.value

                    ContactType.Phone ->
                        valid.value

                    ContactType.Telegram ->
                        -- 13 == String.length "https://t.me/"
                        String.dropLeft 13 valid.value

                    ContactType.Whatsapp ->
                        valid.value
            , deletionStatus = NotDeleted
            }
    in
    FormInput
        { inputs =
            valids
                |> List.indexedMap (\index valid -> ( index, validToInput valid ))
                |> Dict.fromList
        , lastId = List.length valids
        , isTypeModalOpen = False
        }


form : Translation.Translators -> Form.Form msg FormInput (List Valid)
form translators =
    let
        nestedContactForm : Int -> ContactFormInput -> Form.Form msg FormInput Valid
        nestedContactForm id contactFormInput =
            Form.mapValues
                { value = \_ -> contactFormInput
                , update =
                    \newContactFormInput (FormInput values) ->
                        { values | inputs = Dict.insert id newContactFormInput values.inputs }
                            |> FormInput
                }
                (contactForm translators id)
    in
    Form.succeed identity
        |> Form.with
            (Form.introspect
                (\(FormInput { inputs }) ->
                    inputs
                        |> Dict.filter (\_ { deletionStatus } -> deletionStatus /= Deleted)
                        |> Dict.map nestedContactForm
                        |> Dict.values
                        |> Form.list []
                )
            )
        |> Form.withNoOutput
            (Form.arbitrary
                (button
                    [ class "button button-secondary w-full md:w-56 mb-20 mt-10"
                    , onClick openTypeModal
                    , Html.Attributes.type_ "button"
                    ]
                    [ Icons.circledPlus ""
                    , text <| translators.t "contact_form.add"
                    ]
                )
            )
        |> Form.withNoOutput
            (Form.introspect
                (\(FormInput { isTypeModalOpen }) ->
                    let
                        newInputItem : ContactType -> Html (FormInput -> FormInput)
                        newInputItem contactType =
                            li [ class "py-2" ]
                                [ button
                                    [ class "flex items-center w-full py-2 text-gray-333 hover:text-orange-300 group focus-ring rounded-sm"
                                    , onClick (addContactType contactType)
                                    , Html.Attributes.type_ "button"
                                    ]
                                    [ circularIconWithGrayBgInternal
                                        [ class "w-8 h-8 mr-2"
                                        , ariaHidden True
                                        ]
                                        translators
                                        contactType
                                    , text (typeToString translators contactType)
                                    , Icons.arrowDown "-rotate-90 ml-auto text-gray-900 group-hover:text-orange-300 fill-current"
                                    ]
                                ]
                    in
                    Modal.initWith
                        { closeMsg = closeTypeModal
                        , isVisible = isTypeModalOpen
                        }
                        |> Modal.withHeader (translators.t "contact_form.options")
                        |> Modal.withBody
                            [ ul [ class "divide-y divide-gray-100" ]
                                (List.map newInputItem ContactType.list)
                            ]
                        |> Modal.toHtml
                        |> Form.arbitrary
                )
            )


contactForm : Translation.Translators -> Int -> Form.Form msg ContactFormInput Valid
contactForm translators id =
    let
        labelId : String
        labelId =
            "contact-label-" ++ String.fromInt id

        inputId : String
        inputId =
            "contact-input-" ++ String.fromInt id

        slideUpAnimation heightClass deletionStatus =
            [ class "motion-safe:duration-300 motion-safe:ease-in-out motion-safe:animate-fade-in-from-above motion-safe:fill-mode-none"
            , classList
                [ ( "transition-none " ++ heightClass, isNotDeleted deletionStatus )
                , ( "h-0 opacity-0 motion-safe:transition-all", not (isNotDeleted deletionStatus) )
                ]
            ]
    in
    Form.succeed
        (\type_ label value _ ->
            Valid
                { type_ = type_
                , label = label
                , value = value
                }
        )
        |> Form.with
            (Form.introspect
                (\{ type_, deletionStatus } ->
                    Form.arbitraryWith type_
                        (div
                            (class "flex items-end" :: slideUpAnimation "h-14" deletionStatus)
                            [ div
                                [ class "flex items-center" ]
                                [ circularIconWithGrayBgInternal
                                    [ class "w-5 h-5 mb-2 !p-1"
                                    , ariaHidden True
                                    ]
                                    translators
                                    type_
                                , View.Components.label [ class "ml-2" ]
                                    { targetId = inputId
                                    , labelText = typeToString translators type_
                                    }
                                ]
                            ]
                        )
                )
            )
        |> Form.withGroupOf3
            [ class "flex gap-2" ]
            (Form.introspect
                (\{ deletionStatus, type_ } ->
                    Form.Text.init
                        { label = ""
                        , id = labelId
                        }
                        |> Form.Text.withPlaceholder (typeToString translators type_)
                        |> Form.Text.withContainerAttrs
                            (class "mb-0 w-1/3 sm:w-1/2 lg:w-1/3"
                                :: slideUpAnimation "h-12" deletionStatus
                            )
                        |> Form.textField
                            { parser =
                                Form.Validate.succeed
                                    >> Form.Validate.stringLongerThan 2
                                    >> Form.Validate.validate translators
                            , value = .label
                            , update = \newLabel values -> { values | label = newLabel }
                            , externalError = always Nothing
                            }
                        |> Form.optional
                )
            )
            (Form.introspect
                (\{ type_, deletionStatus } ->
                    Form.Text.init
                        { label = ""
                        , id = inputId
                        }
                        |> Form.Text.withPlaceholder (typeToPlaceholder type_)
                        |> Form.Text.withContainerAttrs
                            (class "mb-0 w-2/3 sm:w-1/2 lg:w-2/3"
                                :: slideUpAnimation "h-12" deletionStatus
                            )
                        |> Form.textField
                            { parser =
                                Form.Validate.succeed
                                    >> Form.Validate.stringLongerThan 0
                                    >> (case type_ of
                                            ContactType.Email ->
                                                Form.Validate.email

                                            ContactType.Instagram ->
                                                validateInstagram

                                            ContactType.Link ->
                                                Form.Validate.url
                                                    >> Form.Validate.map Url.toString

                                            ContactType.Phone ->
                                                validatePhone

                                            ContactType.Telegram ->
                                                validateTelegram

                                            ContactType.Whatsapp ->
                                                validatePhone
                                       )
                                    >> Form.Validate.validate translators
                            , value = .value
                            , update = \newValue values -> { values | value = newValue }
                            , externalError = always Nothing
                            }
                )
            )
            (Form.introspect
                (\{ deletionStatus } ->
                    Form.arbitraryWith ()
                        (button
                            (onClick startDeletingContact
                                :: on "transitionend" (Decode.succeed finishDeletingContact)
                                :: Html.Attributes.type_ "button"
                                :: slideUpAnimation "h-12" deletionStatus
                            )
                            [ Icons.circularClose "" ]
                        )
                )
            )
        |> Form.withValidationStrategy Form.ValidateOnSubmit


isNotDeleted : DeletionStatus -> Bool
isNotDeleted deletionStatus =
    deletionStatus == NotDeleted



-- FORM EVENT HANDLERS


openTypeModal : FormInput -> FormInput
openTypeModal (FormInput values) =
    FormInput { values | isTypeModalOpen = True }


closeTypeModal : FormInput -> FormInput
closeTypeModal (FormInput values) =
    FormInput { values | isTypeModalOpen = False }


addContactType : ContactType -> FormInput -> FormInput
addContactType contactType (FormInput values) =
    let
        newEntry : ContactFormInput
        newEntry =
            { type_ = contactType
            , label = ""
            , value = ""
            , deletionStatus = NotDeleted
            }
    in
    FormInput
        { values
            | isTypeModalOpen = False
            , inputs = Dict.insert (values.lastId + 1) newEntry values.inputs
            , lastId = values.lastId + 1
        }


startDeletingContact : ContactFormInput -> ContactFormInput
startDeletingContact values =
    { values | deletionStatus = Deleting }


finishDeletingContact : ContactFormInput -> ContactFormInput
finishDeletingContact values =
    { values | deletionStatus = Deleted }



-- FORM VALIDATION


validatePhone : Form.Validate.Validator String -> Form.Validate.Validator String
validatePhone =
    Form.Validate.custom
        (\phoneNumber ->
            if
                PhoneNumber.valid
                    { defaultCountry = PhoneNumber.Countries.countryBR
                    , otherCountries = PhoneNumber.Countries.all
                    , types = PhoneNumber.anyType
                    }
                    phoneNumber
            then
                Ok phoneNumber

            else
                Err (\{ t } -> t "contact_form.validation.phone.invalid")
        )


validateTelegram : Form.Validate.Validator String -> Form.Validate.Validator String
validateTelegram =
    let
        regex : Regex
        regex =
            Regex.fromString "^[\\w](?!.*?\\.{2})[\\w.]{1,28}[\\w]$"
                |> Maybe.withDefault Regex.never
    in
    Form.Validate.custom
        (\telegram ->
            if Regex.contains regex telegram then
                Ok ("https://t.me/" ++ telegram)

            else
                Err (\{ t } -> t "contact_form.validation.username.invalid")
        )


validateInstagram : Form.Validate.Validator String -> Form.Validate.Validator String
validateInstagram =
    let
        regex : Regex
        regex =
            Regex.fromString "^[\\w](?!.*?\\.{2})[\\w.]{1,28}[\\w]$"
                |> Maybe.withDefault Regex.never
    in
    Form.Validate.custom
        (\instagram ->
            if Regex.contains regex instagram then
                Ok ("https://instagram.com/" ++ instagram)

            else
                Err (\{ t } -> t "contact_form.validation.username.invalid")
        )



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
