module Contact exposing (FormInput, Valid, form, initFormInput)

import Cambiatus.Enum.ContactType as ContactType exposing (ContactType)
import Dict exposing (Dict)
import Form
import Form.Text
import Html exposing (Html, button, div, li, p, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Attributes.Aria exposing (ariaHidden, ariaLabel)
import Html.Events exposing (on, onClick)
import Icons
import Json.Decode as Decode
import Translation
import View.Components
import View.Modal as Modal



-- VIEWS


circularIconWithGrayBg : List (Html.Attribute Never) -> Translation.Translators -> ContactType -> Html msg
circularIconWithGrayBg attrs translators type_ =
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
        , label : String
        , value : String
        }


initFormInput : List Valid -> FormInput
initFormInput valids =
    let
        validToInput : Valid -> ContactFormInput
        validToInput (Valid valid) =
            { type_ = valid.type_
            , label = valid.label

            -- TODO - Remove unnecessary stuff from `value` (https://, etc.)
            , value = valid.value
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
                    [ class "button button-secondary mb-20 mt-10"
                    , onClick openTypeModal
                    ]
                    [ Icons.circledPlus ""

                    -- TODO - I18N
                    , text "Add contact"
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
                                    , onClick (addContactType translators contactType)
                                    ]
                                    [ circularIconWithGrayBg
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
                        -- TODO - I18N
                        |> Modal.withHeader "Contact options"
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
                                [ circularIconWithGrayBg
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
                (\{ deletionStatus } ->
                    Form.Text.init
                        { label = ""
                        , id = labelId
                        }
                        |> Form.Text.withContainerAttrs
                            (class "max-w-27 mb-0"
                                :: slideUpAnimation "h-12" deletionStatus
                            )
                        |> Form.textField
                            { -- TODO - Don't allow empty strings
                              parser = Ok
                            , value = .label
                            , update = \newLabel values -> { values | label = newLabel }
                            , externalError = always Nothing
                            }
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
                            (class "mb-0"
                                :: slideUpAnimation "h-12" deletionStatus
                            )
                        |> Form.textField
                            { -- TODO - Parse according to type_ (add https://, etc)
                              parser = Ok
                            , value = .value
                            , update = \newValue values -> { values | value = newValue }
                            , externalError = always Nothing
                            }
                )
            )
            (Form.introspect
                (\{ deletionStatus } ->
                    Form.arbitrary
                        (button
                            (onClick startDeletingContact
                                :: on "transitionend" (Decode.succeed finishDeletingContact)
                                :: slideUpAnimation "h-12" deletionStatus
                            )
                            [ Icons.circularClose "" ]
                        )
                )
            )


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


addContactType : Translation.Translators -> ContactType -> FormInput -> FormInput
addContactType translators contactType (FormInput values) =
    let
        sameContactTypeCount : Int
        sameContactTypeCount =
            values.inputs
                |> Dict.values
                |> List.filter (\{ type_, deletionStatus } -> (type_ == contactType) && isNotDeleted deletionStatus)
                |> List.length

        newEntry : ContactFormInput
        newEntry =
            { type_ = contactType
            , label =
                if sameContactTypeCount == 0 then
                    typeToString translators contactType

                else
                    typeToString translators contactType
                        ++ " "
                        ++ String.fromInt (sameContactTypeCount + 1)
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
