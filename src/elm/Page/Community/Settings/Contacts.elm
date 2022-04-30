module Page.Community.Settings.Contacts exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Cambiatus.Enum.ContactType as ContactType exposing (ContactType)
import Community
import Form
import Form.Text
import Html exposing (Html, button, div, h2, li, p, text, ul)
import Html.Attributes exposing (class)
import Html.Attributes.Aria exposing (ariaHidden)
import Html.Events exposing (onClick)
import Icons
import List.Extra
import Page
import Profile.Contact
import Session.LoggedIn as LoggedIn
import Translation
import UpdateResult as UR
import View.Components
import View.Modal as Modal



-- MODEL


type Model
    = Authorized (Form.Model FormInput)
    | Loading
    | Unauthorized


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( Loading, LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn )



-- TYPES


type alias FormInput =
    -- TODO - Should this be a dict?
    { inputs : List ContactFormInput
    , lastId : Int
    , isContactTypeModalOpen : Bool
    }


type alias FormOutput =
    List ContactFormOutput


type Msg
    = CompletedLoadCommunity Community.Model
    | GotFormMsg (Form.Msg FormInput)
    | SubmittedForm FormOutput
    | ClickedRemoveContact Int


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedLoadCommunity community ->
            if community.creator == loggedIn.accountName then
                Form.init
                    -- TODO - Use community contacts
                    { lastId = 0
                    , inputs = []
                    , isContactTypeModalOpen = False
                    }
                    |> Authorized
                    |> UR.init

            else
                Unauthorized
                    |> UR.init

        GotFormMsg subMsg ->
            case model of
                Authorized formModel ->
                    Form.update loggedIn.shared subMsg formModel
                        |> UR.fromChild (\newFormModel -> Authorized newFormModel)
                            GotFormMsg
                            LoggedIn.addFeedback
                            model

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Got form msg, but wasn't authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.Contacts", function = "update" }
                            []

        SubmittedForm _ ->
            Debug.todo ""

        ClickedRemoveContact id ->
            case model of
                Authorized formInput ->
                    Form.updateValues
                        (\values -> { values | inputs = List.filter (\input -> input.id /= id) values.inputs })
                        formInput
                        |> Authorized
                        |> UR.init

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Clicked remove contact, but wasn't authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.Contacts", function = "update" }
                            []


createForm : Translation.Translators -> Form.Form Msg FormInput FormOutput
createForm translators =
    let
        nestedContactForm : Int -> ContactFormInput -> Form.Form Msg FormInput ContactFormOutput
        nestedContactForm index contactFormInput =
            Form.mapValues
                { value = \_ -> contactFormInput
                , update =
                    \newValue parent ->
                        { parent
                            | inputs =
                                List.Extra.setAt index
                                    newValue
                                    parent.inputs
                        }
                }
                (contactForm translators)

        createNewContactInput : Int -> ContactType -> ContactFormInput
        createNewContactInput id contactType =
            { id = id
            , contactType = contactType
            , label = Profile.Contact.contactTypeToString translators contactType
            , value = ""
            }

        newContactInputItem contactType =
            li [ class "py-2" ]
                [ button
                    [ class "flex items-center w-full py-2 text-gray-333 hover:text-orange-300 group focus-ring rounded-sm"
                    , onClick
                        (\values ->
                            { isContactTypeModalOpen = False
                            , inputs =
                                createNewContactInput (values.lastId + 1) contactType
                                    :: values.inputs
                            , lastId = values.lastId + 1
                            }
                        )
                    ]
                    [ Profile.Contact.circularIconWithGrayBg
                        [ class "w-8 h-8 mr-2"
                        , ariaHidden True
                        ]
                        translators
                        contactType
                    , text <| Profile.Contact.contactTypeToString translators contactType
                    , Icons.arrowDown "-rotate-90 ml-auto text-gray-900 group-hover:text-orange-300 fill-current"
                    ]
                ]
    in
    Form.succeed identity
        |> Form.with
            (Form.introspect
                (\values ->
                    values.inputs
                        |> List.indexedMap nestedContactForm
                        |> Form.list
                )
            )
        |> Form.withNoOutput
            (Form.arbitrary
                (button
                    [ class "button button-secondary mb-20"
                    , onClick (\values -> { values | isContactTypeModalOpen = True })
                    ]
                    [ Icons.circledPlus ""

                    -- TODO - I18N
                    , text "Add contact"
                    ]
                )
            )
        |> Form.withNoOutput
            (Form.introspect
                (\{ isContactTypeModalOpen } ->
                    Form.arbitrary
                        (Modal.initWith
                            { closeMsg = \values -> { values | isContactTypeModalOpen = False }
                            , isVisible = isContactTypeModalOpen
                            }
                            -- TODO - I18N
                            |> Modal.withHeader "Contact options"
                            |> Modal.withBody
                                [ ul [ class "divide-y divide-gray-100" ]
                                    (List.map newContactInputItem ContactType.list)
                                ]
                            |> Modal.toHtml
                        )
                )
            )


type alias ContactFormInput =
    { id : Int
    , contactType : ContactType
    , label : String
    , value : String
    }


type ContactFormOutput
    = ContactFormOutput
        { contactType : ContactType
        , label : String
        , value : String
        }


contactForm : Translation.Translators -> Form.Form Msg ContactFormInput ContactFormOutput
contactForm translators =
    let
        labelId : Int -> String
        labelId id =
            "contact-label-" ++ String.fromInt id

        inputId : Int -> String
        inputId id =
            "contact-input-" ++ String.fromInt id
    in
    Form.succeed
        (\contactType label value _ ->
            ContactFormOutput
                { contactType = contactType
                , label = label
                , value = value
                }
        )
        |> Form.with
            (Form.introspect
                (\{ id, contactType } ->
                    Form.arbitraryWith contactType
                        (div [ class "flex items-center mb-4" ]
                            [ Profile.Contact.circularIconWithGrayBg
                                [ class "w-5 h-5 p-1"
                                , ariaHidden True
                                ]
                                translators
                                contactType
                            , View.Components.label [ class "mb-0 ml-2" ]
                                { targetId = inputId id
                                , labelText = Profile.Contact.contactTypeToString translators contactType
                                }
                            ]
                        )
                )
            )
        |> Form.withGroupOf3
            [ class "flex gap-2" ]
            (Form.introspect
                (\{ id } ->
                    Form.Text.init
                        { label = ""
                        , id = labelId id
                        }
                        |> Form.Text.withContainerAttrs [ class "max-w-27" ]
                        |> Form.textField
                            { parser = Ok
                            , value = .label
                            , update = \newLabel values -> { values | label = newLabel }
                            , externalError = always Nothing
                            }
                )
            )
            (Form.introspect
                (\{ id, contactType } ->
                    Form.Text.init
                        { label = ""
                        , id = inputId id
                        }
                        |> Form.Text.withPlaceholder (Profile.Contact.contactTypePlaceholder contactType)
                        |> Form.textField
                            -- TODO - Parse phone number
                            { parser = Ok
                            , value = .value
                            , update = \newValue values -> { values | value = newValue }
                            , externalError = always Nothing
                            }
                )
            )
            (Form.introspect
                (\{ id } ->
                    Form.unsafeArbitrary
                        -- TODO - Add aria
                        (button
                            [ class "mb-10"
                            , onClick (ClickedRemoveContact id)
                            ]
                            [ Icons.circularClose "" ]
                        )
                )
            )



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        title =
            t "settings.contacts.title"

        content =
            case model of
                Authorized formModel ->
                    div [ class "flex-grow flex flex-col" ]
                        [ Page.viewHeader loggedIn title
                        , div [ class "bg-white flex flex-col flex-grow container mx-auto pt-6 pb-7 px-4 lg:px-6" ]
                            [ p [ class "text-gray-900" ]
                                -- TODO - I18N
                                [ text "Adicione os contatos que ficarão disponíveis como suporte para os membros da comunidade." ]

                            -- TODO - I18N
                            , h2 [ class "label mt-10 mb-6" ] [ text "Contact options" ]
                            , Form.view [ class "flex flex-col flex-grow" ]
                                loggedIn.shared.translators
                                (\submitButton ->
                                    [ submitButton [ class "button button-primary mt-auto" ]
                                        -- TODO - I18N
                                        [ text "Save" ]
                                    ]
                                )
                                (createForm loggedIn.shared.translators)
                                formModel
                                { toMsg = GotFormMsg
                                , onSubmit = SubmittedForm
                                }
                            ]
                        ]

                Loading ->
                    Page.fullPageLoading loggedIn.shared

                Unauthorized ->
                    Page.fullPageNotFound (t "community.edit.unauthorized") ""
    in
    { title = title, content = content }



-- UTILS


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg

        SubmittedForm _ ->
            [ "SubmittedForm" ]

        ClickedRemoveContact _ ->
            [ "ClickedRemoveContact" ]
