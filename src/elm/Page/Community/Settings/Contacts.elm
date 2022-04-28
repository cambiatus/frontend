module Page.Community.Settings.Contacts exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Community
import Form
import Form.Text
import Html exposing (Html, button, div, h2, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Icons
import List.Extra
import Page
import Session.LoggedIn as LoggedIn
import UpdateResult as UR
import View.Components



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
    { inputs : List ContactFormInput
    , lastId : Int
    }


type alias FormOutput =
    List ContactFormOutput


type Msg
    = NoOp
    | CompletedLoadCommunity Community.Model
    | GotFormMsg (Form.Msg FormInput)
    | SubmittedForm FormOutput
    | ClickedRemoveContact Int


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadCommunity community ->
            if community.creator == loggedIn.accountName then
                Form.init
                    -- TODO - Use community contacts
                    { lastId = 0, inputs = [] }
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


createForm : Form.Form Msg FormInput FormOutput
createForm =
    Form.succeed identity
        |> Form.with
            (Form.introspect
                (\values ->
                    values.inputs
                        |> List.indexedMap
                            (\index value ->
                                Form.mapValues
                                    { value = \_ -> value
                                    , update =
                                        \newValue parent ->
                                            { parent
                                                | inputs =
                                                    List.Extra.setAt index
                                                        newValue
                                                        parent.inputs
                                            }
                                    }
                                    contactForm
                            )
                        |> Form.list
                )
            )
        |> Form.withNoOutput
            (Form.arbitrary
                (button
                    [ class "button button-secondary mb-20"
                    , onClick
                        (\values ->
                            { values
                                | inputs =
                                    { id = values.lastId + 1

                                    -- TODO - Make these dynamic
                                    , contactType = Phone
                                    , label = "Phone"
                                    , value = ""
                                    }
                                        :: values.inputs
                                , lastId = values.lastId + 1
                            }
                        )
                    ]
                    [ Icons.circledPlus ""

                    -- TODO - I18N
                    , text "Add contact"
                    ]
                )
            )


type ContactType
    = Phone
    | Whatsapp
    | Telegram


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


contactForm : Form.Form Msg ContactFormInput ContactFormOutput
contactForm =
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
                (\values ->
                    Form.arbitraryWith values.contactType
                        (div [ class "flex items-center mb-4" ]
                            -- TODO - Use circular icon here
                            [ div [ class "w-5 h-5 bg-gray-100 rounded-full" ] []
                            , View.Components.label [ class "mb-0 ml-2" ]
                                { targetId = "value-input-" ++ String.fromInt values.id

                                -- TODO - Make this dynamic
                                , labelText = "Phone Number"
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
                        , id = "label-input-" ++ String.fromInt id
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
                (\{ id } ->
                    Form.Text.init
                        { label = ""
                        , id = "value-input-" ++ String.fromInt id
                        }
                        -- TODO - Make placeholder dynamic
                        |> Form.Text.withPlaceholder "+55 11 91234 5678"
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
                        -- TODO - Add onClick
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
                            , h2 [ class "label mt-10" ] [ text "Contact options" ]
                            , Form.view [ class "flex flex-col flex-grow" ]
                                loggedIn.shared.translators
                                (\submitButton ->
                                    [ submitButton [ class "button button-primary mt-auto" ]
                                        -- TODO - I18N
                                        [ text "Save" ]
                                    ]
                                )
                                createForm
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
        NoOp ->
            [ "NoOp" ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg

        SubmittedForm _ ->
            [ "SubmittedForm" ]

        ClickedRemoveContact _ ->
            [ "ClickedRemoveContact" ]
