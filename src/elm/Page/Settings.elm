module Page.Settings exposing (Model, Msg, init, msgToString, update, view)

import Form.Toggle
import Html exposing (Html, button, div, h2, li, span, text, ul)
import Html.Attributes exposing (class)
import Session.LoggedIn as LoggedIn
import Translation
import UpdateResult as UR
import View.Components



-- MODEL


type alias Model =
    {}


init : LoggedIn.Model -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



-- TYPES


type Msg
    = NoOp


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    { title = "TODO"
    , content =
        div [ class "container mx-auto px-4 mt-6 mb-20" ]
            -- TODO - I18N
            [ h2 [ class "mb-4" ] [ text "Account ", span [ class "font-bold" ] [ text "settings" ] ]
            , ul [ class "bg-white rounded-md p-4 divide-y" ]
                [ viewCardItem
                    -- TODO - I18N
                    [ text "My 12 words"

                    -- TODO - I18N
                    , button [ class "button button-secondary" ] [ text "Download" ]
                    ]
                , viewCardItem
                    -- TODO - I18N
                    [ text "My security PIN"

                    -- TODO - I18N
                    , button [ class "button button-secondary" ] [ text "Change" ]
                    ]
                , viewCardItem
                    [ span [ class "flex items-center" ]
                        [ -- TODO - I18N
                          text "KYC Data"
                        , View.Components.tooltip
                            { message = "TODO"
                            , iconClass = "text-orange-300"
                            , containerClass = ""
                            }
                        ]

                    -- TODO - I18N
                    , button [ class "button button-secondary" ] [ text "Add" ]
                    ]
                ]
            , h2 [ class "mb-4 mt-10" ]
                -- TODO - I18N
                [ span [ class "font-bold" ] [ text "E-mail" ], text " notifications" ]
            , ul [ class "bg-white rounded-md p-4 divide-y" ]
                [ viewCardItem
                    [ viewNotificationToggle loggedIn.shared.translators
                        -- TODO - I18N
                        { label = "Claim notification"
                        , id = "claim_notification"

                        -- TODO
                        , value = False
                        }
                    ]
                , viewCardItem
                    [ viewNotificationToggle loggedIn.shared.translators
                        -- TODO - I18N
                        { label = "Transfer notification"
                        , id = "transfer_notification"

                        -- TODO
                        , value = False
                        }
                    ]
                , viewCardItem
                    [ viewNotificationToggle loggedIn.shared.translators
                        -- TODO - I18N
                        { label = "Monthly news about the community"
                        , id = "digest_notification"

                        -- TODO
                        , value = False
                        }
                    ]
                ]
            ]
    }


viewCardItem : List (Html Msg) -> Html Msg
viewCardItem body =
    li [ class "flex items-center justify-between py-4 first:pt-0 last:pb-0" ] body


viewNotificationToggle :
    Translation.Translators
    ->
        { label : String
        , id : String
        , value : Bool
        }
    -> Html Msg
viewNotificationToggle translators { label, id, value } =
    Form.Toggle.init { label = text label, id = id }
        |> Form.Toggle.withContainerAttrs [ class "w-full" ]
        |> (\options ->
                Form.Toggle.view options
                    -- TODO
                    { onToggle = \_ -> NoOp
                    , onBlur = NoOp
                    , value = value
                    , error = text ""
                    , hasError = False
                    , isRequired = False
                    , translators = translators
                    }
           )



-- UTILS


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]
