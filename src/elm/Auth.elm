module Auth exposing (ExternalMsg(..), Model, Msg, PrivateKeyLogin, init, initRegister, isAuth, jsAddressToMsg, maybePrivateKey, msgToString, subscriptions, update, view, viewFieldLabel)

import Api
import Asset.Icon as Icon
import Browser.Dom as Dom
import Eos.Account as Eos
import Feedback
import Flags
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput, onSubmit)
import Http
import I18Next exposing (t)
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra as LE
import Log
import Profile exposing (Profile)
import Route
import Session.Shared as Shared exposing (Shared)
import Task
import UpdateResult as UR



-- INIT


init : Shared -> Model
init shared =
    case shared.maybeAccount of
        Just ( _, True ) ->
            { initModel | status = LoginWithPin }

        _ ->
            initModel


initRegister : String -> Model
initRegister pk =
    { initModel | status = LoggedInWithPin pk }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MODEL


type alias Model =
    { status : Status
    , loginError : Maybe String
    , form : PrivateKeyLogin
    , pinVisibility : Bool
    }


initModel : Model
initModel =
    { status = Options
    , loginError = Nothing
    , form = initPrivateKeyLogin
    , pinVisibility = True
    }


type Status
    = Options
    | LoginWithPrivateKey PrivateKeyLogin
    | LoginWithPrivateKeyAccounts (List Eos.Name) PrivateKeyLogin
    | LoggingInWithPrivateKeyAccounts (List Eos.Name) PrivateKeyLogin
    | LoggingInWithPrivateKey PrivateKeyLogin
    | LoggedInWithPrivateKey PrivateKey
    | LoginWithPin
    | LoggingInWithPin
    | LoggedInWithPin PrivateKey


type alias PrivateKeyLogin =
    { privateKey : String
    , usePin : Maybe String
    , enteredPin : List (Maybe String)
    }


initPrivateKeyLogin : PrivateKeyLogin
initPrivateKeyLogin =
    { privateKey = ""
    , usePin = Nothing
    , enteredPin = List.repeat 6 Nothing
    }


encodePrivateKeyLogin : PrivateKeyLogin -> Value
encodePrivateKeyLogin pk =
    Encode.object
        [ ( "privateKey", Encode.string pk.privateKey )
        , ( "usePin"
          , case pk.usePin of
                Nothing ->
                    Encode.null

                Just pin ->
                    Encode.string pin
          )
        ]


type alias PrivateKey =
    String


isAuth : Model -> Bool
isAuth model =
    case model.status of
        LoggedInWithPin _ ->
            True

        LoggedInWithPrivateKey _ ->
            True

        _ ->
            False


maybePrivateKey : Model -> Maybe String
maybePrivateKey model =
    case model.status of
        LoggedInWithPin pk ->
            Just pk

        LoggedInWithPrivateKey pk ->
            Just pk

        _ ->
            Nothing



-- VIEW

{-| You're gonna want a comment explaining how this works I reckon. -}
type alias Handlers msg =
    { tagger : Msg -> msg
    , showFeedback : Feedback.Model -> msg
    }


view : Handlers msg -> Bool -> Shared -> Model -> List (Html msg)
view handlers isModal shared model =
    case model.status of
        Options ->
            viewOptions handlers isModal shared model

        LoginWithPrivateKey _ ->
            viewOptions handlers isModal shared model

        LoginWithPrivateKeyAccounts accounts form ->
            viewMultipleAccount accounts form False isModal shared model
                -- This is gross, we'll fix it in post <3
                |> List.map (Html.map handlers.tagger)

        LoggingInWithPrivateKeyAccounts accounts form ->
            viewMultipleAccount accounts form True isModal shared model
                |> List.map (Html.map handlers.tagger)

        LoggingInWithPrivateKey form ->
            viewLoginWithPrivateKeyLogin form True isModal shared model
                |> List.map (Html.map handlers.tagger)

        LoggedInWithPrivateKey _ ->
            viewOptions handlers isModal shared model

        LoginWithPin ->
            case shared.maybeAccount of
                Just ( accountName, True ) ->
                    viewLoginWithPin accountName False isModal shared model
                        |> List.map (Html.map handlers.tagger)

                _ ->
                    viewOptions handlers isModal shared model

        LoggingInWithPin ->
            case shared.maybeAccount of
                Just ( accountName, True ) ->
                    viewLoginWithPin accountName True isModal shared model
                        |> List.map (Html.map handlers.tagger)

                _ ->
                    viewOptions handlers isModal shared model

        LoggedInWithPin _ ->
            case shared.maybeAccount of
                Just ( accountName, True ) ->
                    viewLoginWithPin accountName True isModal shared model
                        |> List.map (Html.map handlers.tagger)

                _ ->
                    viewOptions handlers isModal shared model


viewOptions : Handlers msg -> Bool -> Shared -> Model -> List (Html msg)
viewOptions h isModal shared model =
    let
        text_ s =
            Html.text (t shared.translations s)
    in
    [ div [ class "" ]
        [ if not isModal then
            viewAuthTabs shared
                |> Html.map h.tagger

          else
            text ""
        , viewAuthError shared model.loginError
            |> Html.map h.tagger
        ]
    , div
        [ onClick (h.showFeedback { message = "Hi!", success = False }) ]
        [ text "clickme!" ]
    , div [ class "card__auth__input" ]
        [ viewFieldLabel shared "auth.login.wordsMode.input" "privateKey" Nothing
        , input
            [ class "input auth__input"
            , type_ "text"
            , id "privateKey"
            , value model.form.privateKey
            , onInput (h.tagger << EnteredPrivateKey)
            , required True
            , autocomplete False
            ]
            []
        ]
    , div []
        [ div [ class "card__auth__pin__form" ]
            [ viewLoginPinForm model shared 
                |> Html.map h.tagger
            ]
        ]
    , button
        [ class "btn btn--primary btn--login"
        , onClick (h.tagger <| SubmittedLoginPrivateKey model.form)
        ]
        [ text_ "auth.login.submit" ]
    , if not isModal then
        a [ Route.href (Route.Register Nothing Nothing), class "card__auth__prompt" ]
            [ span [] [ text_ "auth.login.register" ]
            , span [ class "card__auth__login__mode" ] [ text_ "auth.login.registerLink" ]
            ]

      else
        text ""
    ]


viewLoginWithPrivateKeyLogin : PrivateKeyLogin -> Bool -> Bool -> Shared -> Model -> List (Html Msg)
viewLoginWithPrivateKeyLogin form isDisabled isModal shared model =
    let
        text_ s =
            Html.text (t shared.translations s)
    in
    [ div [ class "card__login-header" ]
        [ h2 [ class "card__title" ]
            [ text_ "auth.loginPrivatekey" ]
        , viewAuthError shared model.loginError
        , button
            [ class "card__close-btn"
            , onClick ClickedViewOptions
            , type_ "button"
            , disabled isDisabled
            , title (t shared.translations "menu.cancel")
            ]
            [ Icon.close "" ]
        ]
    , Html.form
        [ onSubmit (SubmittedLoginPrivateKey form) ]
        [ div [ class "input-group" ]
            [ input
                [ class "input input--login flex100"
                , type_ "text"
                , value form.privateKey
                , onInput EnteredPrivateKey
                , placeholder (t shared.translations "auth.loginPrivatekeyPlaceholder")
                , required True
                , disabled isDisabled
                ]
                []
            , button
                [ class "btn btn--primary btn--login flex000"
                , disabled isDisabled
                ]
                [ text_ "auth.login" ]
            ]
        ]
    ]


viewMultipleAccount : List Eos.Name -> PrivateKeyLogin -> Bool -> Bool -> Shared -> Model -> List (Html Msg)
viewMultipleAccount accounts form isDisabled isModal shared model =
    let
        text_ s =
            Html.text (t shared.translations s)

        btnClass =
            class "btn btn--outline btn--login"
    in
    div [ class "card__login-header" ]
        [ h2 [ class "card__title" ]
            [ text_ "auth.chooseAccount" ]
        , viewAuthError shared model.loginError
        , button
            [ class "card__close-btn"
            , onClick ClickedViewOptions
            , type_ "button"
            , disabled isDisabled
            , title (t shared.translations "menu.cancel")
            ]
            [ Icon.close "" ]
        ]
        :: List.map
            (\a ->
                button
                    [ btnClass
                    , disabled isDisabled
                    , onClick (ClickedPrivateKeyAccount a form)
                    ]
                    [ text (Eos.nameToString a) ]
            )
            accounts


viewLoginWithPin : Eos.Name -> Bool -> Bool -> Shared -> Model -> List (Html Msg)
viewLoginWithPin accountName isDisabled isModal shared model =
    let
        text_ s =
            Html.text (t shared.translations s)

        tr id_ replaces =
            I18Next.tr shared.translations I18Next.Curly id_ replaces
    in
    [ div [ class "card__login-header" ]
        [ p [ class "card__pin__prompt" ]
            [ text (tr "auth.loginPin" [ ( "accountName", Eos.nameToString accountName ) ]) ]
        , viewAuthError shared model.loginError
        ]
    , Html.form
        [ class "card__pin__input__group"
        , onSubmit SubmittedLoginPIN
        ]
        [ viewLoginPinForm model shared
        , button
            [ class "btn btn--primary btn--login flex000"
            , disabled isDisabled
            ]
            [ text_ "auth.login.submit" ]
        ]
    ]


viewAuthTabs : Shared -> Html msg
viewAuthTabs { translations } =
    let
        text_ : String -> Html msg
        text_ s =
            text (t translations s)
    in
    div [ class "card__auth__tabs__login" ]
        [ div [ class "disabled" ]
            [ a [ Route.href (Route.Register Nothing Nothing) ]
                [ p [] [ text_ "auth.login.registerTab" ] ]
            ]
        , div [ class "enabled" ] [ p [] [ text_ "auth.login.loginTab" ] ]
        ]


viewAuthError : Shared -> Maybe String -> Html msg
viewAuthError shared maybeLoginError =
    case maybeLoginError of
        Nothing ->
            text ""

        Just error ->
            div [ class "bg-red border-lg rounded p-4 mt-2" ]
                [ p [ class "text-white" ] [ text (t shared.translations "error.accountNotFound") ]
                ]


viewLoginPinForm : Model -> Shared -> Html Msg
viewLoginPinForm model shared =
    let
        inputs =
            List.range 0 5
                |> List.map (\pos -> digitInput pos model)
    in
    div [ class "card__auth__pin__section" ]
        [ viewFieldLabel shared "auth.pin" "pin_input_0" (Just (toggleViewPin model))
        , div [] inputs
        ]


toggleViewPin : Model -> Html Msg
toggleViewPin model =
    button [ class "", onClick TogglePinVisibility ]
        [ if model.pinVisibility then
            img [ src "/icons/eye-show.svg" ] []

          else
            img [ src "/icons/eye-close.svg" ] []
        ]


digitInput : Int -> Model -> Html Msg
digitInput position { form, pinVisibility } =
    let
        itemVal =
            Maybe.andThen
                identity
                (LE.getAt position form.enteredPin)

        val =
            case itemVal of
                Just dig ->
                    dig

                Nothing ->
                    ""

        passwordAttributes =
            if pinVisibility then
                [ type_ "number"
                ]

            else
                [ type_ "password"
                , class "card__auth__pin__input__password"
                , attribute "inputmode" "numeric"
                ]
    in
    input
        ([ class "card__auth__pin__input appearance-none"
         , id ("pin_input_" ++ String.fromInt position)
         , pattern "[0-9]*"
         , maxlength 1
         , value val
         , onInput (EnteredPinDigit position)
         , required True
         , autocomplete False
         ]
            ++ passwordAttributes
        )
        []


viewFieldLabel : Shared -> String -> String -> Maybe (Html msg) -> Html msg
viewFieldLabel { translations } tSuffix id_ maybeView =
    let
        labelText : String
        labelText =
            t translations (tSuffix ++ ".label")

        tooltipText : String
        tooltipText =
            t translations (tSuffix ++ ".tooltip")
    in
    label [ for id_ ]
        [ div [ class "tooltip__text" ]
            [ span [ class "card__auth__label" ] [ Html.text labelText ]
            , Maybe.withDefault (text "") maybeView
            , if String.isEmpty tooltipText then
                div [] [ Html.text "" ]

              else
                button
                    [ class "tooltip"
                    , type_ "button"
                    , attribute "tooltip" tooltipText
                    ]
                    [ img [ src "/icons/tooltip.svg" ] [] ]
            ]
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg ExternalMsg


type Msg
    = Ignored
    | ClickedViewOptions
    | EnteredPrivateKey String
    | SubmittedLoginPrivateKey PrivateKeyLogin
    | GotMultipleAccountsLogin (List Eos.Name)
    | ClickedPrivateKeyAccount Eos.Name PrivateKeyLogin
    | GotPrivateKeyLogin (Result String ( Eos.Name, String ))
    | SubmittedLoginPIN
    | GotPinLogin (Result String ( Eos.Name, String ))
    | CompletedLoadProfile Status Eos.Name (Result Http.Error Profile)
    | CompletedCreateProfile Status Eos.Name (Result Http.Error Profile)
    | EnteredPinDigit Int String
    | TogglePinVisibility


type ExternalMsg
    = ClickedCancel
    | CompletedAuth Profile
    | UpdatedShared Shared


update : Msg -> Shared -> Model -> UpdateResult
update msg shared model =
    case msg of
        Ignored ->
            UR.init model

        ClickedViewOptions ->
            UR.init
                { model
                    | loginError = Nothing
                    , status = Options
                }

        EnteredPrivateKey s ->
            let
                currentForm =
                    model.form

                newForm =
                    { currentForm | privateKey = s }
            in
            { model | form = newForm }
                |> UR.init

        SubmittedLoginPrivateKey form ->
            if List.any (\a -> a == Nothing) form.enteredPin then
                { model | loginError = Just "Please fill in all the PIN digits" }
                    |> UR.init

            else
                let
                    pinString =
                        form.enteredPin
                            |> List.map (\a -> Maybe.withDefault "" a)
                            |> List.foldl (\a b -> a ++ b) ""

                    newForm =
                        { form | usePin = Just pinString }
                in
                { model | form = newForm }
                    |> UR.init
                    |> UR.addPort
                        { responseAddress = SubmittedLoginPrivateKey form
                        , responseData = Encode.null
                        , data =
                            Encode.object
                                [ ( "name", Encode.string "loginWithPrivateKey" )
                                , ( "form", encodePrivateKeyLogin newForm )
                                ]
                        }

        GotMultipleAccountsLogin accounts ->
            UR.init
                { model
                    | status =
                        case model.status of
                            LoggingInWithPrivateKey form ->
                                LoginWithPrivateKeyAccounts accounts form

                            _ ->
                                model.status
                }

        ClickedPrivateKeyAccount accountName form ->
            UR.init
                { model
                    | status =
                        case model.status of
                            LoginWithPrivateKeyAccounts accounts frm ->
                                LoggingInWithPrivateKeyAccounts accounts frm

                            _ ->
                                model.status
                }
                |> UR.addPort
                    { responseAddress = ClickedPrivateKeyAccount accountName form
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "loginWithPrivateKeyAccount" )
                            , ( "accountName", Eos.encodeName accountName )
                            , ( "form", encodePrivateKeyLogin form )
                            ]
                    }

        GotPrivateKeyLogin (Ok ( accountName, privateKey )) ->
            UR.init model
                |> UR.addCmd (Api.signIn shared accountName (CompletedLoadProfile (LoggedInWithPrivateKey privateKey) accountName))

        GotPrivateKeyLogin (Err err) ->
            UR.init
                { model
                    | loginError = Just err
                    , status =
                        case model.status of
                            LoggingInWithPrivateKey form ->
                                LoginWithPrivateKey form

                            LoggingInWithPrivateKeyAccounts accounts form ->
                                LoginWithPrivateKeyAccounts accounts form

                            _ ->
                                model.status
                }

        SubmittedLoginPIN ->
            case List.any (\a -> a == Nothing) model.form.enteredPin of
                True ->
                    { model | loginError = Just "Please fill in all the PIN digits" }
                        |> UR.init

                False ->
                    let
                        pinString =
                            model.form.enteredPin
                                |> List.map (\a -> Maybe.withDefault "" a)
                                |> List.foldl (\a b -> a ++ b) ""
                    in
                    UR.init { model | status = LoggingInWithPin }
                        |> UR.addPort
                            { responseAddress = SubmittedLoginPIN
                            , responseData = Encode.null
                            , data =
                                Encode.object
                                    [ ( "name", Encode.string "loginWithPin" )
                                    , ( "pin", Encode.string pinString )
                                    ]
                            }

        GotPinLogin (Ok ( accountName, privateKey )) ->
            UR.init model
                |> UR.addCmd (Api.signIn shared accountName (CompletedLoadProfile (LoggedInWithPin privateKey) accountName))

        GotPinLogin (Err err) ->
            UR.init
                { model
                    | loginError = Just err
                    , status =
                        case model.status of
                            LoggingInWithPin ->
                                LoginWithPin

                            _ ->
                                model.status
                }

        CompletedLoadProfile newStatus accountName (Ok profile) ->
            UR.init { model | status = newStatus }
                |> UR.addExt (CompletedAuth profile)
                |> UR.addPort
                    { responseAddress = CompletedLoadProfile newStatus accountName (Ok profile)
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "chatCredentials" )
                            , ( "container", Encode.string "chat-manager" )
                            , ( "credentials", Profile.encodeProfileChat profile )
                            , ( "notificationAddress"
                              , Encode.list Encode.string [ "GotPageMsg", "GotLoggedInMsg", "ReceivedNotification" ]
                              )
                            ]
                    }

        CompletedLoadProfile newStatus accountName (Err err) ->
            case err of
                Http.BadStatus 404 ->
                    UR.init model
                        |> UR.addCmd
                            (Api.signUp shared
                                { name = ""
                                , email = ""
                                , account = accountName
                                , invitationId = Nothing
                                }
                                (CompletedCreateProfile newStatus accountName)
                            )

                _ ->
                    loginFailed err model

        CompletedCreateProfile newStatus accountName (Ok _) ->
            UR.init model
                |> UR.addCmd (Api.signIn shared accountName (CompletedLoadProfile newStatus accountName))

        CompletedCreateProfile _ _ (Err err) ->
            loginFailed err model

        EnteredPinDigit pos data ->
            let
                currentForm =
                    model.form

                newPin =
                    if data == "" then
                        LE.setAt pos Nothing model.form.enteredPin

                    else
                        LE.setAt pos (Just data) model.form.enteredPin

                nextFocusPosition : Int
                nextFocusPosition =
                    if data == "" then
                        pos - 1

                    else
                        pos + 1
            in
            { model | form = { currentForm | enteredPin = newPin } }
                |> UR.init
                |> UR.addPort
                    { responseAddress = Ignored
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "pos", Encode.int pos )
                            , ( "data", Encode.string data )
                            , ( "iswithinif", Encode.bool True )
                            ]
                    }
                |> UR.addCmd (Task.attempt (\_ -> Ignored) (Dom.focus ("pin_input_" ++ String.fromInt nextFocusPosition)))

        TogglePinVisibility ->
            { model | pinVisibility = not model.pinVisibility } |> UR.init


loginFailed : Http.Error -> Model -> UpdateResult
loginFailed httpError model =
    UR.init
        { model
            | loginError =
                case httpError of
                    Http.BadStatus code ->
                        Just (String.fromInt code)

                    _ ->
                        Just "Auth failed"
            , status =
                case model.status of
                    LoggingInWithPrivateKeyAccounts accounts form ->
                        LoginWithPrivateKeyAccounts accounts form

                    LoggingInWithPrivateKey pk ->
                        LoginWithPrivateKey pk

                    LoggingInWithPin ->
                        LoginWithPin

                    _ ->
                        Options
        }
        |> UR.addCmd (Log.httpError httpError)
        |> UR.addPort
            { responseAddress = Ignored
            , responseData = Encode.null
            , data =
                Encode.object
                    [ ( "name", Encode.string "logout" )
                    , ( "container", Encode.string "chat-manager" )
                    ]
            }


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "SubmittedLoginPrivateKey" :: [] ->
            decodeAccountNameOrStringError GotPrivateKeyLogin val

        "ClickedPrivateKeyAccount" :: [] ->
            decodeAccountNameOrStringError GotPrivateKeyLogin val

        "SubmittedLoginPIN" :: [] ->
            decodeAccountNameOrStringError GotPinLogin val

        _ ->
            Nothing


decodeAccountNameOrStringError : (Result String ( Eos.Name, String ) -> Msg) -> Value -> Maybe Msg
decodeAccountNameOrStringError toMsg value =
    Decode.decodeValue
        (Decode.oneOf
            [ Decode.succeed Tuple.pair
                |> Decode.required "accountName" Eos.nameDecoder
                |> Decode.required "privateKey" Decode.string
                |> Decode.map (Ok >> toMsg)
            , Decode.field "accountNames" (Decode.list Eos.nameDecoder)
                |> Decode.map GotMultipleAccountsLogin
            , Decode.field "error" Decode.string
                |> Decode.map (Err >> toMsg)
            ]
        )
        value
        |> Result.toMaybe


msgToString : Msg -> List String
msgToString msg =
    case msg of
        Ignored ->
            [ "Ignored" ]

        ClickedViewOptions ->
            [ "ClickedViewOptions" ]

        EnteredPrivateKey _ ->
            [ "EnteredPrivateKey" ]

        SubmittedLoginPrivateKey _ ->
            [ "SubmittedLoginPrivateKey" ]

        GotMultipleAccountsLogin _ ->
            [ "GotMultipleAccountsLogin" ]

        ClickedPrivateKeyAccount _ _ ->
            [ "ClickedPrivateKeyAccount" ]

        GotPrivateKeyLogin r ->
            [ "GotPrivateKeyLogin", UR.resultToString r ]

        SubmittedLoginPIN ->
            [ "SubmittedLoginPIN" ]

        GotPinLogin r ->
            [ "GotPinLogin", UR.resultToString r ]

        CompletedLoadProfile _ _ r ->
            [ "CompletedLoadProfile", UR.resultToString r ]

        CompletedCreateProfile _ _ r ->
            [ "CompletedCreateProfile", UR.resultToString r ]

        EnteredPinDigit _ _ ->
            [ "EnteredPinDigit" ]

        TogglePinVisibility ->
            [ "TogglePinVisibility" ]
