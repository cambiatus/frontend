module Page.Profile.Editor exposing (Model, Msg, init, msgToString, update, view)

import Api
import Api.Graphql
import Avatar exposing (Avatar)
import File exposing (File)
import Graphql.Http
import Html exposing (Html, button, div, form, input, label, span, text, textarea)
import Html.Attributes exposing (accept, class, disabled, for, id, multiple, style, type_, value)
import Html.Events exposing (onInput)
import Http
import Icons
import Json.Decode
import Page
import Profile exposing (Profile)
import Route
import Session.LoggedIn exposing (External(..), FeedbackStatus(..))
import Session.Shared exposing (Translators)
import UpdateResult as UR



-- INIT


init : Session.LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    let
        profileQuery =
            Api.Graphql.query loggedIn.shared
                (Profile.query loggedIn.accountName)
                CompletedProfileLoad
    in
    ( initModel
    , profileQuery
    )



-- MODEL


type alias Model =
    { fullName : String
    , email : String
    , bio : String
    , location : String
    , interests : List String
    , interest : String
    , status : Status
    , avatar : Maybe Avatar
    , wasSaved : Bool
    }


initModel : Model
initModel =
    { fullName = ""
    , email = ""
    , bio = ""
    , location = ""
    , interests = []
    , interest = ""
    , status = Loading
    , avatar = Nothing
    , wasSaved = False
    }


type Status
    = Loading
    | LoadingFailed (Graphql.Http.Error (Maybe Profile))
    | Loaded Profile



-- VIEW


view : Session.LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        title =
            t "profile.edit.title"

        content =
            case model.status of
                Loading ->
                    Page.fullPageLoading loggedIn.shared

                LoadingFailed _ ->
                    Page.fullPageError (t "profile.title") Http.Timeout

                Loaded profile ->
                    view_ loggedIn model profile
    in
    { title = title
    , content = content
    }


view_ : Session.LoggedIn.Model -> Model -> Profile -> Html Msg
view_ loggedIn model profile =
    let
        { t } =
            loggedIn.shared.translators

        title =
            t "menu.edit" ++ " " ++ ("menu.profile" |> t |> String.toLower)

        pageHeader =
            Page.viewHeader loggedIn title Route.Profile

        avatar =
            case model.avatar of
                Just avatar_ ->
                    avatar_

                Nothing ->
                    profile.avatar

        viewFullName =
            let
                hasCommunitiesWithKycEnabled =
                    profile.communities
                        |> List.any (\c -> c.hasKyc)
            in
            if hasCommunitiesWithKycEnabled then
                viewDisabledInput

            else
                viewInput
    in
    Html.div [ class "bg-white" ]
        [ pageHeader
        , form
            [ class "pt-4 container mx-auto p-4" ]
            [ viewAvatar avatar
            , viewFullName (t "profile.edit.labels.name") FullName model.fullName
            , viewInput (t "profile.edit.labels.email") Email model.email
            , viewBio (t "profile.edit.labels.bio") Bio loggedIn.shared.translators model.bio
            , viewInput (t "profile.edit.labels.localization") Location model.location
            , viewInterests model.interest model.interests loggedIn.shared.translators
            , viewButton (t "profile.edit.submit") ClickedSave "save" model.wasSaved
            ]
        ]


viewDisabledInput : String -> Field -> String -> Html Msg
viewDisabledInput label field currentValue =
    makeViewInput True label field currentValue


viewInput : String -> Field -> String -> Html Msg
viewInput label field currentValue =
    makeViewInput False label field currentValue


makeViewInput : Bool -> String -> Field -> String -> Html Msg
makeViewInput isDisabled label field currentValue =
    div [ class "mb-4" ]
        [ Html.label [ class "input-label" ]
            [ text label ]
        , input
            [ class "w-full input rounded-sm"
            , class <|
                if isDisabled then
                    "bg-gray-200 text-gray-700"

                else
                    ""
            , disabled isDisabled
            , onInput (OnFieldInput field)
            , value currentValue
            ]
            []
        ]


viewInterests : String -> List String -> Translators -> Html Msg
viewInterests interest interests { t } =
    div [ class "mb-4" ]
        [ Html.label [ class "input-label" ]
            [ text (t "profile.edit.labels.interests") ]
        , div [ class "flex mb-4" ]
            [ input
                [ class "w-full input rounded-sm"
                , onInput (OnFieldInput Interest)
                , value interest
                ]
                []
            , button
                [ class "button-secondary px-4 h-12 align-bottom ml-4"
                , onClickPreventDefault AddInterest
                ]
                [ text <| String.toUpper (t "menu.add") ]
            ]
        , div [ class "flex flex-wrap" ]
            (interests
                |> List.map viewInterest
            )
        ]


viewInterest : String -> Html Msg
viewInterest interest =
    div [ class "bg-green px-3 h-8 rounded-sm text-xs mr-4 mb-1 flex" ]
        [ span [ class "m-auto mr-3 leading-none text-white uppercase" ] [ text interest ]
        , button
            [ class "m-auto"
            , onClickPreventDefault (RemoveInterest interest)
            ]
            [ Icons.close "w-4 h-4 text-white fill-current" ]
        ]


onClickPreventDefault : msg -> Html.Attribute msg
onClickPreventDefault message =
    Html.Events.custom "click" (Json.Decode.succeed { message = message, stopPropagation = True, preventDefault = True })


viewButton : String -> Msg -> String -> Bool -> Html Msg
viewButton label msg area isDisabled =
    button
        [ class "button button-primary w-full"
        , class
            (if isDisabled then
                "button-disabled"

             else
                ""
            )
        , style "grid-area" area
        , onClickPreventDefault msg
        , disabled isDisabled
        ]
        [ text label
        ]


viewBio : String -> Field -> Translators -> String -> Html Msg
viewBio lbl field { tr } currentValue =
    div [ class "mb-4" ]
        [ label [ class "input-label" ] [ text lbl ]
        , div [ class "relative" ]
            [ textarea
                [ class "w-full input"
                , onInput (OnFieldInput field)
                , value currentValue
                ]
                []
            , div [ class "input-label pr-1 text-right text-purple-100 font-bold mt-1 absolute right-0" ]
                [ text <|
                    tr
                        "edit.input_counter"
                        [ ( "current", String.fromInt <| String.length currentValue )
                        , ( "max", "255" )
                        ]
                ]
            ]
        ]


viewAvatar : Avatar.Avatar -> Html Msg
viewAvatar url =
    div
        [ class "m-auto w-20 relative mb-4" ]
        [ input
            [ id "profile-upload-avatar"
            , class "profile-img-input"
            , type_ "file"
            , accept "image/*"
            , Page.onFileChange EnteredAvatar
            , multiple False
            ]
            []
        , label
            [ for "profile-upload-avatar"
            , class "block cursor-pointer"
            ]
            [ Avatar.view url "w-20 h-20"
            , span [ class "absolute bottom-0 right-0 bg-orange-300 w-8 h-8 p-2 rounded-full" ] [ Icons.camera ]
            ]
        ]



-- UPDATE


type Msg
    = CompletedProfileLoad (Result (Graphql.Http.Error (Maybe Profile)) (Maybe Profile))
    | OnFieldInput Field String
    | AddInterest
    | RemoveInterest String
    | ClickedSave
    | EnteredAvatar (List File)
    | CompletedAvatarUpload (Result Http.Error Avatar)


type Field
    = FullName
    | Email
    | Bio
    | Location
    | Interest


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


update : Msg -> Model -> Session.LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        { t } =
            loggedIn.shared.translators
    in
    case msg of
        CompletedProfileLoad (Ok Nothing) ->
            UR.init model

        CompletedProfileLoad (Ok (Just profile)) ->
            let
                nullable a =
                    Maybe.withDefault "" a

                redirect =
                    if model.wasSaved then
                        UR.addCmd (Route.pushUrl loggedIn.shared.navKey Route.Profile)

                    else
                        UR.addCmd Cmd.none

                showSuccessMsg =
                    if model.wasSaved then
                        UR.addExt (ShowFeedback Success (t "profile.edit_success"))

                    else
                        UR.addExt HideFeedback
            in
            UR.init
                { model
                    | status = Loaded profile
                    , fullName = nullable profile.name
                    , email = nullable profile.email
                    , bio = nullable profile.bio
                    , location = nullable profile.localization
                    , interests = profile.interests
                }
                |> redirect
                |> showSuccessMsg

        CompletedProfileLoad (Err err) ->
            UR.init { model | status = LoadingFailed err }
                |> UR.logGraphqlError msg err

        OnFieldInput field data ->
            let
                newModel =
                    case field of
                        FullName ->
                            { model | fullName = data }

                        Email ->
                            { model | email = data }

                        Bio ->
                            let
                                limit =
                                    255

                                limitedBio =
                                    if String.length data < limit then
                                        data

                                    else
                                        String.slice 0 limit data
                            in
                            { model | bio = limitedBio }

                        Location ->
                            { model | location = data }

                        Interest ->
                            { model | interest = data }
            in
            UR.init newModel

        AddInterest ->
            let
                newModel =
                    -- Prevent empty and duplicate interests
                    if model.interest /= "" && not (List.any (\interest -> interest == model.interest) model.interests) then
                        { model | interests = model.interest :: model.interests, interest = "" }

                    else
                        model
            in
            UR.init newModel

        RemoveInterest interest ->
            UR.init
                { model
                    | interests =
                        model.interests
                            |> List.filter (\x -> x /= interest)
                }

        ClickedSave ->
            case model.status of
                Loaded profile ->
                    let
                        newProfile =
                            modelToProfile model profile
                    in
                    { model | wasSaved = True }
                        |> UR.init
                        |> UR.addCmd
                            (Api.Graphql.mutation loggedIn.shared
                                (Profile.mutation profile.account (Profile.profileToForm newProfile))
                                CompletedProfileLoad
                            )

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

        EnteredAvatar (file :: _) ->
            let
                uploadAvatar file_ =
                    Api.uploadAvatar loggedIn.shared file_ CompletedAvatarUpload
            in
            case model.status of
                Loaded _ ->
                    model
                        |> UR.init
                        |> UR.addCmd (uploadAvatar file)

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

        EnteredAvatar [] ->
            UR.init model

        CompletedAvatarUpload (Ok a) ->
            UR.init { model | avatar = Just a }

        CompletedAvatarUpload (Err err) ->
            UR.init model
                |> UR.logHttpError msg err


modelToProfile : Model -> Profile -> Profile
modelToProfile model profile =
    { profile
        | name = Just model.fullName
        , email = Just model.email
        , localization = Just model.location
        , interests = model.interests
        , bio = Just model.bio
        , avatar = Maybe.withDefault profile.avatar model.avatar
    }


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedProfileLoad _ ->
            [ "CompletedProfileLoad" ]

        OnFieldInput _ _ ->
            [ "OnFieldInput" ]

        AddInterest ->
            [ "AddInterest" ]

        RemoveInterest _ ->
            [ "RemoveInterest" ]

        ClickedSave ->
            [ "ClickedSave" ]

        CompletedAvatarUpload _ ->
            [ "CompletedAvatarUpload" ]

        EnteredAvatar _ ->
            [ "EnteredAvatar" ]
