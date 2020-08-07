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
import I18Next exposing (Translations, t)
import Icons
import Json.Decode
import Page
import Profile exposing (Profile)
import Route
import Session.LoggedIn exposing (External(..), FeedbackStatus(..))
import Session.Shared exposing (Shared)
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
        title =
            t loggedIn.shared.translations "profile.edit.title"

        content =
            case model.status of
                Loading ->
                    Page.fullPageLoading

                LoadingFailed _ ->
                    Page.fullPageError (t loggedIn.shared.translations "profile.title") Http.Timeout

                Loaded profile ->
                    view_ loggedIn model profile
    in
    { title = title
    , content = content
    }


view_ : Session.LoggedIn.Model -> Model -> Profile -> Html Msg
view_ loggedIn model profile =
    let
        tr s =
            t loggedIn.shared.translations s

        title =
            tr "menu.edit" ++ " " ++ ("menu.profile" |> tr |> String.toLower)

        pageHeader =
            Page.viewHeader loggedIn title Route.Profile

        avatar =
            case model.avatar of
                Just avatar_ ->
                    avatar_

                Nothing ->
                    profile.avatar
    in
    Html.div [ class "bg-white" ]
        [ pageHeader
        , form
            [ class "grid pt-4 gap-4 container mx-auto p-4"
            , style "grid-template" """
                                       "avatar avatar"
                                       "fullname fullname"
                                       "email email" auto
                                       "bio bio"
                                       "location location"
                                       "interests interestButton"
                                       "interestList interestList" auto
                                       "save save" / 1fr auto
                                      """
            ]
            ([ viewAvatar loggedIn avatar
             , viewInput (tr "profile.edit.labels.name") "fullname" FullName model.fullName
             , viewInput (tr "profile.edit.labels.email") "email" Email model.email
             , viewTextArea (tr "profile.edit.labels.bio") "bio" Bio loggedIn.shared model.bio
             , viewInput (tr "profile.edit.labels.localization") "location" Location model.location
             , viewButton (tr "profile.edit.submit") ClickedSave "save" model.wasSaved
             , div [ class "flex flex-wrap", style "grid-area" "interestList" ]
                (model.interests
                    |> List.map viewInterest
                )
             ]
                ++ viewInterests model.interest loggedIn.shared.translations
            )
        ]


viewInput : String -> String -> Field -> String -> Html Msg
viewInput label area field currentValue =
    div [ style "grid-area" area ]
        [ Html.label [ class "input-label" ]
            [ text label ]
        , input
            [ class "w-full input rounded-sm"
            , onInput (OnFieldInput field)
            , value currentValue
            ]
            []
        ]


viewInterests : String -> Translations -> List (Html Msg)
viewInterests interest translations =
    [ viewInput (t translations "profile.edit.labels.interests") "interests" Interest interest
    , button
        [ class "button-secondary px-4 h-12 mt-auto"
        , style "grid-area" "interestButton"
        , onClickPreventDefault AddInterest
        ]
        [ text <| String.toUpper (t translations "menu.add") ]
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


viewTextArea : String -> String -> Field -> Shared -> String -> Html Msg
viewTextArea lbl gridArea field shared currentValue =
    let
        tr : String -> I18Next.Replacements -> String
        tr =
            I18Next.tr shared.translations I18Next.Curly
    in
    div
        [ style "grid-area" gridArea
        ]
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


viewAvatar : Session.LoggedIn.Model -> Avatar.Avatar -> Html Msg
viewAvatar loggedIn url =
    div
        [ class "m-auto relative"
        , style "grid-area" "avatar"
        ]
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
                        UR.addExt (ShowFeedback Success (t loggedIn.shared.translations "profile.edit_success"))

                    else
                        UR.addExt HideFeedback
            in
            UR.init
                { model
                    | status = Loaded profile
                    , fullName = nullable profile.userName
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
        | userName = Just model.fullName
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
