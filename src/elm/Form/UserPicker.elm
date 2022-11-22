module Form.UserPicker exposing
    ( init, Options, map
    , withDisabled, withContainerAttrs, withMenuClass, withModalSelectors
    , getId, isEmpty
    , view, ViewConfig, mapViewConfig
    , Model, update, Msg, msgToString
    , MultiplePickerModel, initMultiple, fromMultiplePicker, toMultiplePicker, getMultipleProfiles
    , SinglePickerModel, initSingle, fromSinglePicker, toSinglePicker, getSingleProfile, setSingle
    , getCurrentQuery
    )

{-| Creates a Cambiatus-style UserPicker. Use it within a `Form.Form`:

    Form.UserPicker.init
        { label = "Pick validators"
        , currentUser = loggedIn.accountName
        }


# Initializing

@docs init, Options, map


# Helpers


## Adding attributes

@docs withDisabled, withContainerAttrs, withMenuClass, withModalSelectors


# Getters

@docs getId, isEmpty


# View

@docs view, ViewConfig, mapViewConfig


# The elm architecture

This is how you actually use this component!

@docs Model, initModel, update, Msg, msgToString


## Multiple users picker

@docs MultiplePickerModel, initMultiple, fromMultiplePicker, toMultiplePicker, getMultipleProfiles


## Single user picker

@docs SinglePickerModel, initSingle, fromSinglePicker, toSinglePicker, getSingleProfile, setSingle


## General helpers

@docs getCurrentQuery

-}

import Avatar
import Eos.Account
import Html exposing (Html, button, div, li, span, ul)
import Html.Attributes exposing (class, classList, id, type_)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (onClick)
import Icons
import List.Extra
import Maybe.Extra
import Profile
import Profile.Summary
import Session.Shared as Shared
import Simple.Fuzzy
import View.Components
import View.Select



-- OPTIONS


type Options msg
    = Options
        { label : String
        , disabled : Bool
        , currentUser : Eos.Account.Name
        , profiles : List Profile.Minimal
        , containerAttrs : List (Html.Attribute msg)
        , menuClass : String
        , profileSummarySelectors : { relative : Maybe String, scroll : Maybe String }
        , areProfilesRelative : Bool
        }


{-| Initializes a UserPicker
-}
init :
    { label : String
    , currentUser : Eos.Account.Name
    , profiles : List Profile.Minimal
    }
    -> Options msg
init { label, currentUser, profiles } =
    Options
        { label = label
        , disabled = False
        , currentUser = currentUser
        , profiles = profiles
        , containerAttrs = []
        , menuClass = ""
        , profileSummarySelectors = { relative = Nothing, scroll = Nothing }
        , areProfilesRelative = True
        }


{-| Change the kind of `msg` on an Options record
-}
map : (msg -> mappedMsg) -> Options msg -> Options mappedMsg
map fn (Options options) =
    Options
        { label = options.label
        , disabled = options.disabled
        , currentUser = options.currentUser
        , profiles = options.profiles
        , containerAttrs = List.map (Html.Attributes.map fn) options.containerAttrs
        , menuClass = options.menuClass
        , profileSummarySelectors = { relative = Nothing, scroll = Nothing }
        , areProfilesRelative = options.areProfilesRelative
        }



-- ADDING ATTRIBUTES


{-| Determines if the UserPicker should be disabled
-}
withDisabled : Bool -> Options msg -> Options msg
withDisabled disabled (Options options) =
    Options { options | disabled = disabled }


{-| Add attributes to the element that holds the label, input and error
-}
withContainerAttrs : List (Html.Attribute msg) -> Options msg -> Options msg
withContainerAttrs attrs (Options options) =
    Options { options | containerAttrs = options.containerAttrs ++ attrs }


{-| Add some classes to the list that displays all users
-}
withMenuClass : String -> Options msg -> Options msg
withMenuClass menuClass (Options options) =
    Options { options | menuClass = options.menuClass ++ " " ++ menuClass }


{-| Apply some JS/CSS magic to make the Profile.Summary work properly on modals.
Use this whenever you're displaying this component inside a modal, otherwise the
Profile.Summary may look broken.
-}
withModalSelectors : Bool -> Options msg -> Options msg
withModalSelectors useModalSelectors (Options options) =
    Options
        { options
            | profileSummarySelectors =
                if useModalSelectors then
                    { relative = Just ".modal-content", scroll = Just ".modal-body" }

                else
                    { relative = Nothing, scroll = Nothing }
            , areProfilesRelative = not useModalSelectors
        }



-- VIEW


type alias ViewConfig msg =
    { onBlur : msg
    , value : Model
    , error : Html msg
    , hasError : Bool
    , translators : Shared.Translators
    }


{-| Change the kind of `msg` on a ViewConfig record
-}
mapViewConfig : (msg -> mappedMsg) -> ViewConfig msg -> ViewConfig mappedMsg
mapViewConfig fn viewConfig =
    { onBlur = fn viewConfig.onBlur
    , value = viewConfig.value
    , error = Html.map fn viewConfig.error
    , hasError = viewConfig.hasError
    , translators = viewConfig.translators
    }


settings : Options msg -> ViewConfig msg -> View.Select.Config Msg Profile.Minimal
settings (Options options) viewConfig =
    let
        { t } =
            viewConfig.translators

        minQueryLength =
            2

        toLabel : Profile.Minimal -> String
        toLabel p =
            Eos.Account.nameToString p.account
    in
    View.Select.newConfig
        { onSelect = SelectedUser
        , toLabel = toLabel
        , filter =
            \query items ->
                if String.length query < minQueryLength then
                    Nothing

                else
                    let
                        accountItems =
                            Simple.Fuzzy.filter toLabel query items

                        nameItems : List Profile.Minimal
                        nameItems =
                            Simple.Fuzzy.filter
                                (\profile -> Maybe.withDefault "" profile.name)
                                query
                                items
                    in
                    (accountItems ++ nameItems)
                        |> List.Extra.unique
                        |> Just
        , onFocusItem = NoOp
        }
        |> View.Select.withInputClass "w-full input"
        |> View.Select.withInputClassList [ ( "with-error", viewConfig.hasError ) ]
        |> View.Select.withDisabled options.disabled
        |> View.Select.withItemClass "bg-indigo-500 hover:bg-opacity-80 focus:bg-opacity-80 active:bg-opacity-90"
        |> View.Select.withPrompt (t "community.actions.form.verifier_placeholder")
        |> View.Select.withItemHtml viewUserItem
        |> View.Select.withNotFound (t "community.actions.form.verifier_not_found")
        |> View.Select.withNotFoundClass "text-red border-solid border-gray-100 border rounded !bg-white px-8 max-w-max"
        |> View.Select.withMenuClass ("flex flex-col w-full border-t-none border-solid border-gray-100 border rounded-sm z-30 bg-white max-h-80 overflow-auto " ++ options.menuClass)
        |> View.Select.withEmptyMenuClass "bg-indigo-500 px-4 py-1"
        |> View.Select.withOnBlur BlurredPicker


viewUserItem : Profile.Minimal -> Html Never
viewUserItem { avatar, name, account } =
    div [ class "flex flex-row items-center z-30 pt-4 pb-2 px-4" ]
        [ Avatar.view avatar "h-10 w-10 mr-4"
        , div [ class "flex flex-col border-dotted border-b border-gray-500 pb-1 w-full text-white text-left" ]
            [ span [ class "font-bold" ]
                [ Html.text <| Maybe.withDefault "" name ]
            , span [ class "font-light" ]
                [ Html.text <| Eos.Account.nameToString account ]
            ]
        ]


view : Options msg -> ViewConfig msg -> (Msg -> msg) -> Html msg
view ((Options options) as wrappedOptions) viewConfig toMsg =
    let
        (Model model) =
            viewConfig.value

        selectedProfiles =
            case model.selectedProfile of
                Multiple profiles_ ->
                    profiles_

                Single Nothing ->
                    []

                Single (Just profile) ->
                    [ profile ]
    in
    div (class "mb-10" :: options.containerAttrs)
        [ View.Components.label [] { targetId = model.id, labelText = options.label }
        , View.Select.view (settings wrappedOptions viewConfig)
            model.selectState
            (List.filter
                (\profile ->
                    List.map .profile selectedProfiles
                        |> List.member profile
                        |> not
                )
                options.profiles
            )
            (List.map .profile selectedProfiles)
            |> Html.map (toMsg << GotSelectMsg)
        , viewConfig.error
        , ul [ class "mt-4 flex flex-wrap gap-6" ]
            (List.indexedMap
                (\index profile ->
                    viewSelectedProfile index wrappedOptions viewConfig profile
                        |> Html.map toMsg
                )
                selectedProfiles
            )
        ]


viewSelectedProfile :
    Int
    -> Options msg
    -> ViewConfig msg
    -> ProfileWithSummary
    -> Html Msg
viewSelectedProfile index (Options options) viewConfig { profile, summary } =
    let
        (Model model) =
            viewConfig.value

        addRelativeSelector =
            case options.profileSummarySelectors.relative of
                Nothing ->
                    Profile.Summary.withRelativeSelector ("#" ++ model.id ++ "-" ++ String.fromInt index)

                Just selector ->
                    Profile.Summary.withRelativeSelector selector

        addScrollSelector =
            case options.profileSummarySelectors.relative of
                Nothing ->
                    identity

                Just selector ->
                    Profile.Summary.withScrollSelector selector
    in
    li
        [ class "flex flex-col items-center"
        , classList [ ( "relative", options.areProfilesRelative ) ]
        , id (model.id ++ "-" ++ String.fromInt index)
        ]
        [ summary
            |> addRelativeSelector
            |> addScrollSelector
            |> Profile.Summary.view viewConfig.translators
                options.currentUser
                profile
            |> Html.map (GotProfileSummaryMsg profile)
        , button
            [ class "hover:opacity-80 focus-ring focus:ring-red focus:ring-opacity-30 rounded-sm mt-2"
            , onClick (ClickedRemoveProfile profile)
            , type_ "button"
            , ariaLabel <|
                viewConfig.translators.tr "community.actions.form.unselect_user"
                    [ ( "username"
                      , Maybe.withDefault
                            (Eos.Account.nameToString profile.account)
                            profile.name
                      )
                    ]
            ]
            [ Icons.trash "text-red" ]
        ]



-- GETTERS


getId : Model -> String
getId (Model model) =
    model.id



-- THE ELM ARCHITECTURE
-- MODEL


type Model
    = Model
        { selectState : View.Select.State
        , id : String
        , selectedProfile : SelectedProfile
        }


type SinglePickerModel
    = SinglePickerModel
        { selectState : View.Select.State
        , id : String
        , selectedProfile : Maybe ProfileWithSummary
        }


type MultiplePickerModel
    = MultiplePickerModel
        { selectState : View.Select.State
        , id : String
        , selectedProfiles : List ProfileWithSummary
        }


type SelectedProfile
    = Single (Maybe ProfileWithSummary)
    | Multiple (List ProfileWithSummary)


type alias ProfileWithSummary =
    { profile : Profile.Minimal
    , summary : Profile.Summary.Model
    }


{-| Initialize a `Model` that will take can have multiple profiles selected at
once
-}
initMultiple : { id : String, selectedProfiles : List Profile.Minimal } -> MultiplePickerModel
initMultiple { id, selectedProfiles } =
    MultiplePickerModel
        { selectState = View.Select.newState id
        , id = id
        , selectedProfiles =
            List.map
                (\profile ->
                    { profile = profile
                    , summary = Profile.Summary.init False
                    }
                )
                selectedProfiles
        }


{-| Initialize a `Model` that will have at most 1 selected profile at a time.
Selecting a new profile will replace the previously selected one.
-}
initSingle : { id : String } -> SinglePickerModel
initSingle { id } =
    SinglePickerModel
        { selectState = View.Select.newState id
        , id = id
        , selectedProfile = Nothing
        }


{-| Set or reset the selected profile on a `SinglePickerModel`. Useful if you
load a profile after initializing the picker.
-}
setSingle : Maybe Profile.Minimal -> SinglePickerModel -> SinglePickerModel
setSingle maybeProfile (SinglePickerModel model) =
    SinglePickerModel
        { model
            | selectedProfile =
                maybeProfile
                    |> Maybe.map
                        (\profile ->
                            { profile = profile
                            , summary = Profile.Summary.init False
                            }
                        )
        }



-- UPDATE


type Msg
    = NoOp
    | GotSelectMsg (View.Select.Msg Profile.Minimal)
    | SelectedUser Profile.Minimal
    | GotProfileSummaryMsg Profile.Minimal Profile.Summary.Msg
    | ClickedRemoveProfile Profile.Minimal
    | BlurredPicker


update : Options msg -> ViewConfig msg -> Msg -> Model -> ( Model, Cmd Msg, Maybe msg )
update options viewConfig msg (Model model) =
    case msg of
        NoOp ->
            ( Model model, Cmd.none, Nothing )

        GotSelectMsg subMsg ->
            let
                ( newState, cmd ) =
                    View.Select.update (settings options viewConfig)
                        subMsg
                        model.selectState
            in
            ( Model { model | selectState = newState }
            , cmd
            , Nothing
            )

        SelectedUser profile ->
            ( Model
                { model
                    | selectedProfile =
                        case model.selectedProfile of
                            Single _ ->
                                { profile = profile
                                , summary = Profile.Summary.init False
                                }
                                    |> Just
                                    |> Single

                            Multiple profiles ->
                                Multiple
                                    ({ profile = profile
                                     , summary = Profile.Summary.init False
                                     }
                                        :: profiles
                                    )
                }
            , Cmd.none
            , Nothing
            )

        GotProfileSummaryMsg targetProfile subMsg ->
            ( Model
                { model
                    | selectedProfile =
                        case model.selectedProfile of
                            Single Nothing ->
                                Single Nothing

                            Single (Just profile) ->
                                { profile | summary = Profile.Summary.update subMsg profile.summary }
                                    |> Just
                                    |> Single

                            Multiple profiles ->
                                profiles
                                    |> List.Extra.updateIf
                                        (.profile >> (==) targetProfile)
                                        (\profile -> { profile | summary = Profile.Summary.update subMsg profile.summary })
                                    |> Multiple
                }
            , Cmd.none
            , Nothing
            )

        ClickedRemoveProfile profileToRemove ->
            ( Model
                { model
                    | selectedProfile =
                        case model.selectedProfile of
                            Single _ ->
                                Single Nothing

                            Multiple profiles ->
                                Multiple
                                    (List.filter
                                        (.profile >> (/=) profileToRemove)
                                        profiles
                                    )
                }
            , Cmd.none
            , Nothing
            )

        BlurredPicker ->
            ( Model model, Cmd.none, Just viewConfig.onBlur )


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        GotSelectMsg _ ->
            [ "GotSelectMsg" ]

        SelectedUser _ ->
            [ "SelectedUser" ]

        GotProfileSummaryMsg _ subMsg ->
            "GotProfileSummaryMsg" :: Profile.Summary.msgToString subMsg

        ClickedRemoveProfile _ ->
            [ "ClickedRemoveProfile" ]

        BlurredPicker ->
            [ "BlurredPicker" ]



-- TRANSFORMING DIFFERENT MODELS


isEmpty : Model -> Bool
isEmpty (Model model) =
    case model.selectedProfile of
        Single maybeProfile ->
            Maybe.Extra.isNothing maybeProfile

        Multiple profiles ->
            List.isEmpty profiles


getSingleProfile : SinglePickerModel -> Maybe Profile.Minimal
getSingleProfile (SinglePickerModel model) =
    Maybe.map .profile model.selectedProfile


getMultipleProfiles : MultiplePickerModel -> List Profile.Minimal
getMultipleProfiles (MultiplePickerModel model) =
    List.map .profile model.selectedProfiles


getCurrentQuery : Model -> String
getCurrentQuery (Model model) =
    View.Select.queryFromState model.selectState
        |> Maybe.withDefault ""


fromSinglePicker : SinglePickerModel -> Model
fromSinglePicker (SinglePickerModel model) =
    Model
        { selectState = model.selectState
        , id = model.id
        , selectedProfile = Single model.selectedProfile
        }


toSinglePicker : Model -> SinglePickerModel
toSinglePicker (Model model) =
    case model.selectedProfile of
        Single maybeProfile ->
            SinglePickerModel
                { selectState = model.selectState
                , id = model.id
                , selectedProfile = maybeProfile
                }

        Multiple selectedProfiles ->
            SinglePickerModel
                { selectState = model.selectState
                , id = model.id
                , selectedProfile = List.head selectedProfiles
                }


fromMultiplePicker : MultiplePickerModel -> Model
fromMultiplePicker (MultiplePickerModel model) =
    Model
        { selectState = model.selectState
        , id = model.id
        , selectedProfile = Multiple model.selectedProfiles
        }


toMultiplePicker : Model -> MultiplePickerModel
toMultiplePicker (Model model) =
    case model.selectedProfile of
        Single maybeProfile ->
            MultiplePickerModel
                { selectState = model.selectState
                , id = model.id
                , selectedProfiles =
                    maybeProfile
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                }

        Multiple profiles ->
            MultiplePickerModel
                { selectState = model.selectState
                , id = model.id
                , selectedProfiles = profiles
                }
