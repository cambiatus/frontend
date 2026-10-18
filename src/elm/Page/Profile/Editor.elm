module Page.Profile.Editor exposing
    ( Model
    , Msg
    , init
    , msgToString
    , receiveBroadcast
    , update
    , view
    )

import Avatar
import Browser.Dom
import Cambiatus.Mutation
import Dict
import Eos
import Form
import Form.File
import Form.LocalityPicker as LocalityPicker
import Form.RichText
import Form.Select
import Form.Text
import Graphql.Http
import Html exposing (Html, button, div, h2, p, span, text)
import Html.Attributes exposing (attribute, class, disabled, id, maxlength, type_)
import Html.Events exposing (onClick)
import Icons
import Log
import Markdown exposing (Markdown)
import Page
import Profile
import Profile.Location as Location exposing (Address, Country(..), Locality, Place)
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External)
import Session.Shared exposing (Translators)
import Task
import UpdateResult as UR
import Utils
import View.Components
import View.Feedback as Feedback



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( initModel
    , Cmd.batch
        [ LoggedIn.maybeInitWith CompletedLoadProfile .profile loggedIn
        , Utils.spawnMessage RequestedEditorData
        ]
    )



-- MODEL


type alias Model =
    { profile : Maybe Profile.Model
    , editorData : RemoteData (Graphql.Http.Error (Maybe Location.EditorData)) Location.EditorData
    , form : FormStatus
    }


type FormStatus
    = Loading
    | Loaded LoadedForm


type alias LoadedForm =
    { form : Form.Model FormInput

    -- `Nothing` while the `location_picker` release hasn't reached the member:
    -- they keep the text field and the legacy save
    , location : Maybe LocationState
    }


releaseKey : String
releaseKey =
    "location_picker"


type alias LocationState =
    { original : { place : Place, address : Address }
    , place : Place
    , address : Address
    , storedText : String
    , municipalityPicker : LocalityPicker.Model
    , bairroPicker : Maybe LocalityPicker.Model
    , typingBairro : Bool
    , cep : CepState
    , showErrors : Bool

    -- The backend said the address has no CEP to be filed under
    , serverWantsPostalCode : Bool
    , feedback : FeedbackState
    }


{-| What the CEP lookup knows. `CepFound` keeps the folded flag: while folded,
rua, bairro and cidade show as one line and only the número is left to type.
-}
type CepState
    = CepIdle
    | CepLoading String
    | CepFound { lookup : Location.PostalLookup, folded : Bool }
    | CepNotFound
    | CepUnavailable


type alias FeedbackState =
    { isOpen : Bool
    , kind : String
    , body : String
    , status : FeedbackStatus
    }


type FeedbackStatus
    = Writing
    | Sending
    | Sent
    | FailedToSend


type alias FormInput =
    { avatar : Form.File.SingleModel
    , fullName : String
    , email : String
    , bio : Form.RichText.Model
    , location : String
    , interest : String
    , interests : List String
    }


type alias FormOutput =
    { avatar : Maybe String
    , fullName : String
    , email : String
    , bio : Markdown
    , location : String
    , interests : List String
    }


createForm : Translators -> { hasKyc : Bool, hasLocationPicker : Bool } -> Form.Form msg FormInput FormOutput
createForm ({ t } as translators) { hasKyc, hasLocationPicker } =
    Form.succeed FormOutput
        |> Form.with
            (Form.File.init { id = "avatar-input" }
                |> Form.File.withContainerAttributes [ class "grid place-items-center" ]
                |> Form.File.withImageClass "object-cover rounded-full mx-auto w-20 h-20"
                |> Form.File.withEntryContainerAttributes (\_ -> [ class "mx-auto rounded-full w-20 h-20" ])
                |> Form.File.withEditIconOverlay
                |> Form.File.withAddImagesView
                    [ span [ class "bg-orange-300 rounded-full p-4 w-20 h-20" ]
                        [ Icons.camera "text-white"
                        ]
                    ]
                |> Form.File.withImageCropperAttributes [ class "rounded-full" ]
                |> Form.file
                    { parser = Ok
                    , translators = translators
                    , value = .avatar
                    , update = \avatar input -> { input | avatar = avatar }
                    , externalError = always Nothing
                    }
                |> Form.optional
            )
        |> Form.with
            (Form.Text.init { label = t "profile.edit.labels.name", id = "name-input" }
                |> Form.Text.withDisabled hasKyc
                |> Form.textField
                    { parser = Ok
                    , value = .fullName
                    , update = \name input -> { input | fullName = name }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init { label = t "profile.edit.labels.email", id = "email-input" }
                |> Form.textField
                    { parser = Ok
                    , value = .email
                    , update = \email input -> { input | email = email }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.RichText.init { label = t "profile.edit.labels.bio" }
                |> Form.RichText.withEditorContainerAttrs [ class "mb-10" ]
                |> Form.richText
                    { parser = Ok
                    , value = .bio
                    , update = \bio input -> { input | bio = bio }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (if hasLocationPicker then
                -- The picker below the form owns the place
                Form.succeed ""

             else
                Form.Text.init { label = t "profile.edit.labels.localization", id = "location-input" }
                    |> Form.textField
                        { parser = Ok
                        , value = .location
                        , update = \location input -> { input | location = location }
                        , externalError = always Nothing
                        }
            )
        |> Form.with (interestsForm translators)


interestsForm : Translators -> Form.Form msg FormInput (List String)
interestsForm { t } =
    let
        viewInterest interest =
            div [ class "bg-green px-3 h-8 rounded-sm text-sm flex text-white" ]
                [ span [ class "m-auto mr-3 uppercase" ] [ text interest ]
                , button
                    [ type_ "button"
                    , onClick
                        (\values ->
                            { values | interests = List.filter (\x -> x /= interest) values.interests }
                        )
                    ]
                    [ Icons.close "w-4 h-4 fill-current" ]
                ]
    in
    Form.introspect (\values -> Form.succeed values.interests)
        |> Form.withNoOutput
            (Form.succeed always
                |> Form.withGroup
                    [ class "flex" ]
                    (Form.Text.init { label = t "profile.edit.labels.interests", id = "interest-input" }
                        |> Form.Text.withContainerAttrs [ class "w-full !mb-4" ]
                        |> Form.textField
                            { parser = Ok
                            , value = .interest
                            , update = \interest input -> { input | interest = interest }
                            , externalError = always Nothing
                            }
                    )
                    (Form.arbitrary
                        (button
                            [ type_ "button"
                            , class "button button-secondary px-4 h-12 ml-4 mb-4 mt-auto max-w-min"
                            , onClick
                                (\values ->
                                    if String.isEmpty values.interest || List.member values.interest values.interests then
                                        values

                                    else
                                        { values
                                            | interest = ""
                                            , interests = values.interest :: values.interests
                                        }
                                )
                            ]
                            [ text <| t "menu.add"
                            ]
                        )
                    )
            )
        |> Form.withNoOutput
            (Form.introspect
                (\values ->
                    Form.arbitrary
                        (div [ class "flex flex-wrap mb-10 gap-x-4 gap-y-2" ]
                            (List.map viewInterest values.interests)
                        )
                )
            )


initModel : Model
initModel =
    { profile = Nothing
    , editorData = RemoteData.NotAsked
    , form = Loading
    }



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        title =
            t "profile.edit.title"

        content =
            case loggedIn.profile of
                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Failure e ->
                    Page.fullPageGraphQLError (t "profile.title") e

                RemoteData.Success profile ->
                    case ( model.editorData, model.form ) of
                        ( RemoteData.Failure e, _ ) ->
                            Page.fullPageGraphQLError (t "profile.title") e

                        ( _, Loading ) ->
                            Page.fullPageLoading loggedIn.shared

                        ( _, Loaded loaded ) ->
                            view_ loggedIn loaded profile
    in
    { title = title
    , content = content
    }


view_ : LoggedIn.Model -> LoadedForm -> Profile.Model -> Html Msg
view_ loggedIn { form, location } profile =
    let
        { t } =
            loggedIn.shared.translators

        translators =
            loggedIn.shared.translators

        title =
            t "menu.edit" ++ " " ++ ("menu.profile" |> t |> String.toLower)
    in
    div [ class "bg-white" ]
        [ Page.viewHeader loggedIn title
        , Form.view [ class "container mx-auto p-4" ]
            translators
            (\submitButton ->
                [ case location of
                    Just locationState ->
                        div []
                            [ viewPlaceSection translators locationState
                            , viewAddressSection translators locationState
                            ]

                    Nothing ->
                        text ""
                , submitButton [ class "button button-primary w-full" ]
                    [ text <| t "profile.edit.submit" ]
                ]
            )
            (createForm translators
                { hasKyc = List.any .hasKyc profile.communities
                , hasLocationPicker = location /= Nothing
                }
            )
            form
            { toMsg = GotFormMsg
            , onSubmit = ClickedSave
            }
        ]


tl : Translators -> String -> String
tl { t } key =
    t ("profile.edit.location." ++ key)


viewPlaceSection : Translators -> LocationState -> Html Msg
viewPlaceSection translators state =
    let
        { place } =
            state
    in
    div [ class "mb-10" ]
        [ div [ class "flex flex-wrap items-center gap-x-3 gap-y-1 mb-4" ]
            [ h2 [ class "font-bold text-lg" ] [ text (tl translators "title") ]
            , viewBeta translators
            ]
        , viewFeedback translators state.feedback
        , Form.Select.init
            { label = tl translators "country"
            , id = "location-country-input"
            , optionToString = countryToString
            }
            |> Form.Select.withContainerAttrs [ class "!mb-4" ]
            |> Form.Select.withOption Brazil (tl translators "country_brazil")
            |> Form.Select.withOption CostaRica (tl translators "country_costa_rica")
            |> Form.Select.withOption Other (tl translators "country_other")
            |> (\options ->
                    Form.Select.view options
                        { onSelect = ChangedCountry
                        , onBlur = NoOp
                        , value = place.country
                        , error = text ""
                        , hasError = False
                        , isRequired = False
                        }
               )
        , case place.country of
            Other ->
                viewTextInput
                    { id = "location-other-input"
                    , label = tl translators "other_label"
                    , placeholder = tl translators "other_placeholder"
                    , value = place.otherText
                    , onInput = ChangedOtherText
                    , error = Nothing
                    , attrs = [ maxlength 120 ]
                    , isTextarea = False
                    , translators = translators
                    }

            _ ->
                div []
                    [ viewMunicipality translators state
                    , if place.municipality /= Nothing then
                        viewBairro translators state

                      else
                        text ""
                    ]
        , viewPreview translators place
        ]


countryToString : Country -> String
countryToString country =
    case country of
        Brazil ->
            "BR"

        CostaRica ->
            "CR"

        Other ->
            "OTHER"


viewBeta : Translators -> Html Msg
viewBeta translators =
    span [ class "inline-flex items-center rounded-full bg-gray-200 text-gray-333 text-sm font-bold pl-3" ]
        [ text (tl translators "beta")
        , button
            [ type_ "button"
            , class "ml-2 px-3 h-8 rounded-full hover:bg-gray-300 focus-ring"
            , onClick ToggledFeedback
            ]
            [ text (tl translators "send_feedback") ]
        ]


viewFeedback : Translators -> FeedbackState -> Html Msg
viewFeedback translators feedback =
    case ( feedback.status, feedback.isOpen ) of
        ( Sent, _ ) ->
            p [ class "mb-4 text-gray-333" ] [ text (tl translators "feedback.sent") ]

        ( _, False ) ->
            text ""

        _ ->
            div [ class "mb-6 p-4 rounded bg-gray-100" ]
                [ Form.Select.init
                    { label = tl translators "feedback.kind"
                    , id = "location-feedback-kind"
                    , optionToString = identity
                    }
                    |> Form.Select.withContainerAttrs [ class "!mb-4" ]
                    |> Form.Select.withOption "problem" (tl translators "feedback.kind_problem")
                    |> Form.Select.withOption "idea" (tl translators "feedback.kind_idea")
                    |> Form.Select.withOption "blocker" (tl translators "feedback.kind_blocker")
                    |> (\options ->
                            Form.Select.view options
                                { onSelect = ChangedFeedbackKind
                                , onBlur = NoOp
                                , value = feedback.kind
                                , error = text ""
                                , hasError = False
                                , isRequired = False
                                }
                       )
                , viewTextInput
                    { id = "location-feedback-body"
                    , label = tl translators "feedback.body"
                    , placeholder = ""
                    , value = feedback.body
                    , onInput = ChangedFeedbackBody
                    , error = Nothing
                    , attrs = [ maxlength 2000 ]
                    , isTextarea = True
                    , translators = translators
                    }
                , if feedback.status == FailedToSend then
                    span [ class "form-error normal-case tracking-normal mb-2 block" ] [ text (tl translators "feedback.error") ]

                  else
                    text ""
                , div [ class "flex gap-4" ]
                    [ button
                        [ type_ "button"
                        , class "button button-primary"
                        , disabled (String.trim feedback.body == "" || feedback.status == Sending)
                        , onClick ClickedSendFeedback
                        ]
                        [ text (tl translators "feedback.send") ]
                    , button
                        [ type_ "button"
                        , class "button button-secondary"
                        , onClick ToggledFeedback
                        ]
                        [ text (tl translators "feedback.cancel") ]
                    ]
                ]


{-| What is picked is shown as a row with a button to pick again, so the
member sees one thing at a time
-}
viewChosen :
    { id : String, label : String, value : String, changeLabel : String, onChange : Msg }
    -> Html Msg
viewChosen config =
    div [ class "mb-4" ]
        [ View.Components.label [] { targetId = config.id, labelText = config.label }
        , div [ class "flex items-center justify-between gap-4 input" ]
            [ span [ id config.id, class "truncate" ] [ text config.value ]
            , button
                [ type_ "button"
                , class "text-orange-500 font-bold hover:underline shrink-0 focus-ring"
                , onClick config.onChange
                ]
                [ text config.changeLabel ]
            ]
        ]


viewMunicipality : Translators -> LocationState -> Html Msg
viewMunicipality translators state =
    let
        label =
            if state.place.country == CostaRica then
                tl translators "canton"

            else
                tl translators "city"
    in
    case state.place.municipality of
        Just municipality ->
            viewChosen
                { id = "location-municipality-chosen"
                , label = label
                , value = Location.municipalityRow state.place.country municipality
                , changeLabel = tl translators "change"
                , onChange = ClickedChangeMunicipality
                }

        Nothing ->
            LocalityPicker.view
                { label = label
                , placeholder =
                    if state.place.country == CostaRica then
                        tl translators "canton_placeholder"

                    else
                        tl translators "city_placeholder"
                , notFound = tl translators "not_found"
                , hasError = False
                }
                state.municipalityPicker
                |> Html.map GotMunicipalityPickerMsg


viewBairro : Translators -> LocationState -> Html Msg
viewBairro translators state =
    let
        isCostaRica =
            state.place.country == CostaRica

        label =
            if isCostaRica then
                tl translators "district"

            else
                tl translators "neighborhood"

        optionalLabel =
            if isCostaRica then
                tl translators "district_optional"

            else
                tl translators "neighborhood_optional"

        textError =
            if state.showErrors then
                case Location.validateBairroText state.place.bairroText of
                    Ok _ ->
                        Nothing

                    Err Location.BairroTooLong ->
                        Just (tl translators "errors.bairro_too_long")

                    Err Location.BairroNotPlain ->
                        Just (tl translators "errors.bairro_not_plain")

            else
                Nothing

        textInput =
            viewTextInput
                { id = "location-bairro-text-input"
                , label = optionalLabel
                , placeholder = tl translators "bairro_text_placeholder"
                , value = state.place.bairroText
                , onInput = ChangedBairroText
                , error = textError
                , attrs = []
                , isTextarea = False
                , translators = translators
                }

        linkButton labelKey msg =
            button
                [ type_ "button"
                , class "text-orange-500 font-bold hover:underline mb-4 -mt-2 block focus-ring"
                , onClick msg
                ]
                [ text (tl translators labelKey) ]
    in
    case ( state.place.bairro, state.bairroPicker ) of
        ( Just bairro, _ ) ->
            viewChosen
                { id = "location-bairro-chosen"
                , label = label
                , value = bairro.name
                , changeLabel = tl translators "change"
                , onChange = ClickedChangeBairro
                }

        ( Nothing, Nothing ) ->
            textInput

        ( Nothing, Just picker ) ->
            case LocalityPicker.availability picker of
                LocalityPicker.Unknown ->
                    p [ class "mb-4 text-gray-700" ] [ text (tl translators "loading") ]

                LocalityPicker.Unavailable ->
                    textInput

                LocalityPicker.Available ->
                    if state.typingBairro then
                        div []
                            [ textInput
                            , linkButton "back_to_list" ClickedBackToList
                            ]

                    else
                        div []
                            [ LocalityPicker.view
                                { label = label
                                , placeholder = tl translators "bairro_placeholder"
                                , notFound = tl translators "not_found"
                                , hasError = False
                                }
                                picker
                                |> Html.map GotBairroPickerMsg
                            , linkButton
                                (if isCostaRica then
                                    "district_not_listed"

                                 else
                                    "neighborhood_not_listed"
                                )
                                ClickedBairroNotListed
                            ]


viewPreview : Translators -> Place -> Html Msg
viewPreview translators place =
    case Location.previewLabel place of
        Just label ->
            div [ class "flex flex-wrap items-center justify-between gap-x-4 gap-y-2 p-4 rounded bg-gray-100" ]
                [ p [ id "location-preview" ]
                    [ text (tl translators "preview_prefix" ++ " ")
                    , span [ class "font-bold" ] [ text label ]
                    ]
                , button
                    [ type_ "button"
                    , class "text-orange-500 font-bold hover:underline focus-ring"
                    , onClick ClickedClearPlace
                    ]
                    [ text (tl translators "clear") ]
                ]

        Nothing ->
            p [ id "location-preview", class "text-gray-700" ] [ text (tl translators "preview_empty") ]


viewAddressSection : Translators -> LocationState -> Html Msg
viewAddressSection translators state =
    let
        isPicked =
            Location.isPicked state.place

        isCostaRica =
            state.place.country == CostaRica

        -- A place that is only text ("Outro país") has a Brazilian address, filed
        -- under its own CEP
        hasOwnAddress =
            isPicked || state.place.country == Other

        errors =
            if state.showErrors && hasOwnAddress then
                case Location.validateAddress state.place.country state.address of
                    Ok _ ->
                        []

                    Err found ->
                        found

            else
                []

        errorFor error message =
            if List.member error errors then
                Just (tl translators message)

            else
                Nothing

        showPreviousText =
            Location.isAddressEmpty state.original.address
                && Location.looksLikeAddress state.storedText
    in
    div [ class "mb-10" ]
        [ h2 [ class "font-bold text-lg mb-1" ] [ text (tl translators "address.title") ]
        , p [ class "text-gray-700 mb-4" ] [ text (tl translators "address.private") ]
        , if state.place.country == Brazil || state.place.country == Other then
            viewCepField translators state errorFor

          else
            text ""
        , if not hasOwnAddress then
            p [ id "location-address-needs-place", class "p-4 rounded bg-gray-100 text-gray-700" ]
                [ text
                    (if isCostaRica then
                        tl translators "address.needs_canton"

                     else
                        tl translators "address.needs_city_or_cep"
                    )
                ]

          else
            div []
                [ if showPreviousText then
                    p [ id "location-previous-text", class "mb-4 p-4 rounded bg-gray-100 text-gray-333 break-words" ]
                        [ text (tl translators "address.previous_text" ++ " ")
                        , span [ class "font-bold" ] [ text state.storedText ]
                        ]

                  else
                    text ""
                , if isCostaRica then
                    div []
                        [ viewTextInput
                            { id = "address-street-input"
                            , label = tl translators "address.directions"
                            , placeholder = ""
                            , value = state.address.street
                            , onInput = ChangedAddress Street
                            , error = errorFor Location.StreetRequired "errors.directions_required"
                            , attrs = [ attribute "autocomplete" "street-address" ]
                            , isTextarea = False
                            , translators = translators
                            }
                        , viewTextInput
                            { id = "address-postal-code-input"
                            , label = tl translators "address.postal_code_cr"
                            , placeholder = ""
                            , value = state.address.postalCode
                            , onInput = ChangedAddress PostalCode
                            , error = errorFor Location.PostalCodeInvalid "errors.postal_code_invalid"
                            , attrs = [ attribute "inputmode" "numeric", attribute "autocomplete" "postal-code", maxlength 5 ]
                            , isTextarea = False
                            , translators = translators
                            }
                        ]

                  else
                    viewBrazilAddressFields translators state errorFor
                ]
        ]


viewCepField : Translators -> LocationState -> (Location.AddressError -> String -> Maybe String) -> Html Msg
viewCepField translators state errorFor =
    div []
        [ viewTextInput
            { id = "address-postal-code-input"
            , label =
                if state.place.country == Other then
                    tl translators "address.cep_required_label"

                else
                    tl translators "address.cep"
            , placeholder = "00000-000"
            , value = state.address.postalCode
            , onInput = ChangedAddress PostalCode
            , error =
                case errorFor Location.PostalCodeRequired "errors.cep_required" of
                    Just required ->
                        Just required

                    Nothing ->
                        if state.serverWantsPostalCode && String.isEmpty (String.trim state.address.postalCode) then
                            Just (tl translators "errors.cep_required")

                        else
                            errorFor Location.PostalCodeInvalid "errors.cep_invalid"
            , attrs = [ attribute "inputmode" "numeric", attribute "autocomplete" "postal-code", maxlength 9 ]
            , isTextarea = False
            , translators = translators
            }
        , div [ id "cep-status", attribute "role" "status", attribute "aria-live" "polite", class "-mt-2 mb-4" ]
            [ case state.cep of
                CepIdle ->
                    p [ class "text-gray-700 text-sm" ]
                        [ text
                            (if state.place.country == Other then
                                tl translators "address.cep_hint_other"

                             else
                                tl translators "address.cep_hint"
                            )
                        ]

                CepLoading _ ->
                    p [ class "text-gray-700 text-sm" ] [ text (tl translators "address.cep_loading") ]

                CepFound { lookup, folded } ->
                    div
                        [ id "cep-found"
                        , class "flex flex-wrap items-center justify-between gap-x-4 gap-y-1 p-4 rounded bg-green/20 text-gray-333"
                        ]
                        [ span [ class "font-bold break-words" ]
                            [ text
                                (Location.foundLine
                                    { street = state.address.street
                                    , place = state.place
                                    , stateCode = lookup.stateCode
                                    }
                                )
                            ]
                        , if folded then
                            button
                                [ type_ "button"
                                , id "cep-edit-address"
                                , class "text-orange-500 font-bold hover:underline focus-ring"
                                , onClick ClickedEditAddress
                                ]
                                [ text (tl translators "address.edit_address") ]

                          else
                            text ""
                        ]

                CepNotFound ->
                    p [ id "cep-not-found", class "p-4 rounded bg-gray-100 text-gray-700" ]
                        [ text (tl translators "address.cep_not_found") ]

                CepUnavailable ->
                    p [ id "cep-unavailable", class "p-4 rounded bg-gray-100 text-gray-700" ]
                        [ text (tl translators "address.cep_unavailable") ]
            ]
        ]


viewBrazilAddressFields : Translators -> LocationState -> (Location.AddressError -> String -> Maybe String) -> Html Msg
viewBrazilAddressFields translators state errorFor =
    let
        streetInput =
            viewTextInput
                { id = "address-street-input"
                , label = tl translators "address.street"
                , placeholder = ""
                , value = state.address.street
                , onInput = ChangedAddress Street
                , error = errorFor Location.StreetRequired "errors.street_required"
                , attrs = [ attribute "autocomplete" "address-line1" ]
                , isTextarea = False
                , translators = translators
                }

        numberAndComplement =
            div [ class "grid grid-cols-1 sm:grid-cols-2 sm:gap-x-4" ]
                [ viewTextInput
                    { id = "address-number-input"
                    , label = tl translators "address.number"
                    , placeholder = ""
                    , value = state.address.number
                    , onInput = ChangedAddress Number
                    , error = errorFor Location.NumberRequired "errors.number_required"
                    , attrs = []
                    , isTextarea = False
                    , translators = translators
                    }
                , viewTextInput
                    { id = "address-complement-input"
                    , label = tl translators "address.complement"
                    , placeholder = ""
                    , value = state.address.complement
                    , onInput = ChangedAddress Complement
                    , error = Nothing
                    , attrs = [ attribute "autocomplete" "address-line2" ]
                    , isTextarea = False
                    , translators = translators
                    }
                ]
    in
    case state.cep of
        -- The street came with the CEP: it folds into the found line. A CEP that
        -- covers a whole town has no street, so its field stays.
        CepFound { lookup, folded } ->
            if folded && lookup.street /= Nothing then
                numberAndComplement

            else
                div [] [ streetInput, numberAndComplement ]

        _ ->
            div [] [ streetInput, numberAndComplement ]


viewTextInput :
    { id : String
    , label : String
    , placeholder : String
    , value : String
    , onInput : String -> Msg
    , error : Maybe String
    , attrs : List (Html.Attribute Msg)
    , isTextarea : Bool
    , translators : Translators
    }
    -> Html Msg
viewTextInput config =
    Form.Text.init { label = config.label, id = config.id }
        |> Form.Text.withContainerAttrs [ class "!mb-4" ]
        |> Form.Text.withPlaceholder config.placeholder
        |> Form.Text.withExtraAttrs config.attrs
        |> (if config.isTextarea then
                Form.Text.withInputElement (Form.Text.TextareaInput { submitOnEnter = False })

            else
                identity
           )
        |> (\options ->
                Form.Text.view options
                    { onChange = config.onInput
                    , onBlur = NoOp
                    , value = config.value
                    , error =
                        case config.error of
                            Just error ->
                                span [ class "form-error normal-case tracking-normal" ] [ text error ]

                            Nothing ->
                                text ""
                    , hasError = config.error /= Nothing
                    , translators = config.translators
                    , isRequired = False
                    }
           )



-- UPDATE


type Msg
    = NoOp
    | CompletedLoadProfile Profile.Model
    | RequestedEditorData
    | GotEditorData (RemoteData (Graphql.Http.Error (Maybe Location.EditorData)) (Maybe Location.EditorData))
    | ClickedSave FormOutput
    | GotSaveResult (RemoteData (Graphql.Http.Error (Maybe Profile.Model)) (Maybe Profile.Model))
    | GotFormMsg (Form.Msg FormInput)
    | ChangedCountry Country
    | GotMunicipalityPickerMsg LocalityPicker.Msg
    | GotBairroPickerMsg LocalityPicker.Msg
    | ClickedChangeMunicipality
    | ClickedChangeBairro
    | ClickedBairroNotListed
    | ClickedBackToList
    | ClickedClearPlace
    | ChangedBairroText String
    | ChangedOtherText String
    | ChangedAddress AddressField String
    | GotPostalLookup String (RemoteData (Graphql.Http.Error (Maybe Location.PostalLookup)) (Maybe Location.PostalLookup))
    | ClickedEditAddress
    | ToggledFeedback
    | ChangedFeedbackKind String
    | ChangedFeedbackBody String
    | ClickedSendFeedback
    | GotFeedbackResult (RemoteData (Graphql.Http.Error (Maybe Bool)) (Maybe Bool))


type AddressField
    = Street
    | Number
    | Complement
    | PostalCode


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        { t } =
            loggedIn.shared.translators
    in
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadProfile profile ->
            { model | profile = Just profile }
                |> loadProfile profile
                |> maybeBuild loggedIn

        RequestedEditorData ->
            let
                symbol =
                    case loggedIn.selectedCommunity of
                        RemoteData.Success community ->
                            community.symbol

                        _ ->
                            loggedIn.shared.selectedCommunity
                                |> Maybe.withDefault Eos.cambiatusSymbol
            in
            { model | editorData = RemoteData.Loading }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.query loggedIn
                        (Location.editorSelectionSet { account = loggedIn.accountName, symbol = symbol })
                        GotEditorData
                    )

        GotEditorData result ->
            case result of
                RemoteData.Success maybeData ->
                    -- A missing user means there is nothing to pick from: keep the legacy field
                    { model
                        | editorData =
                            RemoteData.Success
                                (Maybe.withDefault
                                    { storedText = "", saved = Nothing, address = Nothing, releasedFeatures = [] }
                                    maybeData
                                )
                    }
                        |> maybeBuild loggedIn

                RemoteData.Failure e ->
                    { model | editorData = RemoteData.Failure e }
                        |> UR.init
                        |> UR.logGraphqlError msg
                            (Just loggedIn.accountName)
                            "Got an error when trying to load the place and address of the profile editor"
                            { moduleName = "Page.Profile.Editor", function = "update" }
                            []
                            e

                _ ->
                    UR.init model

        ClickedSave formOutput ->
            case ( loggedIn.profile, model.form ) of
                ( RemoteData.Success profile, Loaded loaded ) ->
                    let
                        newProfile =
                            { profile
                                | name = Just formOutput.fullName
                                , email = Just formOutput.email
                                , localization = Just formOutput.location
                                , interests = formOutput.interests
                                , bio = Just formOutput.bio
                                , avatar =
                                    formOutput.avatar
                                        |> Maybe.map Avatar.fromString
                                        |> Maybe.withDefault profile.avatar
                            }

                        save fields =
                            model
                                |> UR.init
                                |> UR.addExt (LoggedIn.UpdatedLoggedIn { loggedIn | profile = RemoteData.Loading })
                                |> UR.addExt
                                    (LoggedIn.mutation loggedIn
                                        (Profile.mutationWith fields (Profile.profileToForm newProfile))
                                        GotSaveResult
                                    )
                    in
                    case loaded.location of
                        Nothing ->
                            save (Location.legacySaveFields formOutput.location)

                        Just location ->
                            if hasLocationErrors location then
                                setLocation { location | showErrors = True } model
                                    |> UR.init

                            else
                                save
                                    (Location.saveFields
                                        { original = location.original
                                        , current = { place = location.place, address = location.address }
                                        }
                                    )

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Tried saving profile, but current profile wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Profile.Editor", function = "update" }
                            []

        GotSaveResult (RemoteData.Success (Just profile)) ->
            model
                |> UR.init
                |> UR.addExt (LoggedIn.ProfileLoaded profile |> LoggedIn.ExternalBroadcast)
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success (t "profile.edit_success"))
                |> UR.addCmd (Route.pushUrl loggedIn.shared.navKey (Route.Profile loggedIn.accountName))
                |> UR.addBreadcrumb
                    { type_ = Log.DebugBreadcrumb
                    , category = msg
                    , message = "Successfully saved profile"
                    , data = Dict.empty
                    , level = Log.DebugLevel
                    }

        GotSaveResult (RemoteData.Success Nothing) ->
            model
                |> UR.init

        GotSaveResult (RemoteData.Failure e) ->
            if Location.needsPostalCode e then
                -- Not a failure of the profile: the member only has to type the CEP
                withLocation model
                    (\location ->
                        setLocation { location | serverWantsPostalCode = True, showErrors = True } model
                            |> UR.init
                            |> UR.addExt
                                (LoggedIn.UpdatedLoggedIn
                                    { loggedIn
                                        | profile =
                                            model.profile
                                                |> Maybe.map RemoteData.Success
                                                |> Maybe.withDefault loggedIn.profile
                                    }
                                )
                    )

            else
                model
                    |> UR.init
                    |> UR.addExt (LoggedIn.UpdatedLoggedIn { loggedIn | profile = RemoteData.Failure e })
                    |> UR.logGraphqlError msg
                        (Just loggedIn.accountName)
                        "Got an error when trying to save profile"
                        { moduleName = "Page.Profile.Editor", function = "update" }
                        []
                        e

        GotSaveResult _ ->
            UR.init model

        GotFormMsg subMsg ->
            case model.form of
                Loading ->
                    UR.init model

                Loaded loaded ->
                    Form.update loggedIn.shared subMsg loaded.form
                        |> UR.fromChild (\newForm -> { model | form = Loaded { loaded | form = newForm } })
                            GotFormMsg
                            LoggedIn.addFeedback
                            model

        ChangedCountry country ->
            updateLocation model (\location -> resetPlace country location)

        GotMunicipalityPickerMsg subMsg ->
            withLocation model
                (\location ->
                    let
                        result =
                            LocalityPicker.update (pickerConfig loggedIn.shared.translators) subMsg location.municipalityPicker

                        withPicker =
                            { location | municipalityPicker = result.model }

                        ( newLocation, extraRequest ) =
                            case result.selected of
                                Just municipality ->
                                    selectMunicipality municipality withPicker

                                Nothing ->
                                    ( withPicker, Nothing )
                    in
                    setLocation newLocation model
                        |> UR.init
                        |> UR.addCmd (Cmd.map GotMunicipalityPickerMsg result.cmd)
                        |> addPickerRequest loggedIn GotMunicipalityPickerMsg result.request
                        |> addPickerRequest loggedIn GotBairroPickerMsg extraRequest
                )

        GotBairroPickerMsg subMsg ->
            withLocation model
                (\location ->
                    case location.bairroPicker of
                        Nothing ->
                            UR.init model

                        Just picker ->
                            let
                                result =
                                    LocalityPicker.update (pickerConfig loggedIn.shared.translators) subMsg picker

                                withPicker =
                                    { location | bairroPicker = Just result.model }

                                newLocation =
                                    case result.selected of
                                        Just bairro ->
                                            mapPlace (\place -> { place | bairro = Just bairro, bairroText = "" }) withPicker

                                        Nothing ->
                                            withPicker
                            in
                            setLocation newLocation model
                                |> UR.init
                                |> UR.addCmd (Cmd.map GotBairroPickerMsg result.cmd)
                                |> addPickerRequest loggedIn GotBairroPickerMsg result.request
                )

        ClickedChangeMunicipality ->
            updateLocation model (\location -> resetPlace location.place.country location)

        ClickedChangeBairro ->
            updateLocation model
                (\location ->
                    { location | typingBairro = False }
                        |> mapPlace (\place -> { place | bairro = Nothing, bairroText = "" })
                )

        ClickedBairroNotListed ->
            updateLocation model (\location -> { location | typingBairro = True })

        ClickedBackToList ->
            updateLocation model
                (\location ->
                    { location | typingBairro = False }
                        |> mapPlace (\place -> { place | bairroText = "" })
                )

        ClickedClearPlace ->
            updateLocation model (\location -> resetPlace location.place.country location)

        ChangedBairroText text ->
            updateLocation model (mapPlace (\place -> { place | bairroText = text }))

        ChangedOtherText text ->
            updateLocation model (mapPlace (\place -> { place | otherText = text }))

        ChangedAddress field text ->
            withLocation model
                (\location ->
                    let
                        withText =
                            { location | address = setAddressField field text location.address }
                    in
                    case field of
                        PostalCode ->
                            changedPostalCode loggedIn withText model

                        _ ->
                            UR.init (setLocation withText model)
                )

        GotPostalLookup digits result ->
            withLocation model
                (\location ->
                    -- A late answer for a CEP the member has since changed is dropped
                    if location.cep /= CepLoading digits then
                        UR.init model

                    else
                        case result of
                            RemoteData.Success (Just lookup) ->
                                let
                                    folded =
                                        Location.applyLookup lookup { place = location.place, address = location.address }

                                    ( withMunicipality, request ) =
                                        case lookup.municipality of
                                            Just municipality ->
                                                selectMunicipality municipality location

                                            Nothing ->
                                                ( location, Nothing )

                                    newLocation =
                                        { withMunicipality
                                            | place = folded.place
                                            , address = folded.address
                                            , typingBairro = folded.place.bairro == Nothing && folded.place.bairroText /= ""
                                            , cep = CepFound { lookup = lookup, folded = lookup.municipality /= Nothing }
                                        }

                                    focusId =
                                        if lookup.street == Nothing then
                                            "address-street-input"

                                        else
                                            "address-number-input"
                                in
                                setLocation newLocation model
                                    |> UR.init
                                    |> addPickerRequest loggedIn GotBairroPickerMsg request
                                    |> UR.addCmd (Browser.Dom.focus focusId |> Task.attempt (\_ -> NoOp))

                            RemoteData.Success Nothing ->
                                UR.init (setLocation { location | cep = CepNotFound } model)

                            _ ->
                                UR.init (setLocation { location | cep = CepUnavailable } model)
                )

        ClickedEditAddress ->
            updateLocation model
                (\location ->
                    case location.cep of
                        CepFound found ->
                            { location | cep = CepFound { found | folded = False } }

                        _ ->
                            location
                )

        ToggledFeedback ->
            updateLocation model (mapFeedback (\feedback -> { feedback | isOpen = not feedback.isOpen, status = Writing }))

        ChangedFeedbackKind kind ->
            updateLocation model (mapFeedback (\feedback -> { feedback | kind = kind }))

        ChangedFeedbackBody body ->
            updateLocation model (mapFeedback (\feedback -> { feedback | body = body }))

        ClickedSendFeedback ->
            withLocation model
                (\location ->
                    let
                        feedback =
                            location.feedback
                    in
                    if String.trim feedback.body == "" || feedback.status == Sending then
                        UR.init model

                    else
                        setLocation { location | feedback = { feedback | status = Sending } } model
                            |> UR.init
                            |> UR.addExt
                                (LoggedIn.mutation loggedIn
                                    (Cambiatus.Mutation.sendReleaseFeedback
                                        { input =
                                            { key = releaseKey
                                            , kind = feedback.kind
                                            , body = String.trim feedback.body
                                            , path = "/profile/edit"
                                            }
                                        }
                                    )
                                    GotFeedbackResult
                                )
                )

        GotFeedbackResult result ->
            updateLocation model
                (mapFeedback
                    (\feedback ->
                        case result of
                            RemoteData.Success (Just True) ->
                                { feedback | status = Sent, isOpen = False, body = "" }

                            RemoteData.Success _ ->
                                { feedback | status = FailedToSend }

                            RemoteData.Failure _ ->
                                { feedback | status = FailedToSend }

                            _ ->
                                feedback
                    )
                )


{-| Rebuilds the form when the profile arrives again (the form is rebuilt after
a save, as before), keeping the member's place and address
-}
loadProfile : Profile.Model -> Model -> Model
loadProfile profile model =
    case model.form of
        Loaded loaded ->
            { model
                | form =
                    Loaded
                        { loaded | form = initForm profile (loaded.location /= Nothing) }
            }

        Loading ->
            model


initForm : Profile.Model -> Bool -> Form.Model FormInput
initForm profile hasLocationPicker =
    let
        nullable a =
            Maybe.withDefault "" a
    in
    { avatar =
        Form.File.initSingle
            { fileUrl = Avatar.toMaybeString profile.avatar
            , aspectRatio = Just 1
            }
    , fullName = nullable profile.name
    , email = nullable profile.email
    , bio = Form.RichText.initModel "bio-input" profile.bio
    , location =
        if hasLocationPicker then
            ""

        else
            nullable profile.localization
    , interests = profile.interests
    , interest = ""
    }
        |> Form.init


{-| Builds the form once the profile and the editor's own query have both arrived
-}
maybeBuild : LoggedIn.Model -> Model -> UpdateResult
maybeBuild loggedIn model =
    case ( model.form, model.profile, model.editorData ) of
        ( Loading, Just profile, RemoteData.Success data ) ->
            if List.member releaseKey data.releasedFeatures then
                let
                    ( location, request ) =
                        initLocation data
                in
                { model | form = Loaded { form = initForm profile True, location = Just location } }
                    |> UR.init
                    |> addPickerRequest loggedIn GotBairroPickerMsg request

            else
                { model | form = Loaded { form = initForm profile False, location = Nothing } }
                    |> UR.init

        _ ->
            UR.init model


initLocation : Location.EditorData -> ( LocationState, Maybe LocalityPicker.Request )
initLocation data =
    let
        place =
            Location.placeFromSaved { storedText = data.storedText, saved = data.saved }

        address =
            Maybe.withDefault Location.emptyAddress data.address

        ( bairroPicker, request ) =
            case ( place.country, place.municipality ) of
                ( Other, _ ) ->
                    ( Nothing, Nothing )

                ( _, Nothing ) ->
                    ( Nothing, Nothing )

                ( country, Just municipality ) ->
                    bairroPickerFor country municipality
    in
    ( { original = { place = place, address = address }
      , place = place
      , address = address
      , storedText = data.storedText
      , municipalityPicker = newMunicipalityPicker place.country
      , bairroPicker = bairroPicker
      , typingBairro = place.bairro == Nothing && Location.cleanText place.bairroText /= ""
      , cep = CepIdle
      , showErrors = False
      , serverWantsPostalCode = False
      , feedback = { isOpen = False, kind = "problem", body = "", status = Writing }
      }
    , request
    )


newMunicipalityPicker : Country -> LocalityPicker.Model
newMunicipalityPicker country =
    LocalityPicker.init { id = "location-municipality-input", kind = LocalityPicker.Municipalities country }
        |> Tuple.first


bairroPickerFor : Country -> Locality -> ( Maybe LocalityPicker.Model, Maybe LocalityPicker.Request )
bairroPickerFor country municipality =
    LocalityPicker.init
        { id = "location-bairro-input"
        , kind = LocalityPicker.Bairros { country = country, municipalityId = municipality.id }
        }
        |> Tuple.mapFirst Just


selectMunicipality : Locality -> LocationState -> ( LocationState, Maybe LocalityPicker.Request )
selectMunicipality municipality location =
    let
        ( bairroPicker, request ) =
            bairroPickerFor location.place.country municipality
    in
    ( { location | bairroPicker = bairroPicker, typingBairro = False }
        |> mapPlace (\place -> { place | municipality = Just municipality, bairro = Nothing, bairroText = "" })
    , request
    )


{-| Switching country (or clearing) clears the place, never the private address
-}
resetPlace : Country -> LocationState -> LocationState
resetPlace country location =
    { location
        | place = { municipality = Nothing, bairro = Nothing, bairroText = "", otherText = "", country = country }
        , municipalityPicker = newMunicipalityPicker country
        , bairroPicker = Nothing
        , typingBairro = False
        , cep = CepIdle
    }


pickerConfig : Translators -> LocalityPicker.ViewConfig
pickerConfig translators =
    { label = ""
    , placeholder = ""
    , notFound = tl translators "not_found"
    , hasError = False
    }


addPickerRequest :
    LoggedIn.Model
    -> (LocalityPicker.Msg -> Msg)
    -> Maybe LocalityPicker.Request
    -> UpdateResult
    -> UpdateResult
addPickerRequest loggedIn toMsg maybeRequest ur =
    case maybeRequest of
        Nothing ->
            ur

        Just request ->
            UR.addExt
                (LoggedIn.query loggedIn request.selectionSet (request.toMsg >> toMsg))
                ur


withLocation : Model -> (LocationState -> UpdateResult) -> UpdateResult
withLocation model fn =
    case model.form of
        Loaded { location } ->
            case location of
                Just locationState ->
                    fn locationState

                Nothing ->
                    UR.init model

        Loading ->
            UR.init model


setLocation : LocationState -> Model -> Model
setLocation location model =
    case model.form of
        Loaded loaded ->
            { model | form = Loaded { loaded | location = Just location } }

        Loading ->
            model


updateLocation : Model -> (LocationState -> LocationState) -> UpdateResult
updateLocation model fn =
    withLocation model (\location -> UR.init (setLocation (fn location) model))


mapPlace : (Place -> Place) -> LocationState -> LocationState
mapPlace fn location =
    { location | place = fn location.place }


mapFeedback : (FeedbackState -> FeedbackState) -> LocationState -> LocationState
mapFeedback fn location =
    { location | feedback = fn location.feedback }


{-| A complete CEP of a Brazilian address asks the server what it knows; anything
else clears the lookup, so a stale "found" never lingers
-}
changedPostalCode : LoggedIn.Model -> LocationState -> Model -> UpdateResult
changedPostalCode loggedIn location model =
    case ( location.place.country, Location.normalizeCep location.address.postalCode ) of
        ( Brazil, Just digits ) ->
            if isLookupFor digits location.cep then
                UR.init (setLocation location model)

            else
                setLocation { location | cep = CepLoading digits } model
                    |> UR.init
                    |> UR.addExt
                        (LoggedIn.query loggedIn
                            (Location.postalLookupQuery digits)
                            (GotPostalLookup digits)
                        )

        _ ->
            UR.init (setLocation { location | cep = CepIdle } model)


isLookupFor : String -> CepState -> Bool
isLookupFor digits cep =
    case cep of
        CepLoading asked ->
            asked == digits

        CepFound { lookup } ->
            lookup.postalCode == digits

        _ ->
            False


setAddressField : AddressField -> String -> Address -> Address
setAddressField field text address =
    case field of
        Street ->
            { address | street = text }

        Number ->
            { address | number = text }

        Complement ->
            { address | complement = text }

        PostalCode ->
            { address | postalCode = text }


{-| Whether something the member typed in the place or the address would be refused
-}
hasLocationErrors : LocationState -> Bool
hasLocationErrors { place, address } =
    let
        bairroInvalid =
            case ( place.bairro, Location.validateBairroText place.bairroText ) of
                ( Nothing, Err _ ) ->
                    True

                _ ->
                    False

        addressInvalid =
            case Location.validateAddress place.country address of
                Ok _ ->
                    False

                Err _ ->
                    True
    in
    (Location.isPicked place && (bairroInvalid || addressInvalid))
        || (place.country == Other && addressInvalid)


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.ProfileLoaded profile ->
            Just (CompletedLoadProfile profile)

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        CompletedLoadProfile _ ->
            [ "CompletedLoadProfile" ]

        RequestedEditorData ->
            [ "RequestedEditorData" ]

        GotEditorData r ->
            [ "GotEditorData", UR.remoteDataToString r ]

        ClickedSave _ ->
            [ "ClickedSave" ]

        GotSaveResult r ->
            [ "GotSaveResult", UR.remoteDataToString r ]

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg

        ChangedCountry _ ->
            [ "ChangedCountry" ]

        GotMunicipalityPickerMsg subMsg ->
            "GotMunicipalityPickerMsg" :: LocalityPicker.msgToString subMsg

        GotBairroPickerMsg subMsg ->
            "GotBairroPickerMsg" :: LocalityPicker.msgToString subMsg

        ClickedChangeMunicipality ->
            [ "ClickedChangeMunicipality" ]

        ClickedChangeBairro ->
            [ "ClickedChangeBairro" ]

        ClickedBairroNotListed ->
            [ "ClickedBairroNotListed" ]

        ClickedBackToList ->
            [ "ClickedBackToList" ]

        ClickedClearPlace ->
            [ "ClickedClearPlace" ]

        ChangedBairroText _ ->
            [ "ChangedBairroText" ]

        ChangedOtherText _ ->
            [ "ChangedOtherText" ]

        ChangedAddress _ _ ->
            [ "ChangedAddress" ]

        GotPostalLookup _ r ->
            [ "GotPostalLookup", UR.remoteDataToString r ]

        ClickedEditAddress ->
            [ "ClickedEditAddress" ]

        ToggledFeedback ->
            [ "ToggledFeedback" ]

        ChangedFeedbackKind _ ->
            [ "ChangedFeedbackKind" ]

        ChangedFeedbackBody _ ->
            [ "ChangedFeedbackBody" ]

        ClickedSendFeedback ->
            [ "ClickedSendFeedback" ]

        GotFeedbackResult r ->
            [ "GotFeedbackResult", UR.remoteDataToString r ]
