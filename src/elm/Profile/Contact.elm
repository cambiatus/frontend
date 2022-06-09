module Profile.Contact exposing
    ( External(..)
    , Model
    , Msg
    , Normalized
    , circularIcon
    , circularLinkWithGrayBg
    , contactTypeTextColor
    , contactTypeToIcon
    , contactTypeToString
    , initMultiple
    , initSingle
    , selectionSet
    , toHref
    , unwrap
    , update
    , view
    )

import Browser.Dom
import Cambiatus.Enum.ContactType as ContactType exposing (ContactType(..))
import Cambiatus.Mutation
import Cambiatus.Object
import Cambiatus.Object.Contact
import Cambiatus.Object.User as User
import Eos.Account as Eos
import Form.Select
import Form.Text
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Attribute, Html, a, button, div, img, p, text)
import Html.Attributes exposing (class, classList, disabled, href, src, style, tabindex, type_)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (onClick, onSubmit)
import Icons
import Json.Decode
import List.Extra as LE
import Maybe.Extra
import PhoneNumber exposing (Country)
import PhoneNumber.Countries as Countries
import Regex exposing (Regex)
import RemoteData exposing (RemoteData)
import Task
import Translation exposing (Translators)
import UpdateResult as UR
import Url
import Validate
import View.Components
import View.Modal as Modal



-- MODEL


type alias Model =
    { state : UpdatedData
    , kind : Kind
    , contactTypeToDelete : Maybe ContactType
    }


initSingle : Model
initSingle =
    { state = RemoteData.NotAsked
    , kind = Single (initBasic defaultContactType)
    , contactTypeToDelete = Nothing
    }


initMultiple : List Normalized -> Model
initMultiple initialContacts =
    { state = RemoteData.NotAsked
    , kind =
        Multiple
            (List.map
                (\type_ ->
                    initialContacts
                        |> LE.find (\(Normalized { contactType }) -> type_ == contactType)
                        |> Maybe.map initBasicWith
                        |> Maybe.withDefault (initBasic type_)
                )
                ContactType.list
            )
    , contactTypeToDelete = Nothing
    }



-- TYPES


type Kind
    = Single Basic
    | Multiple (List Basic)


type alias Basic =
    { supportedCountry : SupportedCountry
    , contactType : ContactType
    , contact : String
    , label : Maybe String
    , errors : Maybe (List String)
    , showFlags : Bool
    , focusedFlag : Maybe SupportedCountry
    }


initBasic : ContactType -> Basic
initBasic contactType =
    { supportedCountry = defaultCountry
    , contactType = contactType
    , label = Nothing
    , contact = ""
    , errors = Nothing
    , showFlags = False
    , focusedFlag = Nothing
    }


initBasicWith : Normalized -> Basic
initBasicWith ((Normalized { contactType }) as normalized) =
    let
        initial =
            initBasic contactType

        ( maybeCountry, newPhoneContact ) =
            countryFromNormalized normalized

        newContact =
            case contactType of
                Phone ->
                    newPhoneContact

                Whatsapp ->
                    newPhoneContact

                Telegram ->
                    newPhoneContact
                        -- 13 == String.length "https://t.me/"
                        |> String.dropLeft 13

                Instagram ->
                    newPhoneContact
                        -- 22 == String.length "https://instagram.com/"
                        |> String.dropLeft 22

                Email ->
                    newPhoneContact

                Link ->
                    newPhoneContact
    in
    { initial
        | contact = newContact
        , supportedCountry =
            maybeCountry
                |> Maybe.withDefault defaultCountry
    }


countryFromNormalized : Normalized -> ( Maybe SupportedCountry, String )
countryFromNormalized (Normalized { contactType, contact }) =
    if usesPhone contactType then
        let
            countryCode =
                String.dropLeft 1 contact
                    |> String.toList
                    |> LE.takeWhile ((/=) ' ')
                    |> String.fromList

            supportedCountry =
                List.filter (.country >> .countryCode >> (==) countryCode) supportedCountries
                    |> List.head

            newContact =
                Maybe.map (.country >> .countryCode >> String.length >> (+) 1) supportedCountry
                    |> Maybe.withDefault 0
                    |> (\n -> String.dropLeft n contact |> String.trim)
        in
        ( supportedCountry, newContact )

    else
        ( Nothing, contact )


type alias Contact =
    { contactType : ContactType
    , contact : String
    , label : Maybe String
    }


defaultContactType : ContactType
defaultContactType =
    Whatsapp


type alias Profile =
    { account : Eos.Name
    , contacts : List Normalized
    }


{-| The contact string must be formatted in a specific way to be sent to the
backend, so we have this type to ensure the information is normalized
-}
type Normalized
    = Normalized Contact


type alias UpdatedData =
    RemoteData (Graphql.Http.Error (Maybe Profile)) (Maybe Profile)



-- UPDATE


type Msg
    = NoOp
    | SelectedCountry ContactType SupportedCountry
    | ClickedToggleContactFlags ContactType
    | EnteredContactText ContactType String
    | ClickedDeleteContact ContactType
    | ClosedDeleteModal
    | ConfirmedDeleteContact ContactType
    | EnteredContactOption ContactType
    | ClickedSubmit
    | CompletedUpdateContact UpdatedData
    | CompletedDeleteContact UpdatedData
    | PressedUpArrowOnFlagSelect ContactType
    | PressedDownArrowOnFlagSelect ContactType


type External
    = GotContacts String (List Normalized) Bool
    | GotContactsError String
    | GotMutationRequest (SelectionSet (Maybe Profile) RootMutation) (UpdatedData -> Msg)


type alias UpdateResult =
    UR.UpdateResult Model Msg External


update :
    Msg
    -> Model
    -> Translators
    -> List Normalized
    -> UpdateResult
update msg model translators profileContacts =
    let
        toggleFlags ({ showFlags } as contact) =
            { contact
                | showFlags = not showFlags
                , focusedFlag =
                    if showFlags then
                        Nothing

                    else
                        contact.focusedFlag
            }

        setCountry newCountry contact =
            { contact | supportedCountry = newCountry, errors = Nothing, focusedFlag = Nothing }

        setContact newContact contact =
            { contact | contact = newContact }

        setFocusedFlag supportedCountry contact =
            { contact | focusedFlag = Just supportedCountry }

        updateKind contactType updateFn =
            { model
                | kind =
                    case model.kind of
                        Single contact ->
                            updateFn contact
                                |> Single

                        Multiple contacts ->
                            updateIfContactType contactType updateFn contacts
                                |> Multiple
            }

        feedbackScope =
            case model.kind of
                Single _ ->
                    "single"

                Multiple _ ->
                    "multiple"

        currentFocusedFlag : ContactType -> SupportedCountry
        currentFocusedFlag contactType =
            case model.kind of
                Single contact ->
                    contact.focusedFlag
                        |> Maybe.withDefault defaultCountry

                Multiple contacts ->
                    contacts
                        |> List.filter (.contactType >> (==) contactType)
                        |> List.head
                        |> Maybe.andThen .focusedFlag
                        |> Maybe.withDefault defaultCountry

        currentCountry : ContactType -> SupportedCountry
        currentCountry contactType =
            case model.kind of
                Single contact ->
                    contact.supportedCountry

                Multiple contacts ->
                    contacts
                        |> List.filter (.contactType >> (==) contactType)
                        |> List.head
                        |> Maybe.map .supportedCountry
                        |> Maybe.withDefault defaultCountry
    in
    case msg of
        NoOp ->
            UR.init model

        SelectedCountry contactType country ->
            updateKind contactType (setCountry country)
                |> UR.init
                |> UR.addCmd
                    (flagSelectorId contactType
                        |> Browser.Dom.focus
                        |> Task.attempt (\_ -> NoOp)
                    )

        ClickedToggleContactFlags contactType ->
            updateKind contactType toggleFlags
                |> UR.init
                |> UR.addCmd
                    (flagSelectorId contactType
                        |> Browser.Dom.focus
                        |> Task.attempt (\_ -> NoOp)
                    )

        EnteredContactText contactType newContact ->
            updateKind contactType (setContact newContact)
                |> UR.init

        ClosedDeleteModal ->
            { model | contactTypeToDelete = Nothing }
                |> UR.init

        ClickedDeleteContact contactType ->
            { model | contactTypeToDelete = Just contactType }
                |> UR.init

        ConfirmedDeleteContact contactTypeToDelete ->
            let
                withoutContact =
                    updateKind contactTypeToDelete
                        (\basic ->
                            { basic
                                | errors = Nothing
                                , contact = ""
                            }
                        )

                newContacts =
                    List.filter
                        (\(Normalized { contactType }) -> contactType /= contactTypeToDelete)
                        profileContacts
            in
            { withoutContact
                | contactTypeToDelete = Nothing
                , state = RemoteData.Loading
            }
                |> UR.init
                |> UR.addExt (GotMutationRequest (mutation newContacts) CompletedDeleteContact)

        EnteredContactOption option ->
            case model.kind of
                Single contact ->
                    { model
                        | kind =
                            Single
                                { contact
                                    | contactType = option
                                    , errors = Nothing
                                    , contact = ""
                                }
                    }
                        |> UR.init

                Multiple _ ->
                    model
                        |> UR.init

        ClickedSubmit ->
            case submit translators model.kind of
                Err withError ->
                    { model | kind = withError }
                        |> UR.init

                Ok normalized ->
                    { model
                        | state = RemoteData.Loading
                        , kind = removeErrors model.kind
                    }
                        |> UR.init
                        |> UR.addExt (GotMutationRequest (mutation normalized) CompletedUpdateContact)

        CompletedUpdateContact result ->
            let
                feedbackString isSuccess =
                    String.join "."
                        [ "contact_form.feedback"
                        , feedbackScope
                        , if isSuccess then
                            "success"

                          else
                            "failure"
                        ]
                        |> translators.t

                addContactResponse =
                    case result of
                        RemoteData.Success (Just profile) ->
                            UR.addExt (GotContacts (feedbackString True) profile.contacts True)

                        RemoteData.Success Nothing ->
                            UR.addExt (GotContactsError (feedbackString False))

                        RemoteData.Failure _ ->
                            UR.addExt (GotContactsError (feedbackString False))

                        RemoteData.NotAsked ->
                            identity

                        RemoteData.Loading ->
                            identity
            in
            { model | state = result }
                |> UR.init
                |> addContactResponse

        CompletedDeleteContact result ->
            let
                feedbackString isSuccess =
                    String.join "."
                        [ "contact_form.feedback.delete"
                        , if isSuccess then
                            "success"

                          else
                            "failure"
                        ]
                        |> translators.t

                addContactResponse =
                    case result of
                        RemoteData.Success (Just profile) ->
                            UR.addExt (GotContacts (feedbackString True) profile.contacts False)

                        RemoteData.Success Nothing ->
                            UR.addExt (GotContactsError (feedbackString False))

                        RemoteData.Failure _ ->
                            UR.addExt (GotContactsError (feedbackString False))

                        RemoteData.NotAsked ->
                            identity

                        RemoteData.Loading ->
                            identity
            in
            { model | state = RemoteData.NotAsked }
                |> UR.init
                |> addContactResponse

        PressedUpArrowOnFlagSelect contactType ->
            let
                countries =
                    currentCountry contactType
                        |> countryOptions

                nextCountry : SupportedCountry
                nextCountry =
                    countries
                        |> LE.elemIndex (currentFocusedFlag contactType)
                        |> Maybe.map
                            (\nextIndex ->
                                if nextIndex <= 0 then
                                    List.length countries - 1

                                else
                                    nextIndex - 1
                            )
                        |> Maybe.andThen (\index -> LE.getAt index countries)
                        |> Maybe.withDefault defaultCountry
            in
            updateKind contactType (setFocusedFlag nextCountry)
                |> UR.init
                |> UR.addCmd
                    (flagId nextCountry contactType
                        |> Browser.Dom.focus
                        |> Task.attempt (\_ -> NoOp)
                    )

        PressedDownArrowOnFlagSelect contactType ->
            let
                countries =
                    currentCountry contactType
                        |> countryOptions

                nextCountry : SupportedCountry
                nextCountry =
                    countries
                        |> LE.elemIndex (currentFocusedFlag contactType)
                        |> Maybe.map
                            (\nextIndex ->
                                if nextIndex >= List.length countries - 1 then
                                    0

                                else
                                    nextIndex + 1
                            )
                        |> Maybe.andThen (\index -> LE.getAt index countries)
                        |> Maybe.withDefault defaultCountry
            in
            updateKind contactType (setFocusedFlag nextCountry)
                |> UR.init
                |> UR.addCmd
                    (flagId nextCountry contactType
                        |> Browser.Dom.focus
                        |> Task.attempt (\_ -> NoOp)
                    )


updateIfContactType : ContactType -> (Basic -> Basic) -> List Basic -> List Basic
updateIfContactType contactType fn contacts =
    LE.updateIf (.contactType >> (==) contactType) fn contacts


submit : Translators -> Kind -> Result Kind (List Normalized)
submit translators kind =
    case kind of
        Single contact ->
            submitSingle translators contact
                |> Result.mapError Single
                |> Result.map List.singleton

        Multiple contacts ->
            submitMultiple translators contacts
                |> Result.mapError Multiple


submitSingle : Translators -> Basic -> Result Basic Normalized
submitSingle translators basic =
    basic
        |> Validate.validate (validator basic.contactType translators)
        |> Result.mapError (\errors -> addErrors errors basic)
        |> Result.map (\valid -> normalize basic.supportedCountry valid)


submitMultiple : Translators -> List Basic -> Result (List Basic) (List Normalized)
submitMultiple translators basics =
    let
        ( withError, valid ) =
            List.foldr
                (\basic ( basicsAcc, normalizedsAcc ) ->
                    case submitSingle translators basic of
                        Err basicWithErrors ->
                            ( basicWithErrors :: basicsAcc, normalizedsAcc )

                        Ok normalized ->
                            ( { basic | errors = Nothing } :: basicsAcc
                            , normalized :: normalizedsAcc
                            )
                )
                ( [], [] )
                basics
    in
    if List.all (.contact >> String.isEmpty) basics then
        Ok []

    else if List.length valid > 0 then
        Ok valid

    else
        Err withError



-- VIEW


view : Translators -> Model -> Html Msg
view translators model =
    let
        submitText =
            (case model.kind of
                Single contact ->
                    case contact.contactType of
                        Instagram ->
                            "contact_form.username.submit"

                        Telegram ->
                            "contact_form.username.submit"

                        Link ->
                            "contact_form.link.submit"

                        Email ->
                            "contact_form.email.submit"

                        Whatsapp ->
                            "contact_form.phone.submit"

                        Phone ->
                            "contact_form.phone.submit"

                Multiple _ ->
                    "contact_form.submit_multiple"
            )
                |> translators.t
                |> text

        isDisabled =
            RemoteData.isLoading model.state || RemoteData.isSuccess model.state

        submitButton =
            button
                [ class "button w-full"
                , type_ "submit"
                , classList
                    [ ( "button-success", RemoteData.isSuccess model.state )
                    , ( "button-primary", not (RemoteData.isSuccess model.state) )
                    , ( "mt-7", isMultiple model.kind )
                    ]
                , disabled isDisabled
                ]
                [ case model.state of
                    RemoteData.Loading ->
                        View.Components.loadingLogoAnimatedFluid

                    _ ->
                        submitText
                ]
    in
    div
        [ classList
            [ ( "container mx-auto", isMultiple model.kind )
            ]
        ]
        [ Html.form
            [ class "mt-12"
            , classList
                [ ( "px-4", isMultiple model.kind )
                , ( "w-full md:w-5/6 mx-auto lg:w-2/3", not (isMultiple model.kind) )
                ]
            , onSubmit ClickedSubmit
            ]
            ((case model.kind of
                Single contact ->
                    [ viewContactTypeSelect translators contact.contactType
                    , viewInput translators contact
                    ]

                Multiple contacts ->
                    List.map (viewInputWithBackground translators) contacts
             )
                ++ [ submitButton
                   , if RemoteData.isFailure model.state then
                        text (translators.t "contact_form.error")

                     else
                        text ""
                   ]
            )
        , case model.contactTypeToDelete of
            Just contactType ->
                Modal.initWith
                    { closeMsg = ClosedDeleteModal
                    , isVisible = True
                    }
                    |> Modal.withHeader (translators.t "contact_form.delete.title")
                    |> Modal.withBody [ text (translators.t "contact_form.delete.body") ]
                    |> Modal.withFooter
                        [ button
                            [ class "modal-cancel"
                            , onClick ClosedDeleteModal
                            ]
                            [ text (translators.t "contact_form.delete.cancel") ]
                        , button
                            [ class "modal-accept"
                            , onClick (ConfirmedDeleteContact contactType)
                            ]
                            [ text (translators.t "contact_form.delete.accept") ]
                        ]
                    |> Modal.toHtml

            Nothing ->
                text ""
        ]


viewInputWithBackground : Translators -> Basic -> Html Msg
viewInputWithBackground translators basic =
    div [ class "bg-gray-100 p-4 pb-0 rounded mb-4" ]
        [ div [ class "font-menu font-semibold flex items-center mb-4 justify-between" ]
            [ div [ class "flex items-center" ]
                [ contactTypeToIcon "mr-2" False basic.contactType
                , text (contactTypeToString translators basic.contactType)
                ]
            , button
                [ onClick (ClickedDeleteContact basic.contactType)
                , type_ "button"
                ]
                [ Icons.trash "text-red" ]
            ]
        , viewInput translators basic
        ]


contactTypeToString : Translators -> ContactType -> String
contactTypeToString translators contactType =
    ContactType.toString contactType
        |> String.toLower
        |> (\asString -> "contact_form.types." ++ asString)
        |> translators.t


contactTypeToIcon : String -> Bool -> ContactType -> Html msg
contactTypeToIcon class_ isInverted contactType =
    case contactType of
        Phone ->
            if isInverted then
                Icons.phoneInverted class_

            else
                Icons.phone class_

        Instagram ->
            Icons.instagram class_

        Telegram ->
            if isInverted then
                Icons.telegramInverted class_

            else
                Icons.telegram class_

        Whatsapp ->
            if isInverted then
                Icons.whatsappInverted class_

            else
                Icons.whatsapp class_

        Email ->
            Icons.mail class_

        Link ->
            Icons.link class_


circularLinkWithGrayBg : Translators -> String -> Normalized -> Html msg
circularLinkWithGrayBg translators class_ (Normalized normalized) =
    let
        defaultClass =
            case normalized.contactType of
                Whatsapp ->
                    "fill-current text-green "

                Phone ->
                    "fill-current text-orange-500"

                _ ->
                    ""
    in
    a
        [ toHref (Normalized normalized)
        , class "w-10 h-10 flex-shrink-0 bg-gray-100 rounded-full flex items-center justify-center hover:opacity-70 focus-ring"
        , ariaLabel (ariaLabelForContactType translators normalized.contactType)
        ]
        [ contactTypeToIcon (defaultClass ++ class_) True normalized.contactType ]


ariaLabelForContactType : Translators -> ContactType -> String
ariaLabelForContactType { t } contactType =
    case contactType of
        Phone ->
            t "contact_form.reach_out.phone"

        Instagram ->
            t "contact_form.reach_out.instagram"

        Telegram ->
            t "contact_form.reach_out.telegram"

        Whatsapp ->
            t "contact_form.reach_out.whatsapp"

        Email ->
            t "contact_form.reach_out.email"

        Link ->
            t "contact_form.reach_out.link"


circularIcon : String -> Normalized -> Html msg
circularIcon class_ (Normalized normalized) =
    let
        ( bgColor, textColor ) =
            case normalized.contactType of
                Phone ->
                    ( "bg-orange-300", "fill-current text-white" )

                Instagram ->
                    ( "bg-instagram", "" )

                Telegram ->
                    ( "bg-telegram", "" )

                Whatsapp ->
                    ( "bg-whatsapp", "" )

                Email ->
                    ( "bg-gray-500", "fill-current text-orange-300" )

                Link ->
                    ( "bg-gray-500", "" )
    in
    case normalized.contactType of
        Telegram ->
            a
                [ toHref (Normalized normalized)
                , class "hover:opacity-80"
                ]
                [ contactTypeToIcon class_ False normalized.contactType
                ]

        _ ->
            a
                [ class
                    (String.join " "
                        [ "p-2 rounded-full flex items-center justify-center hover:opacity-80"
                        , textColor
                        , bgColor
                        , class_
                        ]
                    )
                , toHref (Normalized normalized)
                ]
                [ contactTypeToIcon "fill-current text-white object-contain" True normalized.contactType ]


toHref : Normalized -> Attribute msg
toHref (Normalized { contactType, contact }) =
    case contactType of
        Phone ->
            href ("tel:" ++ contact)

        Instagram ->
            href contact

        Telegram ->
            href contact

        Whatsapp ->
            href
                ("https://api.whatsapp.com/send?phone="
                    ++ (String.dropLeft 1 contact
                            |> String.filter Char.isAlphaNum
                       )
                )

        Email ->
            href ("mailto:" ++ contact)

        Link ->
            href contact


contactTypeTextColor : ContactType -> String
contactTypeTextColor contactType =
    case contactType of
        Phone ->
            "text-phone"

        Instagram ->
            "text-instagram"

        Telegram ->
            "text-telegram"

        Whatsapp ->
            "text-whatsapp"

        Email ->
            "text-orange-300"

        Link ->
            "text-orange-300"


viewInput : Translators -> Basic -> Html Msg
viewInput translators basic =
    div [ class "flex space-x-4" ]
        (if usesPhone basic.contactType then
            [ viewFlagsSelect translators basic, viewPhoneInput translators basic ]

         else
            [ viewTextInput translators basic ]
        )


viewContactTypeSelect : Translators -> ContactType -> Html Msg
viewContactTypeSelect ({ t } as translators) contactType =
    let
        makeOption contactType_ =
            { option = contactType_
            , label = contactTypeToString translators contactType_
            }
    in
    Form.Select.init
        { label = t "contact_form.contact_type"
        , id = "contact-type"
        , optionToString = ContactType.toString
        }
        |> Form.Select.withOptions (List.map makeOption ContactType.list)
        |> Form.Select.withContainerAttrs [ class "mb-10" ]
        |> (\options ->
                Form.Select.view options
                    { onSelect = EnteredContactOption
                    , onBlur = NoOp
                    , value = contactType
                    , error = text ""
                    , hasError = False
                    , isRequired = False
                    }
           )


flagId : SupportedCountry -> ContactType -> String
flagId supportedCountry contactType =
    "flag-" ++ supportedCountry.country.id ++ "-" ++ ContactType.toString contactType


flagSelectorId : ContactType -> String
flagSelectorId contactType =
    ContactType.toString contactType ++ "_select"


countryOptions : SupportedCountry -> List SupportedCountry
countryOptions supportedCountry =
    supportedCountry
        :: List.filter (\country -> country /= supportedCountry) supportedCountries


viewFlagsSelect : Translators -> Basic -> Html Msg
viewFlagsSelect { t } basic =
    let
        escListener =
            Html.Events.custom "keydown"
                (Json.Decode.field "key" Json.Decode.string
                    |> Json.Decode.andThen
                        (\key ->
                            if key == "Esc" || key == "Escape" then
                                Json.Decode.succeed
                                    { message = ClickedToggleContactFlags basic.contactType
                                    , stopPropagation = True
                                    , preventDefault = True
                                    }

                            else
                                Json.Decode.fail "Expecting Esc"
                        )
                )

        flag addId classes supportedCountry =
            button
                [ class ("w-full flex items-center space-x-2 text-menu rounded-sm focus:outline-none focus:ring focus:ring-offset-2 " ++ classes)
                , onClick (SelectedCountry basic.contactType supportedCountry)
                , type_ "button"
                , tabindex -1
                , escListener
                , if addId then
                    Html.Attributes.id (flagId supportedCountry basic.contactType)

                  else
                    class ""
                ]
                [ img
                    [ class "w-7"
                    , src supportedCountry.flagIcon
                    ]
                    []
                , p [ class "justify-self-center text-left", style "min-width" "4ch" ]
                    [ text ("+" ++ supportedCountry.country.countryCode) ]
                ]

        withPreventAll message =
            { message = message, preventDefault = True, stopPropagation = True }
    in
    div [ class "mb-10 flex-shrink-0" ]
        [ if basic.showFlags then
            button
                [ class "fixed top-0 left-0 w-full h-full cursor-default z-40"
                , onClick (ClickedToggleContactFlags basic.contactType)
                , type_ "button"
                , tabindex -1
                ]
                []

          else
            text ""
        , View.Components.label []
            { targetId = flagSelectorId basic.contactType
            , labelText = t "contact_form.country"
            }
        , button
            [ class "form-select relative"
            , classList [ ( "border-none mx-px", basic.showFlags ) ]
            , onClick (ClickedToggleContactFlags basic.contactType)
            , Html.Attributes.id (flagSelectorId basic.contactType)
            , type_ "button"
            , Html.Events.custom "keydown"
                (Json.Decode.field "key" Json.Decode.string
                    |> Json.Decode.andThen
                        (\key ->
                            case key of
                                "ArrowUp" ->
                                    PressedUpArrowOnFlagSelect basic.contactType
                                        |> withPreventAll
                                        |> Json.Decode.succeed

                                "ArrowDown" ->
                                    PressedDownArrowOnFlagSelect basic.contactType
                                        |> withPreventAll
                                        |> Json.Decode.succeed

                                "Esc" ->
                                    ClickedToggleContactFlags basic.contactType
                                        |> withPreventAll
                                        |> Json.Decode.succeed

                                "Escape" ->
                                    ClickedToggleContactFlags basic.contactType
                                        |> withPreventAll
                                        |> Json.Decode.succeed

                                "Tab" ->
                                    if basic.showFlags then
                                        Json.Decode.succeed
                                            { message = ClickedToggleContactFlags basic.contactType
                                            , preventDefault = False
                                            , stopPropagation = False
                                            }

                                    else
                                        Json.Decode.fail "Only listen to tab when dropdown is open"

                                _ ->
                                    Json.Decode.fail "Expected arrow up or arrow down"
                        )
                )
            ]
            [ flag False "" basic.supportedCountry
            , if basic.showFlags then
                div
                    [ class "absolute input -mx-px inset-x-0 top-0 space-y-4 z-50 h-44 overflow-auto"
                    , tabindex -1
                    ]
                    (countryOptions basic.supportedCountry
                        |> List.map (flag True "mt-1")
                    )

              else
                text ""
            ]
        ]


viewPhoneInput : Translators -> Basic -> Html Msg
viewPhoneInput ({ t, tr } as translators) basic =
    Form.Text.init
        { label = t "contact_form.phone.label"
        , id = ContactType.toString basic.contactType ++ "-input"
        }
        |> Form.Text.withPlaceholder
            (tr "contact_form.phone.placeholder"
                [ ( "example_number", basic.supportedCountry.phonePlaceholder ) ]
            )
        |> Form.Text.withMask (phoneMask basic)
        |> Form.Text.withType Form.Text.Telephone
        |> Form.Text.withAllowedChars Char.isDigit
        |> Form.Text.withContainerAttrs [ class "w-full" ]
        |> (\options ->
                Form.Text.view options
                    { onChange = EnteredContactText basic.contactType
                    , onBlur = NoOp
                    , value = basic.contact
                    , error =
                        case basic.errors of
                            Just (firstError :: _) ->
                                p [ class "form-error" ] [ text firstError ]

                            Just [] ->
                                text ""

                            Nothing ->
                                text ""
                    , hasError = Maybe.Extra.isJust basic.errors
                    , translators = translators
                    , isRequired = False
                    }
           )


phoneMask : Basic -> { mask : String, replace : Char }
phoneMask basic =
    { mask =
        basic.supportedCountry.phonePlaceholder
            |> String.map
                (\phoneChar ->
                    if Char.isDigit phoneChar then
                        '#'

                    else
                        phoneChar
                )
    , replace = '#'
    }


viewTextInput : Translators -> Basic -> Html Msg
viewTextInput ({ t } as translators) basic =
    let
        contactKey =
            case basic.contactType of
                Instagram ->
                    "username"

                Telegram ->
                    "username"

                Link ->
                    "link"

                Email ->
                    "email"

                Whatsapp ->
                    "phone"

                Phone ->
                    "phone"
    in
    Form.Text.init
        { label = t ("contact_form." ++ contactKey ++ ".label")
        , id = ContactType.toString basic.contactType ++ "-input"
        }
        |> Form.Text.withPlaceholder (t ("contact_form." ++ contactKey ++ ".placeholder"))
        |> Form.Text.withContainerAttrs [ class "w-full" ]
        |> (\options ->
                Form.Text.view options
                    { onChange = EnteredContactText basic.contactType
                    , onBlur = NoOp
                    , value = basic.contact
                    , error =
                        case basic.errors of
                            Just (firstError :: _) ->
                                p [ class "form-error" ] [ text firstError ]

                            Just [] ->
                                text ""

                            Nothing ->
                                text ""
                    , hasError = Maybe.Extra.isJust basic.errors
                    , translators = translators
                    , isRequired = False
                    }
           )



-- UTILITIES


usesPhone : ContactType -> Bool
usesPhone contactType =
    List.member contactType [ Whatsapp, Phone ]


unwrap : Normalized -> Contact
unwrap (Normalized contact) =
    contact


addErrors : List String -> Basic -> Basic
addErrors errors basic =
    case errors of
        [] ->
            { basic | errors = Nothing }

        err :: _ ->
            { basic | errors = Just [ err ] }


removeErrors : Kind -> Kind
removeErrors kind =
    case kind of
        Single contact ->
            Single { contact | errors = Nothing }

        Multiple contacts ->
            List.map (\contact -> { contact | errors = Nothing }) contacts
                |> Multiple


isMultiple : Kind -> Bool
isMultiple kind =
    case kind of
        Multiple _ ->
            True

        Single _ ->
            False



-- NORMALIZING


normalize : SupportedCountry -> Validate.Valid Basic -> Normalized
normalize { country } validatedContact =
    let
        { contactType, contact, label } =
            Validate.fromValid validatedContact
    in
    Normalized
        { contactType = contactType
        , contact =
            case contactType of
                Instagram ->
                    "https://instagram.com/" ++ contact

                Phone ->
                    String.join " " [ "+" ++ country.countryCode, contact ]

                Telegram ->
                    "https://t.me/" ++ contact

                Whatsapp ->
                    String.join " " [ "+" ++ country.countryCode, contact ]

                Email ->
                    contact

                Link ->
                    contact
        , label = label
        }



-- VALIDATING


telegramRegex : Regex
telegramRegex =
    Regex.fromString "^[\\w]{5,32}$"
        |> Maybe.withDefault Regex.never


instagramRegex : Regex
instagramRegex =
    Regex.fromString "^[\\w](?!.*?\\.{2})[\\w.]{1,28}[\\w]$"
        |> Maybe.withDefault Regex.never


validateRegex : Regex -> String -> Validate.Validator String Basic
validateRegex regex error =
    Validate.fromErrors
        (\{ contact } ->
            if Regex.contains regex contact then
                []

            else
                [ error ]
        )


validatePhone : String -> Validate.Validator String Basic
validatePhone error =
    Validate.fromErrors
        (\{ supportedCountry, contact } ->
            if
                PhoneNumber.valid
                    { defaultCountry = supportedCountry.country
                    , otherCountries = []
                    , types = PhoneNumber.anyType
                    }
                    contact
            then
                []

            else
                [ error ]
        )


validator : ContactType -> Translators -> Validate.Validator String Basic
validator contactType translators =
    let
        ( specificValidation, field ) =
            case contactType of
                Phone ->
                    ( validatePhone, "phone" )

                Whatsapp ->
                    ( validatePhone, "phone" )

                Instagram ->
                    ( validateRegex instagramRegex, "username" )

                Telegram ->
                    ( validateRegex telegramRegex, "username" )

                Email ->
                    ( \error -> Validate.ifInvalidEmail .contact (\_ -> error), "email" )

                Link ->
                    ( \error ->
                        Validate.fromErrors
                            (\{ contact } ->
                                let
                                    withProtocol =
                                        if String.isEmpty contact then
                                            ""

                                        else if String.startsWith "https://" contact || String.startsWith "http://" contact then
                                            contact

                                        else
                                            "http://" ++ contact
                                in
                                case Url.fromString withProtocol of
                                    Nothing ->
                                        [ error ]

                                    Just _ ->
                                        []
                            )
                    , "link"
                    )

        baseTranslation =
            "contact_form.validation"
    in
    Validate.all
        [ Validate.ifBlank .contact
            (translators.t (String.join "." [ baseTranslation, field, "blank" ]))
        , specificValidation
            (translators.t (String.join "." [ baseTranslation, field, "invalid" ]))
        ]



-- COUNTRY


type alias SupportedCountry =
    { country : Country
    , phonePlaceholder : String
    , flagIcon : String
    }


defaultCountry : SupportedCountry
defaultCountry =
    { country = Countries.countryBR
    , phonePlaceholder = "11 91234 5678"
    , flagIcon = "/icons/flag-brazil.svg"
    }


{-| To add a new `SupportedCountry`, use flags from this figma file:
<https://www.figma.com/file/uAZdgsI1d7qW5kyBNWQZp1>
-}
supportedCountries : List SupportedCountry
supportedCountries =
    [ defaultCountry
    , { country = Countries.countryCA
      , phonePlaceholder = "123 456 7890"
      , flagIcon = "/icons/flag-canada.svg"
      }
    , { country = Countries.countryCR
      , phonePlaceholder = "8123 4567"
      , flagIcon = "/icons/flag-costa-rica.svg"
      }
    , { country = Countries.countryET
      , phonePlaceholder = "91 234 5678"
      , flagIcon = "/icons/flag-ethiopia.svg"
      }
    , { country = Countries.countryGB
      , phonePlaceholder = "20 1234 5678"
      , flagIcon = "/icons/flag-united-kingdom.svg"
      }
    , { country = Countries.countryNZ
      , phonePlaceholder = "2123 4567(89)"
      , flagIcon = "/icons/flag-new-zealand.svg"
      }
    , { country = Countries.countryPT
      , phonePlaceholder = "12 345 6789"
      , flagIcon = "/icons/flag-portugal.svg"
      }
    , { country = Countries.countryUS
      , phonePlaceholder = "209 123 4567"
      , flagIcon = "/icons/flag-usa.svg"
      }
    , { country = Countries.countryZA
      , phonePlaceholder = "71 123 4567"
      , flagIcon = "/icons/flag-south-africa.svg"
      }
    ]



-- GRAPHQL


selectionSet : SelectionSet (Maybe Normalized) Cambiatus.Object.Contact
selectionSet =
    SelectionSet.succeed
        (\maybeType maybeExternalId label ->
            case ( maybeType, maybeExternalId ) of
                ( Just type_, Just externalId ) ->
                    Normalized
                        { contactType = type_
                        , contact = externalId
                        , label = label
                        }
                        |> Just

                _ ->
                    Nothing
        )
        |> with Cambiatus.Object.Contact.type_
        |> with Cambiatus.Object.Contact.externalId
        |> with Cambiatus.Object.Contact.label


profileSelectionSet : SelectionSet Profile Cambiatus.Object.User
profileSelectionSet =
    SelectionSet.succeed Profile
        |> with (Eos.nameSelectionSet User.account)
        |> with
            (User.contacts selectionSet
                |> SelectionSet.map (List.filterMap identity)
            )


mutation : List Normalized -> SelectionSet (Maybe Profile) RootMutation
mutation contacts =
    let
        contactInput (Normalized { contactType, contact, label }) =
            { type_ = Present contactType
            , externalId = Present contact
            , label = Graphql.OptionalArgument.fromMaybe label
            }
    in
    Cambiatus.Mutation.user
        { input =
            { avatar = Absent
            , bio = Absent
            , claimNotification = Absent
            , contacts = Present (List.map contactInput contacts)
            , digest = Absent
            , email = Absent
            , interests = Absent
            , location = Absent
            , name = Absent
            , transferNotification = Absent
            }
        }
        profileSelectionSet
