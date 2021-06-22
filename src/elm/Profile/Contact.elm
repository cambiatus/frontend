module Profile.Contact exposing
    ( ContactResponse(..)
    , Model
    , Msg
    , Normalized
    , circularIcon
    , contactTypeColor
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

import Api.Graphql
import Cambiatus.Enum.ContactType as ContactType exposing (ContactType(..))
import Cambiatus.Mutation
import Cambiatus.Object
import Cambiatus.Object.Contact
import Cambiatus.Object.User as User
import Eos.Account as Eos
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Attribute, Html, a, button, div, img, p, text)
import Html.Attributes exposing (class, classList, disabled, href, src, style, type_)
import Html.Events exposing (onClick, onSubmit)
import Icons
import List.Extra as LE
import PhoneNumber exposing (Country)
import PhoneNumber.Countries as Countries
import Regex exposing (Regex)
import RemoteData exposing (RemoteData)
import Session.Shared exposing (Shared, Translators)
import Validate
import View.Components
import View.Form
import View.Form.Input as Input
import View.Form.Select as Select
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
    , errors : Maybe (List String)
    , showFlags : Bool
    }


initBasic : ContactType -> Basic
initBasic contactType =
    { supportedCountry = defaultCountry
    , contactType = contactType
    , contact = ""
    , errors = Nothing
    , showFlags = False
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


type ContactResponse
    = NotAsked
    | WithContacts String (List Normalized) Bool
    | WithError String



-- UPDATE


type Msg
    = SelectedCountry ContactType SupportedCountry
    | ClickedToggleContactFlags ContactType
    | EnteredContactText ContactType String
    | ClickedDeleteContact ContactType
    | ClosedDeleteModal
    | ConfirmedDeleteContact ContactType
    | EnteredContactOption ContactType
    | ClickedSubmit
    | CompletedUpdateContact UpdatedData
    | CompletedDeleteContact UpdatedData


update : Msg -> Model -> Shared -> String -> List Normalized -> ( Model, Cmd Msg, ContactResponse )
update msg model ({ translators } as shared) authToken profileContacts =
    let
        toggleFlags ({ showFlags } as contact) =
            { contact | showFlags = not showFlags }

        setCountry newCountry contact =
            { contact | supportedCountry = newCountry, errors = Nothing }

        setContact newContact contact =
            { contact | contact = newContact }

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
    in
    case msg of
        SelectedCountry contactType country ->
            ( updateKind contactType (setCountry country)
            , Cmd.none
            , NotAsked
            )

        ClickedToggleContactFlags contactType ->
            ( updateKind contactType toggleFlags
            , Cmd.none
            , NotAsked
            )

        EnteredContactText contactType newContact ->
            ( updateKind contactType (setContact newContact)
            , Cmd.none
            , NotAsked
            )

        ClosedDeleteModal ->
            ( { model | contactTypeToDelete = Nothing }
            , Cmd.none
            , NotAsked
            )

        ClickedDeleteContact contactType ->
            ( { model | contactTypeToDelete = Just contactType }
            , Cmd.none
            , NotAsked
            )

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
            ( { withoutContact
                | contactTypeToDelete = Nothing
                , state = RemoteData.Loading
              }
            , Api.Graphql.mutation shared
                (Just authToken)
                (mutation newContacts)
                CompletedDeleteContact
            , NotAsked
            )

        EnteredContactOption option ->
            case model.kind of
                Single contact ->
                    ( { model
                        | kind =
                            Single
                                { contact
                                    | contactType = option
                                    , errors = Nothing
                                    , contact = ""
                                }
                      }
                    , Cmd.none
                    , NotAsked
                    )

                Multiple _ ->
                    ( model
                    , Cmd.none
                    , NotAsked
                    )

        ClickedSubmit ->
            case submit translators model.kind of
                Err withError ->
                    ( { model | kind = withError }
                    , Cmd.none
                    , NotAsked
                    )

                Ok normalized ->
                    ( { model
                        | state = RemoteData.Loading
                        , kind = removeErrors model.kind
                      }
                    , Api.Graphql.mutation shared
                        (Just authToken)
                        (mutation normalized)
                        CompletedUpdateContact
                    , NotAsked
                    )

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
            in
            ( { model | state = result }
            , Cmd.none
            , case result of
                RemoteData.Success (Just profile) ->
                    WithContacts (feedbackString True) profile.contacts True

                RemoteData.Success Nothing ->
                    WithError (feedbackString False)

                RemoteData.Failure _ ->
                    WithError (feedbackString False)

                RemoteData.NotAsked ->
                    NotAsked

                RemoteData.Loading ->
                    NotAsked
            )

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
            in
            ( { model | state = RemoteData.NotAsked }
            , Cmd.none
            , case result of
                RemoteData.Success (Just profile) ->
                    WithContacts (feedbackString True) profile.contacts False

                RemoteData.Success Nothing ->
                    WithError (feedbackString False)

                RemoteData.Failure _ ->
                    WithError (feedbackString False)

                RemoteData.NotAsked ->
                    NotAsked

                RemoteData.Loading ->
                    NotAsked
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
                    if usesPhone contact.contactType then
                        "contact_form.phone.submit"

                    else
                        "contact_form.username.submit"

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
        [ div [ class "font-menu font-medium flex items-center mb-4 justify-between" ]
            [ div [ class "flex items-center" ]
                [ contactTypeToIcon "mr-2" False basic.contactType
                , text (contactTypeToString translators basic.contactType)
                ]
            , button
                [ onClick (ClickedDeleteContact basic.contactType)
                , type_ "button"
                ]
                [ Icons.trash "" ]
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
            Icons.phone class_

        Instagram ->
            Icons.instagram class_

        Telegram ->
            Icons.telegram class_

        Whatsapp ->
            if isInverted then
                Icons.whatsappInverted class_

            else
                Icons.whatsapp class_


circularIcon : String -> Normalized -> Html msg
circularIcon class_ (Normalized normalized) =
    let
        bgColor =
            case normalized.contactType of
                Phone ->
                    "bg-orange-300"

                Instagram ->
                    "bg-instagram"

                Telegram ->
                    "bg-telegram"

                Whatsapp ->
                    "bg-whatsapp"
    in
    case normalized.contactType of
        Telegram ->
            a [ toHref (Normalized normalized) ]
                [ contactTypeToIcon class_ False normalized.contactType
                ]

        _ ->
            a
                [ class
                    (String.join " "
                        [ "p-2 rounded-full flex items-center justify-center"
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


contactTypeColor : ContactType -> String
contactTypeColor contactType =
    case contactType of
        Phone ->
            "phone"

        Instagram ->
            "instagram"

        Telegram ->
            "telegram"

        Whatsapp ->
            "whatsapp"


viewInput : Translators -> Basic -> Html Msg
viewInput translators basic =
    div [ class "flex space-x-4" ]
        (if usesPhone basic.contactType then
            [ viewFlagsSelect translators basic, viewPhoneInput translators basic ]

         else
            [ viewProfileInput translators basic ]
        )


viewContactTypeSelect : Translators -> ContactType -> Html Msg
viewContactTypeSelect translators contactType =
    case ContactType.list of
        [] ->
            text ""

        first :: rest ->
            let
                makeOption contactType_ =
                    { value = contactType_
                    , label = contactTypeToString translators contactType_
                    }
            in
            Select.init
                { id = "contact_type"
                , label = translators.t "contact_form.contact_type"
                , onInput = EnteredContactOption
                , firstOption = makeOption first
                , value = contactType
                , valueToString = ContactType.toString
                , disabled = False
                , problems = Nothing
                }
                |> Select.withOptions (List.map makeOption rest)
                |> Select.toHtml


viewFlagsSelect : Translators -> Basic -> Html Msg
viewFlagsSelect { t } basic =
    let
        countryOptions =
            basic.supportedCountry
                :: List.filter (\country -> country /= basic.supportedCountry) supportedCountries

        flag classes supportedCountry =
            button
                [ class ("w-full flex items-center space-x-2 text-menu " ++ classes)
                , onClick (SelectedCountry basic.contactType supportedCountry)
                , type_ "button"
                ]
                [ img
                    [ class "w-7"
                    , src supportedCountry.flagIcon
                    ]
                    []
                , p [ class "justify-self-center text-left", style "min-width" "4ch" ]
                    [ text ("+" ++ supportedCountry.country.countryCode) ]
                ]

        id =
            ContactType.toString basic.contactType ++ "_select"
    in
    div [ class "mb-10 flex-shrink-0", Html.Attributes.id id ]
        [ if basic.showFlags then
            button
                [ class "fixed top-0 left-0 h-screen w-screen cursor-default z-40"
                , onClick (ClickedToggleContactFlags basic.contactType)
                , type_ "button"
                ]
                []

          else
            text ""
        , View.Form.label id (t "contact_form.country")
        , button
            [ class "form-select select relative"
            , classList [ ( "border-none mx-px", basic.showFlags ) ]
            , onClick (ClickedToggleContactFlags basic.contactType)
            , type_ "button"
            ]
            [ flag "" basic.supportedCountry
            , if basic.showFlags then
                div
                    [ class "absolute form-input -mx-px inset-x-0 top-0 space-y-4 z-50 h-44 overflow-auto" ]
                    (List.map (flag "mt-1") countryOptions)

              else
                text ""
            ]
        ]


viewPhoneInput : Translators -> Basic -> Html Msg
viewPhoneInput ({ t, tr } as translators) basic =
    div [ class "w-full" ]
        [ Input.init
            { label = t "contact_form.phone.label"
            , id = ContactType.toString basic.contactType ++ "_input"
            , onInput = EnteredContactText basic.contactType
            , disabled = False
            , value = basic.contact
            , placeholder =
                Just
                    (tr "contact_form.phone.placeholder"
                        [ ( "example_number", basic.supportedCountry.phonePlaceholder ) ]
                    )
            , problems = basic.errors
            , translators = translators
            }
            |> Input.toHtml
        ]


viewProfileInput : Translators -> Basic -> Html Msg
viewProfileInput ({ t } as translators) basic =
    div [ class "w-full" ]
        [ Input.init
            { label = t "contact_form.username.label"
            , id = ContactType.toString basic.contactType ++ "_input"
            , onInput = EnteredContactText basic.contactType
            , disabled = False
            , value = basic.contact
            , placeholder = Just (t "contact_form.username.placeholder")
            , problems = basic.errors
            , translators = translators
            }
            |> Input.toHtml
        ]



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
        { contactType, contact } =
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
    ]



-- GRAPHQL


selectionSet : SelectionSet (Maybe Normalized) Cambiatus.Object.Contact
selectionSet =
    SelectionSet.succeed
        (\maybeType maybeExternalId ->
            case ( maybeType, maybeExternalId ) of
                ( Just type_, Just externalId ) ->
                    Contact type_ externalId |> Normalized |> Just

                _ ->
                    Nothing
        )
        |> with Cambiatus.Object.Contact.type_
        |> with Cambiatus.Object.Contact.externalId


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
        contactInput (Normalized { contactType, contact }) =
            { type_ = Present contactType, externalId = Present contact }
    in
    Cambiatus.Mutation.updateUser
        { input =
            { avatar = Absent
            , bio = Absent
            , contacts = Present (List.map contactInput contacts)
            , email = Absent
            , interests = Absent
            , location = Absent
            , name = Absent
            }
        }
        profileSelectionSet
