module Session exposing (AuthenticatedModel, GuestModel, IdentifiedModel, IdentifiedPageModel, ReadOnly(..), ScatterAvailability(..), Session, TranslationStatus(..))

import Account exposing (Profile)
import Auth
import Browser.Navigation as Nav
import Eos
import Eos.Account as Eos
import Flags exposing (AuthPreference, Endpoints, Environment, Flags, defaultEndpoints)
import Http
import I18Next exposing (Translations, initialTranslations, t)
import Log
import Notification exposing (Notification)
import Ports
import Route exposing (Route)
import Translation



-- INIT


init : Flags -> Nav.Key -> ( Session User, Cmd Msg )
init ({ maybeAccount, authPreference } as flags) navKey_ =
    let
        ( user, userCmd ) =
            case maybeAccount of
                Nothing ->
                    ( Guest initGuestModel
                    , Cmd.none
                    )

                Just ( eosName, isPinAvailable ) ->
                    ( LoadingAccountProfile (initLoadingAccountProfileModel eosName isPinAvailable authPreference Nothing)
                    , Cmd.none
                      --, Api.getProfile shared accountName CompletedLoadProfile
                    )
    in
    ( { readOnly =
            ReadOnly
                { navKey = navKey_
                , environment = flags.environment
                , endpoints = flags.endpoints
                }
      , scatterAvailability = VerifyingScatterAvailability
      , language = flags.language
      , translations = initialTranslations
      , translationsStatus = LoadingTranslation
      , user = user
      }
    , Cmd.batch
        [ userCmd
        ]
    )
        |> and verifyScatterAvailability
        |> and (loadTranslation flags.language)



-- MODEL


type alias Session a =
    { readOnly : ReadOnly
    , scatterAvailability : ScatterAvailability
    , language : String
    , translations : Translations
    , translationsStatus : TranslationStatus
    , user : a
    }


type ReadOnly
    = ReadOnly
        { navKey : Nav.Key
        , environment : Environment
        , endpoints : Endpoints
        }


type TranslationStatus
    = LoadingTranslation
    | LoadingTranslationFailed Http.Error
    | LoadedTranslation
    | LoadingAnotherTranslation
    | LoadingAnotherTranslationFailed Http.Error


type ScatterAvailability
    = VerifyingScatterAvailability
    | ScatterAvailable
    | ScatterUnavailable


type User
    = Guest GuestModel
    | LoadingAccountProfile GuestModel LoadingAccountProfileModel
    | Identified IdentifiedModel
    | Authenticated AuthenticatedModel


type alias GuestModel =
    { showLanguageNav : Bool
    , afterLoginRedirect : Maybe Route
    }


initGuestModel : GuestModel
initGuestModel =
    { showLanguageNav = False
    , afterLoginRedirect = afterLoginRedirect
    }


type alias LoadingAccountProfileModel =
    { eosName : Eos.Name
    , isPinAvailable : Bool
    , authPreference : Maybe AuthPreference
    }


initLoadingAccountProfileModel : Eos.Name -> Bool -> Maybe AuthPreference -> Maybe Route -> LoadingAccountProfileModel
initLoadingAccountProfileModel eosName isPinAvailable authPreference afterLoginRedirect =
    { eosName = eosName
    , isPinAvailable = isPinAvailable
    , authPreference = authPreference
    }


type alias IdentifiedModel =
    { page : IdentifiedPageModel
    , isPinAvailable : Bool
    , authPreference : Maybe AuthPreference
    , showAuthModal : Bool
    , auth : Auth.Model
    }


type alias AuthenticatedModel =
    { page : IdentifiedPageModel
    , privateKey : Eos.PrivateKey
    }


type alias IdentifiedPageModel =
    { profile : Profile
    , showUserNav : Bool
    , showLanguageItems : Bool
    , searchText : String
    , showMainNav : Bool
    , notification : Notification.Model
    , collapseMainNav : Bool
    }


initIdentifiedPageModel : Profile -> IdentifiedPageModel
initIdentifiedPageModel profile =
    { profile = profile
    , showUserNav = False
    , showLanguageItems = False
    , searchText = ""
    , showMainNav = False
    , notification = Notification.init
    , collapseMainNav = False
    }


bespiralSymbol : Session a -> Eos.Symbol
bespiralSymbol shared =
    Eos.bespiralSymbol


unwrapReadOnly (ReadOnly readOnly) =
    readOnly


navKey : Session a -> Nav.Key
navKey session =
    .navKey (unwrapReadOnly session.readOnly)


endpoints : Session a -> Endpoints
endpoints session =
    .endpoints (unwrapReadOnly session.readOnly)


environment : Session a -> Environment
environment session =
    .environment (unwrapReadOnly session.readOnly)



-- UPDATE


type Msg
    = CompletedLoadTranslation String (Result Http.Error Translations)
    | CompletedLoadProfile (Result Http.Error Profile)


update : Msg -> Session User -> ( Session User, Cmd Msg )
update msg session =
    case msg of
        CompletedLoadTranslation lang (Ok transl) ->
            ( { session
                | language = lang
                , translations = transl
                , translationsStatus = LoadedTranslation
              }
            , Ports.storeLanguage lang
            )

        CompletedLoadTranslation lang (Err httpError) ->
            ( { session
                | translationsStatus =
                    case session.translationsStatus of
                        LoadingTranslation ->
                            LoadingTranslationFailed httpError

                        LoadingAnotherTranslation ->
                            LoadingAnotherTranslationFailed httpError

                        _ ->
                            session.translationsStatus
              }
            , Log.httpError httpError
            )

        CompletedLoadProfile (Ok profile_) ->
            case session.user of
                LoadingAccountProfile m ->
                    ( Identified
                        { page = initIdentifiedPageModel profile_
                        , isPinAvailable = m.isPinAvailable
                        , authPreference = m.authPreference
                        , showAuthModal = False
                        }
                    , Cmd.none
                    )

                _ ->
                    ( session, logImpossible [ "CompletedLoadProfile", "Ok" ] )

        CompletedLoadProfile (Err httpError) ->
            case session.user of
                LoadingAccountProfile m ->
                    ( Guest initGuestModel
                    , Cmd.none
                    )

                _ ->
                    ( session, logImpossible [ "CompletedLoadProfile", "Ok" ] )



-- HELPERS


and : (Session a -> ( Session a, Cmd Msg )) -> ( Session a, Cmd Msg ) -> ( Session a, Cmd Msg )
and transform ( session, cmd ) =
    let
        ( updtSession, newCmd ) =
            transform session
    in
    ( updtSession
    , Cmd.batch [ cmd, newCmd ]
    )


verifyScatterAvailability : Session a -> ( Session a, Cmd Msg )
verifyScatterAvailability session =
    ( { session | scatterAvailability = VerifyingScatterAvailability }
    , Cmd.none
    )


tryAgainTranslation : Session a -> ( Session a, Cmd Msg )
tryAgainTranslation session =
    loadTranslation session.language session


loadTranslation : String -> Session a -> ( Session a, Cmd Msg )
loadTranslation language session =
    ( { session
        | translationsStatus =
            case session.translationsStatus of
                LoadedTranslation ->
                    LoadingAnotherTranslation

                LoadingAnotherTranslationFailed _ ->
                    LoadingAnotherTranslation

                _ ->
                    LoadingTranslation
      }
    , CompletedLoadTranslation language
        |> Translation.get (endpoints session) language
    )


gotScatterAvailability : Bool -> Session a -> Session a
gotScatterAvailability isAvailable session =
    { session
        | scatterAvailability =
            if isAvailable then
                ScatterAvailable

            else
                ScatterUnavailable
    }


logImpossible : List String -> Cmd Msg
logImpossible descr =
    [ "Session"
    ]
        ++ descr
        |> String.join "."
        |> Log.impossible
