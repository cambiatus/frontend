module Page.Community.Sponsor exposing (Model, Msg, init, msgToString, update, view)

import Browser.Dom
import Community
import Eos
import Html exposing (Html, div, h1, img, label, text)
import Html.Attributes exposing (class, for, src)
import I18Next
import Log
import Page
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Translators)
import Task
import UpdateResult as UR
import Utils
import View.Feedback
import View.Form.Input as Input
import View.PaypalButtons



-- MODEL


type alias Model =
    { amount : String
    , amountProblem : Maybe ( String, I18Next.Replacements )
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( { amount = ""
      , amountProblem = Nothing
      }
    , Browser.Dom.focus amountFieldId
        |> Task.attempt (\_ -> NoOp)
    )



-- UPDATE


type Msg
    = NoOp
    | EnteredAmount String
    | PaypalApproved
    | PaypalCanceled
    | PaypalErrored View.PaypalButtons.Error


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        EnteredAmount amount ->
            { model | amount = amount }
                |> UR.init

        PaypalApproved ->
            UR.init model
                |> UR.addCmd (Route.pushUrl loggedIn.shared.navKey Route.CommunityThankYou)

        PaypalCanceled ->
            -- TODO - Check this
            UR.init model

        PaypalErrored error ->
            case error of
                View.PaypalButtons.AmountTooSmall { minimumAmount } ->
                    { model
                        | amountProblem =
                            Just
                                ( "sponsorship.amount_too_small"
                                , [ ( "minimum", Utils.formatFloat minimumAmount 2 False ) ]
                                )
                    }
                        |> UR.init

                View.PaypalButtons.AmountTooBig { maximumAmount } ->
                    { model
                        | amountProblem =
                            Just
                                ( "sponsorship.amount_too_big"
                                , [ ( "maximum", Utils.formatFloat maximumAmount 2 False ) ]
                                )
                    }
                        |> UR.init

                View.PaypalButtons.LoadError ->
                    model
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure (loggedIn.shared.translators.t "sponsorship.load_error"))
                        |> UR.logEvent
                            { username = Just loggedIn.accountName
                            , message = "Error loading paypal buttons"
                            , tags = [ Log.TypeTag Log.PaypalError ]
                            , location = { moduleName = "Page.Community.Sponsor", function = "update" }
                            , contexts = [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                            , transaction = msg
                            , level = Log.Error
                            }

                View.PaypalButtons.UnknownError ->
                    model
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure (loggedIn.shared.translators.t "sponsorship.unknown_error"))


amountFieldId : String
amountFieldId =
    "sponsor-amount-input"



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        title =
            -- TODO - I18N
            loggedIn.shared.translators.t "sponsorship.sponsor"

        content =
            div [ class "flex flex-col md:flex-grow" ]
                [ Page.viewHeader loggedIn title
                , case loggedIn.selectedCommunity of
                    RemoteData.Success community ->
                        view_ loggedIn.shared.translators community model

                    RemoteData.Loading ->
                        Page.fullPageLoading loggedIn.shared

                    RemoteData.NotAsked ->
                        Page.fullPageLoading loggedIn.shared

                    RemoteData.Failure err ->
                        Page.fullPageGraphQLError title err
                ]
    in
    { title = title
    , content = content
    }


view_ : Translators -> Community.Model -> Model -> Html Msg
view_ ({ t, tr } as translators) community model =
    let
        dollarSymbol =
            Eos.symbolFromString "2,USD"
                -- We know the above symbol string can be parsed
                |> Maybe.withDefault community.symbol
    in
    div [ class "m-4 bg-white rounded md:m-0 md:bg-white md:flex-grow" ]
        [ div [ class "container mx-auto" ]
            [ div [ class "flex flex-col items-center w-full px-4 py-7 bg-white md:w-1/2 md:mx-auto" ]
                [ img [ class "h-10", src community.logo ] []

                -- TODO - Use new typography text-size class
                , h1 [ class "font-bold text-black text-[46px] mb-8" ] [ text community.name ]
                , label
                    [ for "sponsor-amount-input"

                    -- TODO - Move this to the input's label, use new typography text-size class
                    , class "text-center text-purple-500 text-[22px] mb-2 font-bold"
                    ]
                    -- TODO - I18N
                    [ text "Digite o valor que deseja contribuir" ]
                , Input.init
                    { label = ""
                    , id = "sponsor-amount-input"
                    , onInput = EnteredAmount

                    -- TODO - Check disabled
                    , disabled = False
                    , value = model.amount
                    , placeholder = Nothing
                    , problems =
                        model.amountProblem
                            |> Maybe.map
                                (\( translation, replacements ) ->
                                    [ tr translation replacements ]
                                )
                    , translators = translators
                    }
                    |> Input.withContainerAttrs [ class "w-full lg:w-1/2" ]
                    |> Input.withCurrency dollarSymbol
                    |> Input.toHtml
                , View.PaypalButtons.view [ class "w-full" ]
                    { id = "sponsorship-paypal-buttons"
                    , value =
                        model.amount
                            |> String.toFloat
                            -- TODO - Check if we can use default 0
                            |> Maybe.withDefault 0
                    , communityName = community.name
                    , onApprove = PaypalApproved
                    , onCancel = PaypalCanceled
                    , onError = PaypalErrored
                    }
                ]
            ]
        ]



-- UTILS


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        EnteredAmount _ ->
            [ "EnteredAmount" ]

        PaypalApproved ->
            [ "PaypalApproved" ]

        PaypalCanceled ->
            [ "PaypalCanceled" ]

        PaypalErrored _ ->
            [ "PaypalErrored" ]
