module View.SponsorModal exposing (Model, Msg, init, msgToString, show, update, view)

import Community
import Eos
import Html exposing (Html, div, img, p, span, text)
import Html.Attributes exposing (class, src)
import Route
import Session.LoggedIn as LoggedIn
import View.Feedback as Feedback
import View.Form.Input as Input
import View.Modal as Modal
import View.PaypalButtons as PaypalButtons



-- MODEL


type alias Model =
    { isVisible : Bool, amount : String }


init : Model
init =
    { isVisible = False
    , amount = "0"
    }



-- TYPES


type Msg
    = ClosedModal
    | EnteredAmount String
    | PaypalApproved
    | PaypalCanceled
    | PaypalErrored PaypalButtons.Error



-- UPDATE


update : LoggedIn.Model -> Msg -> Model -> ( Model, Cmd Msg, Maybe ( Feedback.Status, String ) )
update loggedIn msg model =
    case msg of
        ClosedModal ->
            ( { model | isVisible = False }, Cmd.none, Nothing )

        EnteredAmount amount ->
            ( { model | amount = amount }, Cmd.none, Nothing )

        PaypalApproved ->
            ( model
            , Route.pushUrl loggedIn.shared.navKey Route.CommunityThankYou
            , Nothing
            )

        PaypalCanceled ->
            ( model
            , Cmd.none
            , Just ( Feedback.Failure, "Cancelled purchase" )
            )

        PaypalErrored _ ->
            ( { model | isVisible = False }
            , Cmd.none
            , Just ( Feedback.Failure, "Error when purchasing" )
            )



-- VIEW


view : LoggedIn.Model -> Community.Model -> Model -> Html Msg
view loggedIn community model =
    let
        dollarSymbol =
            Eos.symbolFromString "2,USD"
                |> Maybe.withDefault community.symbol
    in
    Modal.initWith
        { closeMsg = ClosedModal
        , isVisible = model.isVisible
        }
        |> Modal.withBody
            [ div [ class "w-full px-4 mx-auto md:w-5/6 md:px-0 lg:w-2/3" ]
                [ div [ class "flex flex-col items-center w-full" ]
                    [ img [ class "h-12", src community.logo ] []
                    , p [ class "text-2xl pt-4 pb-8 text-center" ]
                        [ text (loggedIn.shared.translators.t "sponsorship.sponsor")
                        , span [ class "font-bold" ] [ text community.name ]
                        ]
                    , Input.init
                        { label = ""
                        , id = "paypal-amount-input"
                        , onInput = EnteredAmount
                        , disabled = False
                        , value = model.amount
                        , placeholder = Nothing
                        , problems = Nothing
                        , translators = loggedIn.shared.translators
                        }
                        |> Input.withCurrency dollarSymbol
                        |> Input.withContainerAttrs [ class "w-full" ]
                        |> Input.toHtml
                    , PaypalButtons.view [ class "w-full" ]
                        { id = "sponsorship-paypal-buttons"
                        , value =
                            model.amount
                                |> String.toFloat
                                |> Maybe.withDefault 0
                        , communityName = community.name
                        , onApprove = PaypalApproved
                        , onCancel = PaypalCanceled
                        , onError = PaypalErrored
                        }
                    ]
                ]
            ]
        |> Modal.toHtml



-- UTILS


show : Model -> Model
show model =
    { model | isVisible = True }


msgToString : Msg -> List String
msgToString msg =
    case msg of
        ClosedModal ->
            [ "ClosedModal" ]

        EnteredAmount _ ->
            [ "EnteredAmount" ]

        PaypalApproved ->
            [ "PaypalApproved" ]

        PaypalCanceled ->
            [ "PaypalCanceled" ]

        PaypalErrored _ ->
            [ "PaypalErrored" ]
