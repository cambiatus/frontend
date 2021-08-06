module View.Sponsorship exposing (ModalMsg, SponsorModal, initModal, modalMsgToString, showModal, updateModal, viewModal, viewSupporter)

import Community
import Eos
import Html exposing (Html, div, img, p, span, text)
import Html.Attributes exposing (class, src)
import I18Next
import Route
import Session.LoggedIn as LoggedIn
import Utils
import View.Feedback as Feedback
import View.Form.Input as Input
import View.Modal as Modal
import View.PaypalButtons as PaypalButtons



-- MODAL


type alias SponsorModal =
    { isVisible : Bool
    , amount : String
    , amountProblem : Maybe ( String, I18Next.Replacements )
    }


initModal : SponsorModal
initModal =
    { isVisible = False
    , amount = "0"
    , amountProblem = Nothing
    }



-- TYPES


type ModalMsg
    = ClosedModal
    | EnteredAmount String
    | PaypalApproved
    | PaypalCanceled
    | PaypalErrored PaypalButtons.Error



-- UPDATE


updateModal : LoggedIn.Model -> ModalMsg -> SponsorModal -> ( SponsorModal, Cmd ModalMsg, Maybe ( Feedback.Status, String ) )
updateModal loggedIn msg model =
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
            , Nothing
            )

        PaypalErrored error ->
            case error of
                PaypalButtons.AmountTooSmall { minimumAmount } ->
                    ( { model
                        | amountProblem =
                            Just
                                ( "sponsorship.amount_too_small"
                                , [ ( "minimum", Utils.formatFloat minimumAmount 2 False ) ]
                                )
                      }
                    , Cmd.none
                    , Nothing
                    )

                PaypalButtons.AmountTooBig { maximumAmount } ->
                    ( { model
                        | amountProblem =
                            Just
                                ( "sponsorship.amount_too_big"
                                , [ ( "maximum", Utils.formatFloat maximumAmount 2 False ) ]
                                )
                      }
                    , Cmd.none
                    , Nothing
                    )

                PaypalButtons.LoadError ->
                    ( { model | isVisible = False }
                    , Cmd.none
                    , Just ( Feedback.Failure, loggedIn.shared.translators.t "sponsorship.load_error" )
                    )

                PaypalButtons.UnknownError ->
                    ( { model | isVisible = False }
                    , Cmd.none
                    , Just ( Feedback.Failure, loggedIn.shared.translators.t "sponsorship.unknown_error" )
                    )



-- VIEW


viewModal : LoggedIn.Model -> Community.Model -> SponsorModal -> Html ModalMsg
viewModal loggedIn community model =
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
                        , problems =
                            model.amountProblem
                                |> Maybe.map
                                    (\( translation, replacements ) ->
                                        [ loggedIn.shared.translators.tr translation replacements ]
                                    )
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


showModal : SponsorModal -> SponsorModal
showModal model =
    { model | isVisible = True }


modalMsgToString : ModalMsg -> List String
modalMsgToString msg =
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



-- OTHER COMPONENTS
-- TODO - Use supporter


viewSupporter : Html msg
viewSupporter =
    div [ class "flex py-4" ]
        [ div [ class "bg-gray-500 rounded-full w-14 h-14" ] []
        , div [ class "ml-4" ]
            [ p [ class "font-bold text-sm md:text-base" ] [ text "Cecília Braga" ]
            , p [ class "text-green" ]
                [ span [ class "text-heading font-bold mr-1" ] [ text "10" ]
                , span [ class "uppercase text-caption md:text-xs" ] [ text "dollars" ]
                ]
            ]
        , span [ class "ml-auto uppercase text-caption text-gray-900" ] [ text "22 feb 2021" ]
        ]
