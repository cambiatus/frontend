module Shop exposing
    ( Id
    , ImageId
    , Product
    , ProductPreview
    , StockTracking(..)
    , createProduct
    , deleteProduct
    , encodeTransferSale
    , getAvailableUnits
    , hasUnitTracking
    , idSelectionSet
    , idToString
    , idUrlParser
    , isOutOfStock
    , productPreviewQuery
    , productQuery
    , productSelectionSet
    , productsQuery
    , updateProduct
    , updateProductCategories
    , viewImageCarrousel
    )

import Avatar
import Cambiatus.Mutation as Mutation
import Cambiatus.Object
import Cambiatus.Object.Product
import Cambiatus.Object.ProductImage
import Cambiatus.Object.ProductPreview
import Cambiatus.Query as Query
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, button, div, img, text, ul)
import Html.Attributes exposing (alt, class, classList, id, src)
import Html.Attributes.Aria exposing (ariaHidden, ariaLabel)
import Html.Events exposing (onClick)
import Icons
import Json.Encode as Encode exposing (Value)
import Markdown exposing (Markdown)
import Profile
import Shop.Category
import Translation
import Url.Parser
import Utils exposing (onClickPreventAll)
import View.Components



-- Sale


type alias Product =
    { id : Id
    , title : String
    , description : Markdown
    , creatorId : Eos.Name
    , price : Float
    , symbol : Symbol
    , images : List String
    , stockTracking : StockTracking
    , creator : Profile.Minimal
    , categories : List Shop.Category.Model
    }



-- ID


type Id
    = Id Int


idToString : Id -> String
idToString (Id id) =
    String.fromInt id


idUrlParser : Url.Parser.Parser (Id -> a) a
idUrlParser =
    Url.Parser.int
        |> Url.Parser.map Id


idSelectionSet : SelectionSet Id Cambiatus.Object.Product
idSelectionSet =
    SelectionSet.map Id Cambiatus.Object.Product.id


encodeId : Id -> Value
encodeId (Id id) =
    Encode.int id


type StockTracking
    = NoTracking
    | UnitTracking { availableUnits : Int }


type alias ProductPreview =
    { symbol : Symbol
    , creator : Profile.Minimal
    , description : Markdown
    , id : Id
    , images : List String
    , price : Float
    , title : String
    }



-- Type used when transfering to a sale


type alias TransferSale =
    { id : Id
    , from : Eos.Name
    , to : Eos.Name
    , quantity : Eos.Asset
    , units : Int
    }


encodeTransferSale : TransferSale -> Value
encodeTransferSale t =
    Encode.object
        [ ( "sale_id", encodeId t.id )
        , ( "from", Eos.encodeName t.from )
        , ( "to", Eos.encodeName t.to )
        , ( "quantity", Eos.encodeAsset t.quantity )
        , ( "units", Encode.int t.units )
        ]



-- PRODUCT GRAPHQL API


productSelectionSet : SelectionSet Product Cambiatus.Object.Product
productSelectionSet =
    SelectionSet.succeed
        (\id title description creatorId price symbol images maybeUnits trackStock creator categories ->
            { id = id
            , title = title
            , description = description
            , creatorId = creatorId
            , price = price
            , symbol = symbol
            , images =
                images
                    |> List.filter (not << String.isEmpty)
            , stockTracking =
                if trackStock then
                    case maybeUnits of
                        Nothing ->
                            NoTracking

                        Just units ->
                            UnitTracking { availableUnits = units }

                else
                    NoTracking
            , creator = creator
            , categories = categories
            }
        )
        |> with idSelectionSet
        |> with Cambiatus.Object.Product.title
        |> with (Markdown.selectionSet Cambiatus.Object.Product.description)
        |> with (Eos.nameSelectionSet Cambiatus.Object.Product.creatorId)
        |> with Cambiatus.Object.Product.price
        |> with (Eos.symbolSelectionSet Cambiatus.Object.Product.communityId)
        |> with (Cambiatus.Object.Product.images Cambiatus.Object.ProductImage.uri)
        |> with Cambiatus.Object.Product.units
        |> with Cambiatus.Object.Product.trackStock
        |> with (Cambiatus.Object.Product.creator Profile.minimalSelectionSet)
        |> with (Cambiatus.Object.Product.categories Shop.Category.selectionSet)


productPreviewSelectionSet : SelectionSet ProductPreview Cambiatus.Object.ProductPreview
productPreviewSelectionSet =
    SelectionSet.succeed
        (\communityId creator description id images price title ->
            { symbol = communityId
            , creator = creator
            , description = description
            , id = id
            , images =
                images
                    |> List.filter (not << String.isEmpty)
            , price = price
            , title = title
            }
        )
        |> with (Eos.symbolSelectionSet Cambiatus.Object.ProductPreview.communityId)
        |> with
            (Eos.nameSelectionSet Cambiatus.Object.ProductPreview.creatorId
                |> SelectionSet.map productPreviewProfile
            )
        |> with (Markdown.selectionSet Cambiatus.Object.ProductPreview.description)
        |> with (SelectionSet.map Id Cambiatus.Object.ProductPreview.id)
        |> with (Cambiatus.Object.ProductPreview.images Cambiatus.Object.ProductImage.uri)
        |> with Cambiatus.Object.ProductPreview.price
        |> with Cambiatus.Object.ProductPreview.title


productPreviewProfile : Eos.Name -> Profile.Minimal
productPreviewProfile accountName =
    { account = accountName
    , name = accountName |> Eos.nameToString |> Just
    , avatar = Avatar.empty
    , email = Nothing
    , bio = Nothing
    , contacts = []
    }


productQuery : Id -> SelectionSet (Maybe Product) RootQuery
productQuery (Id saleId) =
    Query.product { id = saleId } productSelectionSet


productPreviewQuery : Id -> SelectionSet ProductPreview RootQuery
productPreviewQuery (Id productId) =
    Query.productPreview { id = productId } productPreviewSelectionSet


productsQuery : { user : Maybe Eos.Name, categories : List Shop.Category.Id } -> SelectionSet (List Product) RootQuery
productsQuery { user, categories } =
    Query.products
        (\_ ->
            { filters =
                Present
                    { account =
                        user
                            |> Maybe.map Eos.nameToString
                            |> Graphql.OptionalArgument.fromMaybe
                    , categoriesIds =
                        if List.isEmpty categories then
                            Absent

                        else
                            categories
                                |> List.map (Shop.Category.idToInt >> Just)
                                |> Present
                    , inStock = Absent
                    }
            }
        )
        productSelectionSet


createProduct :
    { title : String
    , description : Markdown
    , images : List String
    , categories : List Shop.Category.Id
    , price : Float
    , stockTracking : StockTracking
    }
    -> SelectionSet decodesTo Cambiatus.Object.Product
    -> SelectionSet (Maybe decodesTo) RootMutation
createProduct options selectionSet =
    upsert
        { id = Nothing
        , title = options.title
        , description = options.description
        , images = options.images
        , categories = options.categories
        , price = options.price
        , stockTracking = options.stockTracking
        }
        selectionSet


{-| Images will be overwritten with whatever is passed in here. If you want to
keep the existing images, you must include them in the `images` field.
-}
updateProduct :
    { id : Id
    , title : String
    , description : Markdown
    , images : List String
    , categories : List Shop.Category.Id
    , price : Float
    , stockTracking : StockTracking
    }
    -> SelectionSet decodesTo Cambiatus.Object.Product
    -> SelectionSet (Maybe decodesTo) RootMutation
updateProduct options selectionSet =
    upsert
        { id = Just options.id
        , title = options.title
        , description = options.description
        , images = options.images
        , categories = options.categories
        , price = options.price
        , stockTracking = options.stockTracking
        }
        selectionSet


updateProductCategories :
    { id : Id, categories : List Shop.Category.Id }
    -> SelectionSet decodesTo Cambiatus.Object.Product
    -> SelectionSet (Maybe decodesTo) RootMutation
updateProductCategories { id, categories } selectionSet =
    let
        (Id unwrappedId) =
            id
    in
    Mutation.product
        (\optionals ->
            { optionals
                | id = Present unwrappedId
                , categories =
                    categories
                        |> List.map Shop.Category.idToInt
                        |> Present
            }
        )
        selectionSet


upsert :
    { id : Maybe Id
    , title : String
    , description : Markdown
    , images : List String
    , categories : List Shop.Category.Id
    , price : Float
    , stockTracking : StockTracking
    }
    -> SelectionSet decodesTo Cambiatus.Object.Product
    -> SelectionSet (Maybe decodesTo) RootMutation
upsert { id, title, description, images, categories, price, stockTracking } =
    Mutation.product
        (\_ ->
            { id =
                case id of
                    Nothing ->
                        Absent

                    Just (Id unwrappedId) ->
                        Present unwrappedId
            , categories =
                categories
                    |> List.map Shop.Category.idToInt
                    |> Present
            , title = Present title
            , description = Present (Markdown.toRawString description)
            , images = Present images
            , price = Present price
            , trackStock =
                case stockTracking of
                    NoTracking ->
                        Present False

                    UnitTracking _ ->
                        Present True
            , units =
                case stockTracking of
                    NoTracking ->
                        Present 0

                    UnitTracking { availableUnits } ->
                        Present availableUnits
            }
        )


deleteProduct :
    Id
    -> SelectionSet decodesTo Cambiatus.Object.DeleteStatus
    -> SelectionSet (Maybe decodesTo) RootMutation
deleteProduct (Id id) =
    Mutation.deleteProduct { id = id }



-- HELPER FUNCTIONS


isOutOfStock : Product -> Bool
isOutOfStock product =
    case product.stockTracking of
        NoTracking ->
            False

        UnitTracking { availableUnits } ->
            availableUnits == 0


hasUnitTracking : Product -> Bool
hasUnitTracking product =
    case product.stockTracking of
        NoTracking ->
            False

        UnitTracking _ ->
            True


getAvailableUnits : Product -> Maybe Int
getAvailableUnits product =
    case product.stockTracking of
        NoTracking ->
            Nothing

        UnitTracking { availableUnits } ->
            Just availableUnits



-- VIEWS


type ImageId
    = ImageId String


{-| This does not treat the case where there are no images. Treat it accordingly!
-}
viewImageCarrousel :
    Translation.Translators
    ->
        { containerAttrs : List (Html.Attribute msg)
        , listAttrs : List (Html.Attribute msg)
        , imageContainerAttrs : List (Html.Attribute msg)
        , imageOverlayAttrs : List (Html.Attribute msg)
        , imageAttrs : List (Html.Attribute msg)
        }
    ->
        { showArrows : Bool
        , productId : Maybe Id
        , onScrollToImage : { containerId : String, imageId : String } -> msg
        , currentIntersecting : Maybe ImageId
        , onStartedIntersecting : ImageId -> msg
        , onStoppedIntersecting : ImageId -> msg
        }
    -> ( String, List String )
    -> Html msg
viewImageCarrousel { t, tr } { containerAttrs, listAttrs, imageContainerAttrs, imageOverlayAttrs, imageAttrs } options ( firstImage, otherImages ) =
    let
        maybeProductId =
            Maybe.map (\(Id opaqueId) -> opaqueId) options.productId

        imageUrls =
            firstImage :: otherImages

        containerId =
            case maybeProductId of
                Nothing ->
                    "image-carrousel"

                Just productId ->
                    "image-carrousel-" ++ String.fromInt productId

        imageId index =
            case maybeProductId of
                Nothing ->
                    "product-image-" ++ String.fromInt index

                Just productId ->
                    "product-image-" ++ String.fromInt productId ++ "-" ++ String.fromInt index

        indexFromImageId (ImageId imgId) =
            case maybeProductId of
                Nothing ->
                    -- 14 == String.length "product-image-"
                    String.dropLeft 14 imgId
                        |> String.toInt

                Just productId ->
                    String.dropLeft (String.length ("product-image-" ++ String.fromInt productId ++ "-")) imgId
                        |> String.toInt

        clickedScrollToImage imageIndex =
            options.onScrollToImage
                { containerId = containerId
                , imageId = imageId imageIndex
                }

        isCurrentIntersecting imageIndex =
            case options.currentIntersecting of
                Nothing ->
                    False

                Just (ImageId currentIntersecting) ->
                    currentIntersecting == imageId imageIndex

        maybeCurrentIntersectingIndex =
            options.currentIntersecting
                |> Maybe.andThen indexFromImageId
    in
    div (class "relative" :: containerAttrs)
        [ case maybeCurrentIntersectingIndex of
            Nothing ->
                text ""

            Just currentIntersectingIndex ->
                div [ class "absolute left-4 bottom-1/2 translate-y-1/2" ]
                    [ button
                        [ class "bg-white/80 rounded-full transition-all origin-left"
                        , classList
                            [ ( "hidden", not options.showArrows )
                            , ( "opacity-0 scale-50 pointer-events-none", currentIntersectingIndex == 0 )
                            ]
                        , onClick (clickedScrollToImage (currentIntersectingIndex - 1))
                        , ariaLabel <| t "shop.carrousel.previous"
                        , ariaHidden (currentIntersectingIndex == 0)
                        ]
                        [ Icons.arrowDown "rotate-90"
                        ]
                    ]
        , ul
            (class "flex h-full min-w-full overflow-x-scroll overflow-y-hidden snap-x scrollbar-hidden"
                :: id containerId
                :: listAttrs
            )
            (List.indexedMap
                (\index image ->
                    div
                        (class "w-full h-full flex-shrink-0 snap-center snap-always grid place-items-center"
                            :: id (imageId index)
                            :: imageContainerAttrs
                        )
                        [ img
                            (src image
                                :: alt ""
                                :: class "object-cover object-center max-w-full max-h-full"
                                :: imageAttrs
                            )
                            []
                        ]
                )
                imageUrls
            )
        , case maybeCurrentIntersectingIndex of
            Nothing ->
                text ""

            Just currentIntersectingIndex ->
                div [ class "absolute right-4 bottom-1/2 translate-y-1/2" ]
                    [ button
                        [ class "bg-white/80 rounded-full transition-all origin-right"
                        , classList
                            [ ( "hidden", not options.showArrows )
                            , ( "opacity-0 scale-50 pointer-events-none", currentIntersectingIndex == List.length imageUrls - 1 )
                            ]
                        , onClick (clickedScrollToImage (currentIntersectingIndex + 1))
                        , ariaLabel <| t "shop.carrousel.next"
                        , ariaHidden (currentIntersectingIndex == List.length imageUrls - 1)
                        ]
                        [ Icons.arrowDown "-rotate-90" ]
                    ]
        , if List.length imageUrls == 1 then
            text ""

          else
            div [ class "absolute bottom-4 left-1/2 -translate-x-1/2 flex items-center gap-2 z-10" ]
                (List.indexedMap
                    (\index _ ->
                        button
                            [ class "border-2 w-2 h-2 rounded-full transition-colors"
                            , classList
                                [ ( "bg-white border-orange-500", isCurrentIntersecting index )
                                , ( "bg-white border-white", not (isCurrentIntersecting index) )
                                ]
                            , onClickPreventAll (clickedScrollToImage index)
                            , ariaLabel <| tr "shop.carrousel.index" [ ( "index", String.fromInt (index + 1) ) ]
                            ]
                            []
                    )
                    imageUrls
                )
        , if List.length imageUrls == 1 then
            text ""

          else
            div
                (class "bg-gradient-to-t from-black/40 to-transparent absolute bottom-0 left-0 right-0 h-10"
                    :: imageOverlayAttrs
                )
                []
        , View.Components.intersectionObserver
            { targetSelectors =
                List.indexedMap (\index _ -> "#" ++ imageId index) imageUrls
            , threshold = 0.01
            , breakpointToExclude = Nothing
            , onStartedIntersecting = Just (ImageId >> options.onStartedIntersecting)
            , onStoppedIntersecting = Just (ImageId >> options.onStoppedIntersecting)
            }
        ]
