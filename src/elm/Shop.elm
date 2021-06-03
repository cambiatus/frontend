module Shop exposing
    ( Filter(..)
    , Product
    , ProductId
    , encodeTransferSale
    , productQuery
    , productsQuery
    )

import Avatar exposing (Avatar)
import Cambiatus.Object
import Cambiatus.Object.Product
import Cambiatus.Object.User as User
import Cambiatus.Query as Query
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Json.Encode as Encode exposing (Value)
import Profile.Contact as Contact



-- Sale


type alias Product =
    { id : Int
    , title : String
    , description : String
    , creatorId : Eos.Name
    , price : Float
    , symbol : Symbol
    , image : Maybe String
    , units : Int
    , trackStock : Bool
    , creator : ShopProfile
    }


type alias ProductId =
    String


type alias ShopProfile =
    { account : Eos.Name
    , name : Maybe String
    , avatar : Avatar
    , email : Maybe String
    , bio : Maybe String
    , contacts : List Contact.Normalized
    }


type Filter
    = UserSales
    | All



-- Type used when transfering to a sale


type alias TransferSale =
    { id : Int
    , from : Eos.Name
    , to : Eos.Name
    , quantity : Eos.Asset
    , units : Int
    }


encodeTransferSale : TransferSale -> Value
encodeTransferSale t =
    Encode.object
        [ ( "sale_id", Encode.int t.id )
        , ( "from", Eos.encodeName t.from )
        , ( "to", Eos.encodeName t.to )
        , ( "quantity", Eos.encodeAsset t.quantity )
        , ( "units", Encode.int t.units )
        ]



-- PRODUCT GRAPHQL API


productSelection : SelectionSet Product Cambiatus.Object.Product
productSelection =
    SelectionSet.succeed Product
        |> with Cambiatus.Object.Product.id
        |> with Cambiatus.Object.Product.title
        |> with Cambiatus.Object.Product.description
        |> with (Eos.nameSelectionSet Cambiatus.Object.Product.creatorId)
        |> with Cambiatus.Object.Product.price
        |> with (Eos.symbolSelectionSet Cambiatus.Object.Product.communityId)
        |> with Cambiatus.Object.Product.image
        |> with Cambiatus.Object.Product.units
        |> with Cambiatus.Object.Product.trackStock
        |> with (Cambiatus.Object.Product.creator shopProfileSelectionSet)


shopProfileSelectionSet : SelectionSet ShopProfile Cambiatus.Object.User
shopProfileSelectionSet =
    SelectionSet.succeed ShopProfile
        |> with (Eos.nameSelectionSet User.account)
        |> with User.name
        |> with (Avatar.selectionSet User.avatar)
        |> with User.email
        |> with User.bio
        |> with
            (User.contacts Contact.selectionSet
                |> SelectionSet.map (List.filterMap identity)
            )


productQuery : Int -> SelectionSet (Maybe Product) RootQuery
productQuery saleId =
    Query.product { id = saleId } productSelection


productsQuery : Filter -> Eos.Name -> Symbol -> SelectionSet (List Product) RootQuery
productsQuery filter accName communityId =
    case filter of
        UserSales ->
            let
                args =
                    \_ ->
                        { filters = Present { account = Eos.nameToString accName, inStock = Absent }
                        }
            in
            Query.products args { communityId = Eos.symbolToString communityId } productSelection

        All ->
            let
                args =
                    { communityId = Eos.symbolToString communityId
                    }
            in
            Query.products identity args productSelection
