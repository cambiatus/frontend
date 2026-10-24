module Page.Dashboard.Analysis exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , update
    , view
    )

{-| Claim validation moved to the new app (`/app/review`). The route stays so that old links
and bookmarks land on a page that explains it.
-}

import Html exposing (Html)
import Json.Encode as Encode
import ReviewMoved
import Session.LoggedIn as LoggedIn
import UpdateResult as UR


init : LoggedIn.Model -> UpdateResult
init _ =
    UR.init {}


type alias Model =
    {}


{-| The page has no interactions.
-}
type alias Msg =
    Never


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view { shared } _ =
    { title = shared.translators.t "review_moved.title"
    , content = ReviewMoved.viewMovedContent shared.translators shared.endpoints.api
    }


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg _ _ =
    never msg


jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
jsAddressToMsg _ _ =
    Nothing


msgToString : Msg -> List String
msgToString msg =
    never msg
