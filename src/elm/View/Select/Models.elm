module View.Select.Models exposing (State, newState)


type alias State =
    { id : String
    , query : Maybe String
    , highlightedItem : Maybe Int
    , showMenu : Bool
    }


newState : String -> State
newState id =
    { id = id
    , query = Nothing
    , highlightedItem = Nothing
    , showMenu = False
    }
