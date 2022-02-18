module View.Select.Config exposing
    ( Config
    , Style
    , newConfig
    )

import Html exposing (Html)
import View.Select.Styles as Styles


type alias Style =
    ( String, String )


type alias RequiredConfig msg item =
    { onSelect : item -> msg
    , toLabel : item -> String
    , filter : String -> List item -> Maybe (List item)
    , onFocusItem : msg
    }


type alias Config msg item =
    { cutoff : Maybe Int
    , emptySearch : Bool
    , filter : String -> List item -> Maybe (List item)
    , highlightedItemClass : String
    , highlightedItemStyles : List Style
    , inputId : String
    , inputClass : String
    , inputClassList : List ( String, Bool )
    , inputStyles : List Style
    , inputControlClass : String
    , inputControlStyles : List Style
    , inputWrapperClass : String
    , inputWrapperStyles : List Style
    , itemClass : String
    , itemStyles : List Style
    , itemHtml : Maybe (item -> Html Never)
    , isMultiSelect : Bool
    , menuClass : String
    , emptyMenuClass : String
    , menuStyles : List Style
    , multiInputItemContainerClass : String
    , multiInputItemContainerStyles : List Style
    , multiInputItemClass : String
    , multiInputItemStyles : List Style
    , notFound : String
    , notFoundClass : String
    , notFoundShown : Bool
    , notFoundStyles : List Style
    , onQueryChange : Maybe (String -> msg)
    , onSelect : item -> msg
    , onFocus : Maybe msg
    , onBlur : Maybe msg
    , onRemoveItem : Maybe (item -> msg)
    , prompt : String
    , promptClass : String
    , promptStyles : List Style
    , removeItemSvgClass : String
    , removeItemSvgStyles : List Style
    , scoreThreshold : Int
    , toLabel : item -> String
    , transformQuery : String -> String
    , underlineClass : String
    , underlineStyles : List Style
    , disabled : Bool
    , onFocusItem : msg
    }


newConfig : RequiredConfig msg item -> Config msg item
newConfig requiredConfig =
    { emptySearch = False
    , filter = requiredConfig.filter
    , cutoff = Nothing
    , highlightedItemClass = ""
    , highlightedItemStyles = []
    , underlineStyles = []
    , inputId = Styles.inputId
    , inputClass = ""
    , inputClassList = []
    , inputControlClass = ""
    , inputControlStyles = []
    , inputStyles = []
    , inputWrapperClass = ""
    , inputWrapperStyles = []
    , itemClass = ""
    , itemStyles = []
    , itemHtml = Nothing
    , isMultiSelect = False
    , menuClass = ""
    , emptyMenuClass = ""
    , menuStyles = []
    , multiInputItemContainerClass = ""
    , multiInputItemContainerStyles = []
    , multiInputItemClass = ""
    , multiInputItemStyles = []
    , notFound = "No results found"
    , notFoundClass = ""
    , notFoundShown = True
    , notFoundStyles = []
    , onQueryChange = Nothing
    , onSelect = requiredConfig.onSelect
    , onFocus = Nothing
    , onBlur = Nothing
    , onRemoveItem = Nothing
    , prompt = ""
    , promptClass = ""
    , promptStyles = []
    , removeItemSvgClass = ""
    , removeItemSvgStyles = []
    , scoreThreshold = 2000
    , toLabel = requiredConfig.toLabel
    , transformQuery = identity
    , underlineClass = ""
    , disabled = False
    , onFocusItem = requiredConfig.onFocusItem
    }
