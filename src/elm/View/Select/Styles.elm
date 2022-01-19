module View.Select.Styles exposing (hiddenMenuStyles, inputClass, inputControlClass, inputControlStyles, inputId, inputStyles, inputWrapperClass, inputWrapperStyles, menuItemStyles, multiInputItemClass, multiInputItemContainerClass, multiInputItemContainerStyles, multiInputItemStyles, multiInputItemText, multiInputRemoveItem, removeItemSvgStyles, underlineStyles, visibleMenuStyles)

-- INPUT CONSTANTS


inputId : String
inputId =
    "elm-select-input"


inputControlClass : String
inputControlClass =
    "elm-select-input-control "


inputControlStyles : List ( String, String )
inputControlStyles =
    [ ( "position", "relative" )
    , ( "background", "white" )
    ]


inputWrapperClass : String
inputWrapperClass =
    "elm-select-input-wrapper "


inputWrapperStyles : List ( String, String )
inputWrapperStyles =
    [ ( "display", "flex" )
    , ( "flex", "1" )
    , ( "flex-direction", "row" )
    , ( "flex-wrap", "wrap" )
    ]


inputClass : String
inputClass =
    "elm-select-input input"


inputStyles : List ( String, String )
inputStyles =
    []


multiInputItemContainerClass : String
multiInputItemContainerClass =
    "elm-select-multi-input-item-container "


multiInputItemContainerStyles : List ( String, String )
multiInputItemContainerStyles =
    [ ( "display", "flex" )
    , ( "flex-direction", "row" )
    , ( "align-items", "center" )
    , ( "justify-content", "center" )
    ]


multiInputItemClass : String
multiInputItemClass =
    "elm-select-multi-input-item "


multiInputItemStyles : List ( String, String )
multiInputItemStyles =
    [ ( "display", "flex" )
    , ( "border-width", "0.1rem" )
    , ( "border-radius", "0.2em" )
    , ( "border-color", "#E3E5E8" )
    , ( "background-color", "#E3E5E8" )
    , ( "font-size", ".75rem" )
    , ( "margin-right", ".2rem" )
    ]


multiInputItemText : List ( String, String )
multiInputItemText =
    [ ( "text-overflow", "ellipsis" )
    , ( "padding-left", ".5rem" )
    , ( "padding-right", ".3rem" )
    , ( "padding-top", ".05rem" )
    , ( "padding-bottom", ".05rem" )
    , ( "display", "none" )
    ]


multiInputRemoveItem : List ( String, String )
multiInputRemoveItem =
    [ ( "display", "flex" )
    , ( "alignItems", "center" )
    , ( "justifyContent", "center" )
    , ( "padding-right", ".1rem" )
    ]



-- UNDERLINE


underlineStyles : List ( String, String )
underlineStyles =
    []



-- ITEM CONSTANTS


menuItemStyles : List ( String, String )
menuItemStyles =
    [ ( "cursor", "pointer" )
    ]



-- CLEAR CONSTANTS
-- MENU CONSTANTS


visibleMenuStyles : List ( String, String )
visibleMenuStyles =
    [ ( "position", "absolute" ) ]


hiddenMenuStyles : List ( String, String )
hiddenMenuStyles =
    [ ( "display", "none" ) ]



-- REMOVE ITEM CONSTANTS


removeItemSvgStyles : List ( String, String )
removeItemSvgStyles =
    [ ( "cursor", "pointer" )
    ]
