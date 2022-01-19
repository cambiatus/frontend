module View.Select.Messages exposing (Msg(..))


type Msg item
    = NoOp
    | OnFocus
    | OnBlur
    | OnRemoveItem item
    | OnEsc
    | OnDownArrow
    | OnUpArrow
    | OnResetFocusToFirstItem
    | OnQueryChange String
    | OnSelect item
