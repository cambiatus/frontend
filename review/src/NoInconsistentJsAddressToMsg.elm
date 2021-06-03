module NoInconsistentJsAddressToMsg exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Case, Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Rule)


{-| Reports when a `jsAddressToMsg` function case comparison doesn't start with
a valid `Msg` name

    config =
        [ NoInconsistentJsAddressToMsg.rule
        ]


## Fail

    type Msg
        = Apples
        | Bananas

    jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
    jsAddressToMsg addr val =
        case addr of
            "Oranges" :: _ ->
                Nothing

            "Bananas" :: _ ->
                Nothing


## Success

    type Msg
        = Apples
        | Bananas

    jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
    jsAddressToMsg addr val =
        case addr of
            "Apples" :: _ ->
                Nothing

            "Bananas" :: _ ->
                Nothing

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoInconsistentJsAddressToMsg" []
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
        |> Rule.fromModuleRuleSchema


type alias MsgNames =
    List String


declarationListVisitor : List (Node Declaration) -> MsgNames -> ( List (Rule.Error {}), MsgNames )
declarationListVisitor nodes _ =
    nodes
        |> List.filterMap
            (\node ->
                case Node.value node of
                    CustomTypeDeclaration { name, constructors } ->
                        case Node.value name of
                            "Msg" ->
                                constructors
                                    |> List.map (Node.value >> .name >> Node.value)
                                    |> Just

                            _ ->
                                Nothing

                    _ ->
                        Nothing
            )
        |> List.concat
        |> Tuple.pair []


declarationEnterVisitor : Node Declaration -> MsgNames -> ( List (Rule.Error {}), MsgNames )
declarationEnterVisitor node msgNames =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            let
                functionName =
                    declaration
                        |> Node.value
                        |> .name
                        |> Node.value

                expression =
                    declaration
                        |> Node.value
                        |> .expression
                        |> Node.value
            in
            if functionName == "jsAddressToMsg" then
                case expression of
                    CaseExpression { cases } ->
                        cases
                            |> List.map (caseVisitor msgNames)
                            |> List.concat
                            |> (\errors -> ( errors, msgNames ))

                    _ ->
                        ( [], msgNames )

            else
                ( [], msgNames )

        _ ->
            ( [], msgNames )


caseVisitor : MsgNames -> Case -> List (Rule.Error {})
caseVisitor msgNames ( patternNode, _ ) =
    case Node.value patternNode of
        UnConsPattern firstPattern _ ->
            case Node.value firstPattern of
                StringPattern stringPattern ->
                    validateMsgName msgNames stringPattern (Node.range firstPattern)

                _ ->
                    []

        ListPattern patterns ->
            case List.head patterns of
                Just firstPattern ->
                    case Node.value firstPattern of
                        StringPattern stringPattern ->
                            validateMsgName msgNames stringPattern (Node.range firstPattern)

                        _ ->
                            []

                Nothing ->
                    []

        _ ->
            []


validateMsgName : MsgNames -> String -> Range -> List (Rule.Error {})
validateMsgName msgNames stringPattern range =
    if List.member stringPattern msgNames then
        []

    else
        [ Rule.error
            { message = "There is no `Msg` called `" ++ stringPattern ++ "`"
            , details =
                [ "The first element should be a valid `Msg` name"
                , "Make sure this name represents a valid `Msg` constructor from this module"
                , "Maybe you meant one of these:\n\n"
                    ++ (msgNames
                            |> List.map (\msgName -> "\t" ++ msgName)
                            |> String.join "\n"
                       )
                    ++ "\n"
                ]
            }
            range
        ]
