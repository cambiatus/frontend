module NoInconsistentMsgToString exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Case, Expression(..))
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)


{-| Reports when a `msgToString` function result is inconsistent with the actual
`Msg` name

    config =
        [ NoInconsistentMsgToString.rule
        ]


## Fail

    type Msg
        = Apples
        | Bananas

    msgToString : Msg -> List String
    msgToString msg =
        case msg of
            Apples ->
                [ "Oranges" ]

            Bananas ->
                [ "Bananas" ]


## Success

    type Msg
        = Apples
        | Bananas

    msgToString : Msg -> List String
    msgToString msg =
        case msg of
            Apples ->
                [ "Apples" ]

            Bananas ->
                [ "Bananas" ]

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoInconsistentMsgToString" ()
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


declarationVisitor : Node Declaration -> List (Rule.Error {})
declarationVisitor node =
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
            if functionName == "msgToString" then
                case expression of
                    CaseExpression { cases } ->
                        cases
                            |> List.map caseVisitor
                            |> List.concat

                    _ ->
                        []

            else
                []

        _ ->
            []


caseVisitor : Case -> List (Rule.Error {})
caseVisitor ( patternNode, expressionNode ) =
    case Node.value expressionNode of
        ListExpr listExpr ->
            case List.head listExpr of
                Just firstNode ->
                    nodesVisitor patternNode firstNode

                Nothing ->
                    []

        OperatorApplication "::" Infix.Right firstOperator _ ->
            nodesVisitor patternNode firstOperator

        OperatorApplication "::" Infix.Left _ firstOperator ->
            nodesVisitor patternNode firstOperator

        _ ->
            []


nodesVisitor : Node Pattern -> Node Expression -> List (Rule.Error {})
nodesVisitor patternNode exprNode =
    case ( Node.value patternNode, Node.value exprNode ) of
        ( NamedPattern { name } _, Literal lit ) ->
            if lit == name then
                []

            else
                [ Rule.errorWithFix
                    { message = "The result should start with `" ++ name ++ "`"
                    , details = [ "Make sure the string representation of the `Msg` is equal to that `Msg`s name" ]
                    }
                    (Node.range exprNode)
                    [ Fix.replaceRangeBy (Node.range exprNode) ("\"" ++ name ++ "\"") ]
                ]

        _ ->
            []
