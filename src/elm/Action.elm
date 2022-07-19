module Action exposing
    ( Action
    , Community
    , Objective
    , ObjectiveId
    , isClosed
    , isPastDeadline
    , objectiveIdFromInt
    , objectiveIdToInt
    , selectionSet
    )

import Cambiatus.Enum.VerificationType exposing (VerificationType)
import Cambiatus.Object
import Cambiatus.Object.Action as ActionObject
import Cambiatus.Object.Community
import Cambiatus.Object.Objective
import Cambiatus.Scalar exposing (DateTime)
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html.Attributes exposing (id)
import Markdown exposing (Markdown)
import Profile
import Time
import Utils



-- TYPES


type alias Action =
    -- TODO - Use opaque type for id
    { id : Int
    , description : Markdown
    , image : Maybe String
    , objective : Objective
    , reward : Float
    , verifierReward : Float
    , creator : Eos.Name
    , validators : List Profile.Minimal
    , usages : Int
    , usagesLeft : Int
    , deadline : Maybe DateTime
    , verificationType : VerificationType
    , verifications : Int
    , isCompleted : Bool
    , hasProofPhoto : Bool
    , hasProofCode : Bool
    , photoProofInstructions : Maybe Markdown
    , position : Maybe Int
    , claimCount : Int
    }



-- GRAPHQL


type alias Community =
    { symbol : Symbol }


communitySelectionSet : SelectionSet Community Cambiatus.Object.Community
communitySelectionSet =
    SelectionSet.succeed Community
        |> with (Eos.symbolSelectionSet Cambiatus.Object.Community.symbol)


type alias Objective =
    { id : ObjectiveId
    , description : Markdown
    , community : Community
    , isCompleted : Bool
    }


type ObjectiveId
    = ObjectiveId Int


objectiveIdFromInt : Int -> ObjectiveId
objectiveIdFromInt =
    ObjectiveId


objectiveIdSelectionSet : SelectionSet ObjectiveId Cambiatus.Object.Objective
objectiveIdSelectionSet =
    Cambiatus.Object.Objective.id |> SelectionSet.map ObjectiveId


objectiveSelectionSet : SelectionSet Objective Cambiatus.Object.Objective
objectiveSelectionSet =
    SelectionSet.succeed Objective
        |> with objectiveIdSelectionSet
        |> with (Markdown.selectionSet Cambiatus.Object.Objective.description)
        |> with (Cambiatus.Object.Objective.community communitySelectionSet)
        |> with Cambiatus.Object.Objective.isCompleted


selectionSet : SelectionSet Action Cambiatus.Object.Action
selectionSet =
    SelectionSet.succeed Action
        |> with ActionObject.id
        |> with (Markdown.selectionSet ActionObject.description)
        |> with ActionObject.image
        |> with
            (SelectionSet.map
                (\o ->
                    { id = o.id
                    , description = o.description
                    , community = o.community
                    , isCompleted = o.isCompleted
                    }
                )
                (ActionObject.objective objectiveSelectionSet)
            )
        |> with ActionObject.reward
        |> with ActionObject.verifierReward
        |> with (Eos.nameSelectionSet ActionObject.creatorId)
        |> with (ActionObject.validators Profile.minimalSelectionSet)
        |> with ActionObject.usages
        |> with ActionObject.usagesLeft
        |> with ActionObject.deadline
        |> with ActionObject.verificationType
        |> with ActionObject.verifications
        |> with ActionObject.isCompleted
        |> with (SelectionSet.map (Maybe.withDefault False) ActionObject.hasProofPhoto)
        |> with (SelectionSet.map (Maybe.withDefault False) ActionObject.hasProofCode)
        |> with (Markdown.maybeSelectionSet ActionObject.photoProofInstructions)
        |> with ActionObject.position
        |> with (ActionObject.claimCount (\optionals -> { optionals | status = OptionalArgument.Absent }))



-- INTEROP


objectiveIdToInt : ObjectiveId -> Int
objectiveIdToInt (ObjectiveId id) =
    id



-- HELPERS


isPastDeadline : Action -> Time.Posix -> Bool
isPastDeadline action now =
    case action.deadline of
        Just _ ->
            Time.posixToMillis now > Time.posixToMillis (Utils.fromMaybeDateTime action.deadline)

        Nothing ->
            False


isClosed : Action -> Time.Posix -> Bool
isClosed action now =
    isPastDeadline action now
        || (action.usages > 0 && action.usagesLeft == 0)
