module Action exposing (Action)

import Cambiatus.Enum.VerificationType exposing (VerificationType)
import Cambiatus.Scalar exposing (DateTime)
import Eos exposing (Symbol)
import Eos.Account as Eos
import Markdown exposing (Markdown)
import Profile



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


type alias Objective =
    { id : ObjectiveId
    , description : Markdown
    , community : Community
    , isCompleted : Bool
    }


type ObjectiveId
    = ObjectiveId Int
