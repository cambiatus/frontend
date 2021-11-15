module Book.Helpers exposing (mockTranslators)

import Session.Shared as Shared


mockTranslators : Shared.Translators
mockTranslators =
    { t = identity, tr = \x _ -> x }
