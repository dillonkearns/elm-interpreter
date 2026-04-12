module NormalizationFlags exposing (NormalizationFlags, all, none)

{-| Feature flags for individual normalization passes.
Toggle independently to measure impact of each optimization.
-}


type alias NormalizationFlags =
    { foldConstantApplications : Bool
    , inlinePrecomputedRefs : Bool
    , inlineFunctions : Bool
    }


all : NormalizationFlags
all =
    { foldConstantApplications = True
    , inlinePrecomputedRefs = True
    , inlineFunctions = True
    }


none : NormalizationFlags
none =
    { foldConstantApplications = False
    , inlinePrecomputedRefs = False
    , inlineFunctions = False
    }
