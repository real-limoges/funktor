module Funktor.Audio.Envelope (
    EnvelopeParams (..),
    defaultEnvelope,
    envelopeAmplitude,
) where

data EnvelopeParams = EnvelopeParams
    { envAttack :: !Double
    , envDecay :: !Double
    , envSustain :: !Double
    , envRelease :: !Double
    }
    deriving (Eq, Show)

defaultEnvelope :: EnvelopeParams
defaultEnvelope = EnvelopeParams 0.01 0.1 0.7 0.3

envelopeAmplitude :: EnvelopeParams -> Double -> Maybe Double -> Double -> Double
envelopeAmplitude (EnvelopeParams a d s r) on offMaybe t
    | t < on = 0
    | t < attackEnd = (t - on) / a
    | t < decayEnd = 1 - (1 - s) * ((t - attackEnd) / d)
    | Just off <- offMaybe, t >= off =
        let relStart = max off decayEnd
            relEnd = relStart + r
         in if t < relEnd then s * (1 - (t - relStart) / r) else 0
    | otherwise = s
  where
    attackEnd = on + a
    decayEnd = attackEnd + d
