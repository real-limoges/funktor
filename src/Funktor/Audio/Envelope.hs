module Funktor.Audio.Envelope
    ( EnvelopeParams(..)
    , defaultEnvelope
    , envelopeAmplitude
    ) where

data EnvelopeParams = EnvelopeParams
    { envAttack  :: !Double
    , envDecay   :: !Double
    , envSustain :: !Double
    , envRelease :: !Double
    } deriving (Eq, Show)

defaultEnvelope :: EnvelopeParams
defaultEnvelope = EnvelopeParams
    { envAttack  = 0.01
    , envDecay   = 0.1
    , envSustain = 0.7
    , envRelease = 0.3
    }

envelopeAmplitude :: EnvelopeParams -> Double -> Maybe Double -> Double -> Double
envelopeAmplitude params noteOnTime maybeNoteOffTime currentTime
    | currentTime < noteOnTime = 0
    | otherwise = case maybeNoteOffTime of
        Nothing -> amplitudeBeforeRelease params noteOnTime currentTime
        Just noteOffTime -> amplitudeWithRelease params noteOnTime noteOffTime currentTime
  where
    -- | Helper for the case where the note has not been released yet.
    amplitudeBeforeRelease :: EnvelopeParams -> Double -> Double -> Double
    amplitudeBeforeRelease p on t
        | t < attackEnd = attackAmp p on t
        | t < decayEnd  = decayAmp p on t
        | otherwise     = envSustain p
      where
        attackEnd = on + envAttack p
        decayEnd  = attackEnd + envDecay p

    -- | Helper for the case where the note has been released.
    amplitudeWithRelease :: EnvelopeParams -> Double -> Double -> Double -> Double
    amplitudeWithRelease p on off t
        | t < attackEnd = attackAmp p on t
        | t < decayEnd  = decayAmp p on t
        | t < off        = envSustain p
        | t < relEnd    = releaseAmp p on off t
        | otherwise     = 0
      where
        attackEnd = on + envAttack p
        decayEnd  = attackEnd + envDecay p
        relStart  = max off (on + envAttack p + envDecay p)
        relEnd    = relStart + envRelease p

    -- | Attack phase amplitude (linear rise from 0 to 1).
    attackAmp :: EnvelopeParams -> Double -> Double -> Double
    attackAmp p on t = (t - on) / envAttack p

    -- | Decay phase amplitude (linear fall from 1 to sustain level).
    decayAmp :: EnvelopeParams -> Double -> Double -> Double
    decayAmp p on t =
        1 - (1 - envSustain p) * ((t - (on + envAttack p)) / envDecay p)

    -- | Release phase amplitude (linear fall from sustain to 0).
    releaseAmp :: EnvelopeParams -> Double -> Double -> Double -> Double
    releaseAmp p on off t =
        let relStart = max off (on + envAttack p + envDecay p)
            relDur   = t - relStart
        in envSustain p * (1 - relDur / envRelease p)
