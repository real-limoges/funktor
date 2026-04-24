module Funktor.Audio.Envelope
    ( EnvelopeParams (..)
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
    | otherwise =
        let attackEnd = noteOnTime + envAttack params
            decayEnd = attackEnd + envDecay params
            releaseDur = envRelease params
        in if currentTime < attackEnd
               then let t = (currentTime - noteOnTime) / envAttack params
                    in t
               else case maybeNoteOffTime of
                   Nothing ->
                       if currentTime < decayEnd
                           then let t = (currentTime - attackEnd) / envDecay params
                                in 1 - (1 - envSustain params) * t
                           else envSustain params
                   Just noteOffTime ->
                       let relStart = max noteOffTime (noteOnTime + envAttack params + envDecay params)
                           relDur = currentTime - relStart
                           relEnd = relStart + releaseDur
                       in if currentTime < noteOffTime
                              then if currentTime < decayEnd
                                       then let t = (currentTime - attackEnd) / envDecay params
                                            in 1 - (1 - envSustain params) * t
                                       else envSustain params
                              else if currentTime < relEnd
                                       then let t = relDur / releaseDur
                                            in envSustain params * (1 - t)
                                       else 0
