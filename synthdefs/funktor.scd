// Funktor SynthDef. Evaluate this whole block once in SuperCollider IDE
// (place cursor anywhere in the outer parens, Cmd/Ctrl+Enter). The server
// boots on the default port (57110); after "Funktor: synthdef loaded" prints,
// Haskell-side `play` will work.

(
s = Server.local;
s.options.numOutputBusChannels = 2;
s.waitForBoot({
    SynthDef(\funktor_note, { |freq=440, amp=0.5, gate=1,
                              attack=0.01, decay=0.1, sustain=0.7, release=0.3,
                              wave=0, cutoff=2000|
        var sig, env;
        env = EnvGen.kr(
            Env.adsr(attack, decay, sustain, release),
            gate,
            doneAction: 2
        );
        sig = Select.ar(wave.asInteger.clip(0, 3), [
            SinOsc.ar(freq),
            Saw.ar(freq),
            Pulse.ar(freq, 0.5),
            LFTri.ar(freq)
        ]);
        sig = LPF.ar(sig, cutoff);
        Out.ar(0, (sig * amp * env).dup);
    }).add;
    "Funktor: synthdef loaded, server ready on port 57110".postln;
});
)
