window.onload = init;

var context, gain, bufferLoader, instruments, instrumentsLookup;

function init() {
    try {
        context = new AudioContext();
        gain = context.createGain();
        gain.gain.value = 3;
        gain.connect(context.destination)
    }
    catch(e) {
        alert("Web Audio API is not supported in this browser");
    }

    instruments = [
        "static/sounds/hihat.wav",
        "static/sounds/snare.wav",
        "static/sounds/kick.wav",
        "static/sounds/metronome_low.wav",
        "static/sounds/metronome_high.wav"
    ]

    // Start loading the drum kit.
    bufferLoader = new BufferLoader(
        new AudioContext(), instruments, bufferLoadCompleted
    );

    bufferLoader.load();
}

function playSound(instrument, time, accent) {
    var source = context.createBufferSource();
    source.buffer = instrument;

    if (accent) {
        source.connect(gain);
    } else {
        source.connect(context.destination);
    }

    source.start(time);
}

function play(model) {
    var beat = model[0]
    var tempo = model[1]
    var hasMetronome = model[2]

    // We'll start playing the rhythm 100 milliseconds from "now"
    var startTime = context.currentTime + 0.100;

    var quarterNoteTime = 60 / tempo;
    var sixteenthNoteTime = quarterNoteTime / 4;

    beat.forEach(function(channel, i) {
        var instrumentName = channel[0].toLowerCase()
        var sound = instrumentsLookup[instrumentName]

        channel[1].forEach(function(note, j) {
            var time = startTime + j * sixteenthNoteTime

            // metronome click
            if (hasMetronome && j % 4 == 0) {
                playSound(instrumentsLookup.metronome, time, false)
            }

            if (note === "x") {
                playSound(sound, time, false)
            }

            if (note === ">") {
                playSound(sound, time, true)
            }
        })
    })
}

function bufferLoadCompleted(bufferList) {
    instrumentsLookup = {
        hihat: bufferList[0],
        snare: bufferList[1],
        kick: bufferList[2],
        metronome: bufferList[4]
    }
}
