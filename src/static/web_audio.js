// TODO: metronome
window.onload = init;

var context, gain, bufferLoader, instruments;

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
        "static/sounds/kick.wav"
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

    // We'll start playing the rhythm 100 milliseconds from "now"
    var startTime = context.currentTime + 0.100;

    var quarterNoteTime = 60 / tempo;
    var sixteenthNoteTime = quarterNoteTime / 4;

    beat.forEach(function(channel, i) {
        var sound = bufferLoader.bufferList[i]

        channel.forEach(function(note, j) {
            if (note === "x") {
                playSound(sound, startTime + j * sixteenthNoteTime, false)
            }

            if (note === ">") {
                playSound(sound, startTime + j * sixteenthNoteTime, true)
            }
        })
    })
}

function bufferLoadCompleted() {}
