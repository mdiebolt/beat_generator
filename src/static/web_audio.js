window.onload = init;

var context, bufferLoader, instruments, instrumentsLookup;

function init() {
  try {
    context = new AudioContext();
  } catch (e) {
    alert("Web Audio API is not supported in this browser");
  }

  instruments = [
    "static/sounds/hihat.wav",
    "static/sounds/snare.wav",
    "static/sounds/kick.wav",
    "static/sounds/metronome_low.wav",
    "static/sounds/tom1.wav",
    "static/sounds/tom2.wav",
    "static/sounds/tom3.wav",
    "static/sounds/tom4.wav"
  ];

  // Start loading the drum kit.
  bufferLoader = new BufferLoader(
    new AudioContext(),
    instruments,
    bufferLoadCompleted
  );

  bufferLoader.load();
}

function playSound(instrument, time, accent) {
  var source = context.createBufferSource();
  source.buffer = instrument;

  if (accent) {
    gainNode = context.createGain();
    gainNode.gain.value = 1;
    source.connect(gainNode);
    gainNode.connect(context.destination);
  } else {
    gainNode = context.createGain();
    gainNode.gain.value = 0.5;
    source.connect(gainNode);
    gainNode.connect(context.destination);
  }

  source.start(time);
}

function play(model) {
  var beat = model[0];
  var tempo = model[1];
  var hasMetronome = model[2];

  // Start playing the rhythm 100 milliseconds from "now"
  var startTime = context.currentTime + 0.1;

  var quarterNoteTime = 60 / tempo;
  var sixteenthNoteTime = quarterNoteTime / 4;

  var offset = 0;
  beat.forEach(function(channel, i) {
    var instrumentName = channel[0].toLowerCase();
    var sound = instrumentsLookup[instrumentName];

    channel[1].forEach(function(note, j) {
      var time = startTime + j * sixteenthNoteTime;

      if (note === "x") {
        playSound(sound, time, false);
      }

      if (note === ">") {
        playSound(sound, time, true);
      }
    });
  });
}

function bufferLoadCompleted(bufferList) {
  instrumentsLookup = {
    hihat: bufferList[0],
    snare: bufferList[1],
    kick: bufferList[2],
    metronome: bufferList[3],
    tom1: bufferList[4],
    tom2: bufferList[5],
    tom3: bufferList[6],
    tom4: bufferList[7]
  };
}
