require("./styles/main.scss");

// inject bundled Elm app into div#main
var Elm = require("../elm/Main");
var app = Elm.Main.embed( document.getElementById("main" ));

app.ports.play.subscribe(play);
