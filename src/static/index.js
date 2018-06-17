require("./styles/main.scss");

var Elm = require("../elm/Main");
var $main = document.getElementById("main");
var app = Elm.Main.embed($main);

app.ports.play.subscribe(play);
