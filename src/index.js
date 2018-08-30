'use strict'

// Require index.html so it gets copied to dist
require("./index.html");

var Elm = require("./Main.elm");

console.log(Elm)
var app = Elm.Elm.Main.init({
    node: document.getElementById('elm')
});