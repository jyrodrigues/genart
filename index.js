'use strict'

// Require index.html so it gets copied to dist
require("./index.html");

var Elm = require("./Main.elm");
var mount_node = document.getElementById("main");

var app = Elm.Main.embed(mount_node);