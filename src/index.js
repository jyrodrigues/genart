'use strict'

// Import index.html so it gets copied to dist
import "./index.html";
import { Elm } from "./Main.elm";

var app = Elm.Main.init({
    node: document.getElementById('elm')
});