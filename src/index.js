'use strict'

// Import index.html so it gets copied to dist
import "./index.html";
import { Elm } from "./Main.elm";

var storageName = 'genart/v0.1/state';

var app = Elm.Main.init({
    node: document.getElementById('elm'),
    flags: JSON.parse(localStorage.getItem(storageName)) || []
});

app.ports.saveStateToLocalStorage.subscribe(function(stateAsListOfStrings) {
    localStorage.setItem(storageName, JSON.stringify(stateAsListOfStrings));
});

/**
 * Hackery to prevent going back in history with `backspace` press.
 */

var turnAngleInput;

document.onkeydown = (e) => {
    if (!turnAngleInput) {
        turnAngleInput = document.getElementById("TurnAngle");
    }

    if (e.key === "Backspace" && document.activeElement !== turnAngleInput) {
        e.preventDefault();
    }
}
