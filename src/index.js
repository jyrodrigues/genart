'use strict'

// Import index.html so it gets copied to dist
import "./index.html";
//import "./js-libs/WebMIDIAPI.min.js"
import { Elm } from "./Main.elm";

var storageKeys =
    { v2: 'genart/v0.2/state'
    , latest: 'genart/v0.3/state'
    };

var storage = mapValue(storageKeys, function(key) {
    var item = localStorage.getItem(key);
    var parsedItem = "";

    try {
        parsedItem = JSON.parse(item);
    } catch (e) {
        console.log("Error while parsing localStorage[" + key + "] = " + item, e);
    }

    return parsedItem;
});

var elmApp = Elm.Main.init({
    node: document.getElementById('elm'),
    flags: storage || {},
});

elmApp.ports.saveEncodedModelToLocalStorage.subscribe(function(encodedModel) {
    localStorage.setItem(storageKeys.latest, JSON.stringify(encodedModel));
});

// This is used only once!
elmApp.ports.saveMergedModelsVersionsAndDeleteOldOnes.subscribe(function(encodedMergedModel) {
    localStorage.setItem(storageKeys.latest, JSON.stringify(encodedMergedModel));

    //localStorage.removeItem(storageKeys.v1)
    localStorage.removeItem(storageKeys.v2)
});

// As per https://stackoverflow.com/a/46403589
function saveSvg(svgEl, name) {
    svgEl.setAttribute("xmlns", "http://www.w3.org/2000/svg");
    var svgData = svgEl.outerHTML;
    var preface = '<?xml version="1.0" standalone="no"?>\r\n';
    var svgBlob = new Blob([preface, svgData], {type:"image/svg+xml;charset=utf-8"});
    var svgUrl = URL.createObjectURL(svgBlob);
    var downloadLink = document.createElement("a");
    downloadLink.href = svgUrl;
    downloadLink.download = name;
    document.body.appendChild(downloadLink);
    downloadLink.click();
    document.body.removeChild(downloadLink);
}

elmApp.ports.downloadSvg.subscribe(function() {
    saveSvg(document.getElementById("MainSVG"), "hybridcode.svg");
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

/**
 * MIDI stuff.
 *
 * Constants from https://github.com/chrenn/minilab-mk2-bitwig/blob/master/minilab-mk2.control.js
 *
 * host.defineController("Arturia", "MiniLab Mk II", "1.0", "2fdbbb1f-f771-43f7-83e8-bc01c3e573de", "bequadro");
 * khost.defineMidiPorts(1, 1);
 * host.addDeviceNameBasedDiscoveryPair(["Arturia MiniLab mkII"], ["Arturia MiniLab mkII"]);
 */


// MIDI messages.
const MIDI_STATUS_PAD_ON = 153;
const MIDI_STATUS_PAD_OFF = 137;
const MIDI_STATUS_KNOBS = 176;
const MIDI_DATA1_PAD_OFFSET = 36;

// Knobs mapping (MIDI data1).
const KNOB1 = 112;
const KNOB1_CLICK = 113;
const KNOB9 = 114;
const KNOB9_CLICK = 115;
const KNOBS_LEFT_ = [74, 71, 76, 18, 19, 16];
const KNOBS_RIGHT = [77, 93, 73, 75, 17, 91, 79, 72];

// MiniLab colors.
const COLOR = {
	BLACK: "00",
	RED: "01",
	BLUE: "10",
	GREEN: "04",
	CYAN: "14",
	PURPLE: "11",
	YELLOW: "05",
	WHITE: "7F"
};

// Set single pad color via SysEx.
function setPadColor(pad, color) {
	let padHex = (112 + pad).toString(16);
	sendSysex("F0 00 20 6B 7F 42 02 00 10 " + padHex + " " + color + " F7");
}

// Pad color mapping. Notes = white, Control = color.
const PAD_COLORS = [
	COLOR.WHITE,
	COLOR.WHITE,
	COLOR.WHITE,
	COLOR.WHITE,
	COLOR.WHITE,
	COLOR.WHITE,
	COLOR.WHITE,
	COLOR.WHITE,
	COLOR.WHITE,
	COLOR.WHITE,
	COLOR.BLACK,
	COLOR.BLACK,
	COLOR.CYAN,
	COLOR.GREEN,
	COLOR.YELLOW,
	COLOR.RED
];



navigator.requestMIDIAccess()
    .then(onMIDISuccess, onMIDIFailure);

function onMIDISuccess(midiAccess) {
    console.log("MIDI Access Granted: midiAccess obj =", midiAccess);
    for (var input of midiAccess.inputs.values()) {
        input.onmidimessage = getMIDIMessage;
    }
}

function getMIDIMessage(midiMessage) {
    console.log(midiMessage.data);
    elmApp.ports.midiEvent.send(midiMessage);
}

function onMIDIFailure() {
    console.log('Could not access your MIDI devices.');
}



/**
 * Helper Funcions
 *
 * Copied from Lodash.js: https://github.com/lodash/lodash/blob/d5ef31929a1262abbc75b8dadc0b6ae6e9558b5f/mapValue.js
 */

function mapValue(object, fn) {
  object = Object(object);
  var result = {};

  Object.keys(object).forEach(function(key) {
    result[key] = fn(object[key], key, object);
  })

  return result;
}

/**
 * Copy and paste on browser to
 *
 *

var storageKeys =
    { v2: 'genart/v0.2/state'
    , latest: 'genart/v0.3/state'
    };

function none() {
    localStorage.removeItem(storageKeys.v2);
    localStorage.removeItem(storageKeys.latest);
}

function v2() {
    localStorage.setItem(
        storageKeys.v2,
        "{\"state\":[\"LLDRDRDLDLD\"],\"gallery\":[{\"composition\":[\"LLDRDRDLDLD\",\"LLDRDRDLDLD\"],\"turnAngle\":90,\"bgColor\":\"#666666\",\"strokeColor\":\"#00b46e\"}],\"bgColor\":\"#333333\",\"strokeColor\":\"#00b46e\",\"turnAngle\":90,\"scale\":1,\"translateX\":0,\"translateY\":0}"
    );
}

function v3() {
    localStorage.setItem(
        storageKeys.latest,
        "{\"image\":{\"composition\":[\"DLDLDRRDLDLD\",\"D\"],\"turnAngle\":90,\"backgroundColor\":\"#333333\",\"strokeColor\":\"#00b46e\",\"translateX\":-108.83347826086958,\"translateY\":-338.8206521739131,\"scale\":0.3699999999999997},\"gallery\":[{\"composition\":[\"DLDLDRRDLDLD\",\"DLDLDRRDLDLD\"],\"turnAngle\":90,\"backgroundColor\":\"#333333\",\"strokeColor\":\"#26b460\",\"translateX\":46.26324691231233,\"translateY\":-65.65293135386595,\"scale\":0.1}]}"
    );
}

function rubbish() {
  localStorage.setItem(storageKeys.v2, "a");
  localStorage.setItem(storageKeys.latest, "a");
}

none();
v2();
v3();
rubbish();


 *
 *
 */
