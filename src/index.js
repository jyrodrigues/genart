'use strict'

// Import index.html so it gets copied to dist
//import "./index.html";
import "./RangeInput.css";
import "./Transitions.css";
import { Elm } from "./Main.elm";
import { downloadSvgAsJpeg, saveSvg } from "./ImageDownload.js"



/**
 * STORAGE
 */

var storageKey = "genart";
var encodedStorage = localStorage.getItem(storageKey);
var storage;

try {
    storage = JSON.parse(encodedStorage);
} catch (e) {
    console.log("Error while parsing localStorage[" + storageKey + "] = " + encodedStorage, e);
}



/**
 * INIT
 */

var elmApp = Elm.Main.init({
    node: document.getElementById('elm'),
    flags: storage || {},
});



/**
 * PORTS
 */

elmApp.ports.saveEncodedModelToLocalStorage.subscribe(function(encodedModel) {
    localStorage.setItem(storageKey, JSON.stringify(encodedModel));
});

elmApp.ports.downloadSvg.subscribe(function() {
    //saveSvg(document.getElementById("MainSVG"), "hybridcode.svg");
    saveSvg(document.getElementById("MainSVG"), "colorwheel.svg");
});

elmApp.ports.downloadSvgAsJpeg.subscribe(function() {
    var svgElement = document.getElementById("MainSVG");
    downloadSvgAsJpeg(svgElement, "colorwheel.jpeg");
});

elmApp.ports.requestFullscreen.subscribe(function() {
    var mainImage = document.getElementById("MainImgKeyedWrapper");
    mainImage.requestFullscreen();
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
 * MIDI INTEROP (*not* Firefox friendly)
 *
 * https://webaudio.github.io/web-midi-api/
 */

var midiOptions = {
    sysex: true, // allow setPadColor
    software: false,
};

if (navigator.requestMIDIAccess) {
    navigator.requestMIDIAccess(midiOptions)
        .then(onMIDISuccess, onMIDIFailure);
} else {
    console.log("Open this page on a MIDI compatible browser to use a MIDI device to control the app.");
    console.log("Check here for compatibility: https://caniuse.com/#feat=mdn-api_midiaccess");
}
console.log("Currently only MINILAB MK2 is supported for MIDI input and control.");

function onMIDISuccess(midiAccess) {
    console.log("MIDI Access Granted: midiAccess obj =", midiAccess);

    for (var input of midiAccess.inputs.values()) {
        input.onmidimessage = getMIDIMessage;
    }

    var firstOutput = midiAccess.outputs.values().next().value;
    for (var i = 0; i < 16; i++) {
        firstOutput.send(setPadColor(i, PAD_COLORS[i]));
    }
}

function getMIDIMessage(midiMessage) {
    console.log(midiMessage.data);
    elmApp.ports.midiEvent.send(midiMessage);
}

function onMIDIFailure() {
    console.log('Could not access your MIDI devices.');
}

// Set single pad color via SysEx.
function setPadColor(pad, color) {
	let padHex = (112 + pad)//.toString(16);
	return new Uint8Array( [ 0xF0, 0x00, 0x20, 0x6B, 0x7F, 0x42, 0x02, 0x00, 0x10, padHex, color, 0xF7 ] );
}

/**
 * Constants from https://github.com/chrenn/minilab-mk2-bitwig/blob/master/minilab-mk2.control.js
 *
 * Originally (and probably) from
 * https://www.untergeek.de/2014/11/taming-arturias-beatstep-sysex-codes-for-programming-via-ipad/
 *
 * More stuff
 * https://answers.bitwig.com/questions/16952/arturia-minilab-mk2-script-need-some-help
 *
 *
 * Other configs present in `minilab-mk2.control.js`
 * host.defineController("Arturia", "MiniLab Mk II", "1.0", "2fdbbb1f-f771-43f7-83e8-bc01c3e573de", "bequadro");
 * khost.defineMidiPorts(1, 1);
 * host.addDeviceNameBasedDiscoveryPair(["Arturia MiniLab mkII"], ["Arturia MiniLab mkII"]);
 */
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

const COLOR = {
	BLACK: 0x00,
	RED: 0x01,
	BLUE: 0x10,
	GREEN: 0x04,
	CYAN: 0x14,
	PURPLE: 0x11,
	YELLOW: 0x05,
	WHITE: 0x7F
};

const PAD_COLORS = [
        // Video Controls
	COLOR.WHITE,
	COLOR.WHITE,
        // HSL
	COLOR.YELLOW,
	COLOR.YELLOW,
	COLOR.YELLOW,
        // RGB
	COLOR.RED,
	COLOR.GREEN,
	COLOR.BLUE,
        // OTHER
	COLOR.BLACK,
	COLOR.RED,
	COLOR.GREEN,
	COLOR.YELLOW,
	COLOR.BLUE,
	COLOR.PURPLE,
	COLOR.CYAN,
	COLOR.WHITE
];



/**
 * SET LOOCAL STORAGE
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
