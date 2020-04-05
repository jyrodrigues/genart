'use strict'

// Import index.html so it gets copied to dist
import "./index.html";
import "./colorwheel.svg";
import { Elm } from "./Main.elm";



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

elmApp.ports.downloadSvgAsPng.subscribe(function() {
    console.log(document.getElementById("MainSVG").getBoundingClientRect());
    var a = document.getElementById("MainSVG");
    console.log(a.getBoundingClientRect());
    downloadSvgAsPng(a, "colorwheel.png");
});


/**
 * SVG DOWNLOAD
 *
 * https://stackoverflow.com/a/46403589
 */

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



/**
 * PNG DOWNLOAD
 *
 */

/** SVG TO CANVAS - Extracted from http://www.graphicalweb.org/2010/papers/62-From_SVG_to_Canvas_and_Back/#svg_to_canvas */

function svgToCanvas(svgOriginalElement, canvasElement, callback, size) {
    var logPrefix = "Function svgToCanvas: ";

    if (!svgOriginalElement) {
        console.warn(logPrefix + "No SVG element provided.", svgOriginalElement);
        return;
    }

    if (!canvasElement) {
        console.warn(logPrefix + "No Canvas element provided.", canvasElement);
        return;
    }

    var deepClone = true;
    var svgElement = svgOriginalElement.cloneNode(deepClone);

    console.log(svgOriginalElement.getBoundingClientRect())
    console.log(svgElement.getBoundingClientRect())
    return;

    /**
     * Setting "absolute" size to SVG, since relative sizes (like percentage)
     * doesn't work with Firefox.
     *
     *
     * Detour: SVG Lenght geekout
     *
     * Possible CSS2 units are defined here:
     * https://www.w3.org/TR/WD-CSS2/syndata.html#length-units
     *
     * Units constants for SVGLength interface here:
     * https://www.w3.org/TR/SVG11/types.html#InterfaceSVGLength
     *
     * With almost all length types the the browser can reason about
     * the image when we insert into a canvas, but not with these:
     *
     * SVGLength.SVG_LENGTHTYPE_UNKNOWN
     * SVGLength.SVG_LENGTHTYPE_PERCENTAGE
     *
     * And then, we should replace with the given (or default) fixed size
     *
     */
    try {
        var widthUnitType = svgElement.width.baseVal.unitType;
        if (widthUnitType === SVGLength.SVG_LENGTHTYPE_UNKNOWN
            || widthUnitType === SVGLength.SVG_LENGTHTYPE_PERCENTAGE
        ) {
            svgElement.setAttribute('width', svgBoundingBoxSize.width);
        }
        // If height is relative, we make it absolute.
        var heightUnitType = svgElement.height.baseVal.unitType;
        if (heightUnitType === SVGLength.SVG_LENGTHTYPE_UNKNOWN
            || heightUnitType === SVGLength.SVG_LENGTHTYPE_PERCENTAGE
        ) {
            svgElement.setAttribute('height', svgBoundingBoxSize.height);
        }
    } catch (e) {
        console.warn(logPrefix + "This browser doesn't have support for the SVGLength interface", e);

        /**
         * Test if height is set in percentage (e.g. "80%"); if so then change to absolute
         * boundingClientRect height.
         *
         * It's possible to use this test only (instead of the method above) but it's VERY
         * informative the other method and more involved in Web standards, something I
         * think every developer should know more about. That's the reason we keep it there.
         */
        var boundingClientRect = svgElement.getBoundingClientRect();
        var regexPercentageValue = RegExp("^([0-9]+|[0-9]*\.[0-9]+)%$");
        if (regexPercentageValue.test(svgElement.attributes.width.value)) {
            svgElement.attributes.width = boundingClientRect.width;
        }
        if (regexPercentageValue.test(svgElement.attributes.height.value)) {
            svgElement.attributes.height = boundingClientRect.height;
        }
    }

    // https://developer.mozilla.org/en/XMLSerializer
    var svg_xml = (new XMLSerializer()).serializeToString(svgElement);
    var ctx = canvasElement.getContext('2d');


    ctx.canvas.width = 600;
    ctx.canvas.height = 600;

    // This is just a JavaScript (HTML) image.
    var img = new Image();
    // http://en.wikipedia.org/wiki/SVG#Native_support
    // https://developer.mozilla.org/en/DOM/window.btoa
    img.src = "data:image/svg+xml;base64," + btoa(svg_xml);

    img.onload = function() {
        // After this, Canvas’ origin-clean is DIRTY!
        ctx.drawImage(img, 0, 0);
        !!callback && callback();
    }
}

/** SET CANVAS - Extracted from https://github.com/ericdrowell/concrete/blob/master/src/concrete.js */

function downloadCanvas(canvasElement, filename) {
    var logPrefix = "Function downloadCanvas: ";

    if (!canvasElement || !canvasElement.toBlob) {
        console.warn(logPrefix + "Canvas element not supported", canvasElement);
        return;
    }

    canvasElement.toBlob(function(blob) {
        if (!filename) {
            console.log(logPrefix + "No file name specified, defaulting to \"canvas.png\".");
            filename = "canvas.png";
        }

        var anchor = document.createElement('a');
        var dataUrl = URL.createObjectURL(blob);

        anchor.setAttribute('href', dataUrl);
        anchor.setAttribute('target', '_blank');
        anchor.setAttribute('download', filename);

        if (document.createEvent) {
            var eventObject = document.createEvent('MouseEvents');
            eventObject.initEvent('click', true, true);
            anchor.dispatchEvent(eventObject);
        }
        else if (anchor.click) {
            anchor.click();
        } else {
            console.warn(logPrefix + "Download not supported. No \"document.createEvent\" or \"domElement.click\" available.");
        }
    });
}

function downloadSvgAsPng(svgElement, filename) {
    var canvas = document.createElement('canvas');
    svgToCanvas(svgElement, function() {
        downloadCanvas(canvas, filename);
    });
}



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

navigator.requestMIDIAccess(midiOptions)
    .then(onMIDISuccess, onMIDIFailure);

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
