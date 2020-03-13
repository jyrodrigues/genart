'use strict'

// Import index.html so it gets copied to dist
import "./index.html";
import { Elm } from "./Main.elm";

var storageKeys =
    { v2: 'genart/v0.2/state'
    , latest: 'genart/v0.3/state'
    };

var storage = mapValue(storageKeys, function(key) {
    var parsedItem = "";

    try {
        parsedItem = JSON.parse(localStorage.getItem(key));
    } catch (e) {
        console.log("Error while parsing localStorage[" + key + "]", e);;
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

function both() {
    v2();
    v3();
}

none();
v2();
v3();
both();


 *
 *
 */
