'use strict'

// Import index.html so it gets copied to dist
import "./index.html";
import { Elm } from "./Main.elm";

var storageLatestKey = 'genart/v0.3/state';

var storage =
    { v2: JSON.parse(localStorage.getItem('genart/v0.2/state'))
    , latest: JSON.parse(localStorage.getItem(storageLatestKey))
    }


var app = Elm.Main.init({
    node: document.getElementById('elm'),
    flags: storage || {},
});

app.ports.saveEncodedModelToLocalStorage.subscribe(function(encodedModel) {
    localStorage.setItem(storageLatestKey, JSON.stringify(encodedModel));
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

app.ports.downloadSvg.subscribe(function() {
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
