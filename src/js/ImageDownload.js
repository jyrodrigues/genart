'use strict'

/**
 * SVG DOWNLOAD
 *
 * https://stackoverflow.com/a/46403589
 */

export function saveSvg(svgEl, name) {
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
 * JPEG DOWNLOAD
 *
 */

export function downloadSvgAsJpeg(svgElement, filename) {
    var canvas = document.createElement('canvas');
    svgToCanvas(svgElement, canvas, function() {
        downloadCanvas(canvas, filename);
    });
}

/** SVG TO CANVAS - Extracted from http://www.graphicalweb.org/2010/papers/62-From_SVG_to_Canvas_and_Back/#svg_to_canvas */

// TODO Add size parameter to define downloaded image size
//function svgToCanvas(svgOriginalElement, canvasElement, callback, size) {
function svgToCanvas(svgOriginalElement, canvasElement, callback) {
    var logPrefix = "Function svgToCanvas: ";

    if (!svgOriginalElement) {
        console.warn(logPrefix + "No SVG element provided.", svgOriginalElement);
        return;
    }

    if (!canvasElement) {
        console.warn(logPrefix + "No Canvas element provided.", canvasElement);
        return;
    }

    var { svgElement, width, height } = cloneSvgAndFixDimensions(svgOriginalElement);

    // https://developer.mozilla.org/en/XMLSerializer
    var svg_xml = (new XMLSerializer()).serializeToString(svgElement);
    // This is just a JavaScript (HTML) image.
    var img = new Image();
    // http://en.wikipedia.org/wiki/SVG#Native_support
    // https://developer.mozilla.org/en/DOM/window.btoa
    img.src = "data:image/svg+xml;base64," + btoa(svg_xml);

    var ctx = canvasElement.getContext('2d');
    // TODO test if this will always work (using boundingClientRect).
    ctx.canvas.width = width;
    ctx.canvas.height = height;

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

    var anchor = document.createElement('a');

    anchor.href = canvasElement.toDataURL("image/jpeg", 0.9);
    anchor.download = filename;

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
}

function cloneSvgAndFixDimensions(svgOriginalElement) {
    // TODO test if element is an SVGElement or something in those lines.

    var boundingClientRect = svgOriginalElement.getBoundingClientRect();

    // Cloning so that we don't mess with the original element.
    // We may change the width and height.
    var deepClone = true;
    var svgElement = svgOriginalElement.cloneNode(deepClone);

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
        var heightUnitType = svgElement.height.baseVal.unitType;
        if (heightUnitType === SVGLength.SVG_LENGTHTYPE_UNKNOWN
            || heightUnitType === SVGLength.SVG_LENGTHTYPE_PERCENTAGE
        ) {
            svgElement.setAttribute('height', svgBoundingBoxSize.height);
        }
    } catch (e) {
        //console.warn(logPrefix + "This browser doesn't have support for the SVGLength interface", e);

        /**
         * Test if height is set in percentage (e.g. "80%"); if so then change to absolute
         * boundingClientRect height.
         *
         * It's possible to use this test only (instead of the method above) but it's VERY
         * informative the other method and more involved in Web standards, something I
         * think every developer should know more about. That's the reason we keep it there.
         */
        var regexPercentageValue = RegExp("^([0-9]+|[0-9]*\.[0-9]+)%$");
        if (regexPercentageValue.test(svgElement.attributes.width.value)) {
            svgElement.setAttribute("width", boundingClientRect.width);
        }
        if (regexPercentageValue.test(svgElement.attributes.height.value)) {
            svgElement.setAttribute("height",boundingClientRect.height);
        }
    }

    return {
        svgElement: svgElement,
        width: boundingClientRect.width,
        height: boundingClientRect.height,
    };
}
