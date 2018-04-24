define(["require", "exports"], function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    function arrayRemove(theArray, item) {
        const index = theArray.indexOf(item);
        if (index !== -1) {
            theArray.splice(index, 1);
        }
    }
    exports.arrayRemove = arrayRemove;
    function StrToInt(value) {
        return parseInt(value);
    }
    exports.StrToInt = StrToInt;
    function compareTextIgnoreCase(a, b) {
        return a.toUpperCase() === b.toUpperCase();
    }
    exports.compareTextIgnoreCase = compareTextIgnoreCase;
    var Color;
    (function (Color) {
        Color[Color["clGreen"] = 0] = "clGreen";
        Color[Color["clBlue"] = 1] = "clBlue";
        Color[Color["clRed"] = 2] = "clRed";
        Color[Color["clBlack"] = 3] = "clBlack";
    })(Color = exports.Color || (exports.Color = {}));
    function caption(text) {
        return text.replace("&", "");
    }
    exports.caption = caption;
    function expander(expanded, closedLabel = "", openLabel = "") {
        return expanded
            ? "▲" + openLabel
            : "▼" + closedLabel;
    }
    exports.expander = expander;
    var ScrollIntoViewDirection;
    (function (ScrollIntoViewDirection) {
        ScrollIntoViewDirection[ScrollIntoViewDirection["kFromBottom"] = 0] = "kFromBottom";
        ScrollIntoViewDirection[ScrollIntoViewDirection["kFromTop"] = 1] = "kFromTop";
    })(ScrollIntoViewDirection = exports.ScrollIntoViewDirection || (exports.ScrollIntoViewDirection = {}));
    function makeFileNameWithWldExtension(fileName) {
        if (!fileName.endsWith(".wld")) {
            return fileName + ".wld";
        }
        else {
            return fileName;
        }
    }
    exports.makeFileNameWithWldExtension = makeFileNameWithWldExtension;
    function makeFileNameWithoutWldExtension(fileName) {
        if (fileName.endsWith(".wld")) {
            return fileName.substring(0, fileName.length - 4);
        }
        else {
            return fileName;
        }
    }
    exports.makeFileNameWithoutWldExtension = makeFileNameWithoutWldExtension;
    function notebookTabButton(selected) {
        return "button.ml2.mt1.w-6rem.bb-0.br3.br--top.b--gray" + (selected ? ".bg-white" : "");
    }
    exports.notebookTabButton = notebookTabButton;
});
