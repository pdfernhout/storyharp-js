define(["require", "exports"], function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    const css = `
.w-3 { width: 3%; }
.w-15 { width: 15%; }
.w-17 { width: 17%; }
.us-none { "user-select: none" }
`;
    function addStylesBeyondTachyons() {
        const style = document.createElement('style');
        style.type = 'text/css';
        style.appendChild(document.createTextNode(css));
        document.head.appendChild(style);
    }
    exports.addStylesBeyondTachyons = addStylesBeyondTachyons;
});
