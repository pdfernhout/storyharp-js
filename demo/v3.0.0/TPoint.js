define(["require", "exports"], function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TPoint {
        constructor(x = 0, y = 0) {
            this.X = Math.round(x);
            this.Y = Math.round(y);
        }
        copy() {
            return new TPoint(this.X, this.Y);
        }
        scale(scale) {
            return new TPoint(this.X * scale, this.Y * scale);
        }
    }
    exports.TPoint = TPoint;
});
