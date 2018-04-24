define(["require", "exports", "./TPoint"], function (require, exports, TPoint_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TRect {
        constructor(Left = 0, Top = 0, Right = Left, Bottom = Top) {
            this.Left = Math.round(Left);
            this.Top = Math.round(Top);
            this.Right = Math.round(Right);
            this.Bottom = Math.round(Bottom);
        }
        get width() {
            return this.Right - this.Left;
        }
        get height() {
            return this.Bottom - this.Top;
        }
        center() {
            return new TPoint_1.TPoint(Math.round(this.Left + this.width / 2), Math.round(this.Top + this.height / 2));
        }
        intersects(otherRect) {
            return this.Left < otherRect.Right
                && this.Right > otherRect.Left
                && this.Top < otherRect.Bottom
                && this.Bottom > otherRect.Top;
        }
        contains(point) {
            return this.Left < point.X
                && this.Right > point.X
                && this.Top < point.Y
                && this.Bottom > point.Y;
        }
        copy() {
            return new TRect(this.Left, this.Top, this.Right, this.Bottom);
        }
        scale(scale) {
            return new TRect(this.Left * scale, this.Top * scale, this.Right * scale, this.Bottom * scale);
        }
    }
    exports.TRect = TRect;
});
