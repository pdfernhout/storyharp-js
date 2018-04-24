define(["require", "exports", "./TPoint"], function (require, exports, TPoint_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TSDragRecord {
        constructor(node) {
            this.draggedNode = node;
            this.originalLocation = this.draggedNode.position.copy();
            this.newLocation = this.originalLocation.copy();
        }
        doDrag() {
            this.draggedNode.position = this.newLocation.copy();
        }
        undoDrag() {
            this.draggedNode.position = this.originalLocation.copy();
        }
        offset(delta) {
            this.newLocation = new TPoint_1.TPoint(this.newLocation.X + delta.X, this.newLocation.Y + delta.Y);
            this.draggedNode.position = this.newLocation.copy();
        }
    }
    exports.TSDragRecord = TSDragRecord;
});
