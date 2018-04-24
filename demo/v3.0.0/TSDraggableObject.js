define(["require", "exports", "./TPoint", "./TRect", "./common"], function (require, exports, TPoint_1, TRect_1, common_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    let nextDraggableObjectUUID = 1;
    class TSDraggableObject {
        constructor() {
            this.uuid = nextDraggableObjectUUID++;
            this.position = new TPoint_1.TPoint();
            this.extent = new TPoint_1.TPoint();
            this.selected = false;
        }
        displayName() {
            return "Error - override needed";
        }
        setPosition(value) {
            const [firstNumber, secondNumber] = value.split(",");
            try {
                this.position.X = common_1.StrToInt(firstNumber);
                this.position.Y = common_1.StrToInt(secondNumber);
            }
            catch (e) {
                console.log("setPosition exception", e);
            }
        }
        bounds() {
            const topLeft = new TPoint_1.TPoint(this.position.X - this.extent.X / 2, this.position.Y - this.extent.Y / 2);
            return new TRect_1.TRect(topLeft.X, topLeft.Y, topLeft.X + this.extent.X, topLeft.Y + this.extent.Y);
        }
        center() {
            return this.position.copy();
        }
    }
    exports.TSDraggableObject = TSDraggableObject;
});
