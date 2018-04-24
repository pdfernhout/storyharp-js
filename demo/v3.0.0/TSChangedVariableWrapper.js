define(["require", "exports"], function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TSChangedVariableWrapper {
        constructor(variable, newState) {
            this.variable = variable;
            this.newState = newState;
            this.oldState = variable.getState();
        }
        doChange() {
            this.variable.setState(this.newState);
        }
        undoChange() {
            this.variable.setState(this.oldState);
        }
    }
    exports.TSChangedVariableWrapper = TSChangedVariableWrapper;
});
