define(["require", "exports", "./TSVariable"], function (require, exports, TSVariable_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    let nextWrapperUUID = 1;
    class TSDesiredStateVariableWrapper {
        constructor(variable, desiredState) {
            this.uuid = nextWrapperUUID++;
            this.variable = variable;
            this.desiredState = desiredState;
        }
        leader() {
            if (this.desiredState === TSVariable_1.TSVariableState.kAbsent) {
                return "~";
            }
            else {
                return "";
            }
        }
        displayLeader() {
            if (this.desiredState === TSVariable_1.TSVariableState.kAbsent) {
                return "~";
            }
            else {
                return "  ";
            }
        }
        invertDesiredState() {
            if (this.desiredState === TSVariable_1.TSVariableState.kAbsent) {
                this.desiredState = TSVariable_1.TSVariableState.kPresent;
            }
            else {
                this.desiredState = TSVariable_1.TSVariableState.kAbsent;
            }
        }
        displayString() {
            return this.displayLeader() + this.variable.phrase;
        }
    }
    exports.TSDesiredStateVariableWrapper = TSDesiredStateVariableWrapper;
});
