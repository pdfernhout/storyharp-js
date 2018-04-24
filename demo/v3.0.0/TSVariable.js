define(["require", "exports", "./TSDraggableObject", "./TSRule"], function (require, exports, TSDraggableObject_1, TSRule_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    var TSVariableState;
    (function (TSVariableState) {
        TSVariableState[TSVariableState["kAbsent"] = 0] = "kAbsent";
        TSVariableState[TSVariableState["kPresent"] = 1] = "kPresent";
    })(TSVariableState = exports.TSVariableState || (exports.TSVariableState = {}));
    class TSVariable extends TSDraggableObject_1.TSDraggableObject {
        constructor() {
            super(...arguments);
            this.phrase = "";
            this.state = TSVariableState.kAbsent;
            this.contextUseages = 0;
            this.requirementsUseages = 0;
            this.commandUseages = 0;
            this.moveUseages = 0;
            this.changesUseages = 0;
            this.indexInVariables = 0;
        }
        displayName() {
            return this.phrase;
        }
        setPhrase(aPhrase) {
            this.phrase = aPhrase;
        }
        setState(newState) {
            this.state = newState;
        }
        getState() {
            return this.state;
        }
        hasUseagesForField(col) {
            let result = false;
            switch (col) {
                case TSRule_1.TSRuleField.kRuleContext:
                    result = (this.contextUseages > 0) || (this.moveUseages > 0);
                    break;
                case TSRule_1.TSRuleField.kRuleCommand:
                    result = this.commandUseages > 0;
                    break;
                case TSRule_1.TSRuleField.kRuleReply:
                    result = false;
                    break;
                case TSRule_1.TSRuleField.kRuleMove:
                    result = this.moveUseages > 0;
                    break;
                case TSRule_1.TSRuleField.kRuleRequirements:
                    result = this.requirementsUseages > 0;
                    break;
                case TSRule_1.TSRuleField.kRuleChanges:
                    result = this.changesUseages > 0;
                    break;
                default:
                    throw new Error("Unexpected case");
            }
            return result;
        }
        meetsDisplayOptions(displayOptions) {
            for (let i = 0; i <= 5; i++) {
                if (i === TSRule_1.TSRuleField.kRuleCommand) {
                    continue;
                }
                if (this.hasUseagesForField(i) && displayOptions[i]) {
                    return true;
                }
            }
            return false;
        }
    }
    exports.TSVariable = TSVariable;
});
