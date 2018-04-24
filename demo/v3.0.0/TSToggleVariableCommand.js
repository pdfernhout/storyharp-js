define(["require", "exports", "./KfCommand", "./TSVariable"], function (require, exports, KfCommand_1, TSVariable_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TSToggleVariableCommand extends KfCommand_1.KfCommand {
        constructor(domain, variable) {
            super();
            this.variable = new TSVariable_1.TSVariable();
            this.domain = domain;
            this.variable = variable;
            this.oldState = variable.getState();
            if (this.oldState === TSVariable_1.TSVariableState.kPresent) {
                this.newState = TSVariable_1.TSVariableState.kAbsent;
            }
            else {
                this.newState = TSVariable_1.TSVariableState.kPresent;
            }
        }
        setVariableStateWithUpdate(state) {
            this.variable.setState(state);
            this.domain.world.updateAvailable();
            this.domain.speechSystem.listenForAvailableCommands();
        }
        doCommand() {
            this.setVariableStateWithUpdate(this.newState);
            KfCommand_1.KfCommand.prototype.doCommand.call(this);
        }
        undoCommand() {
            this.setVariableStateWithUpdate(this.oldState);
            KfCommand_1.KfCommand.prototype.undoCommand.call(this);
        }
        description() {
            let result = "";
            if (this.newState === TSVariable_1.TSVariableState.kPresent) {
                result = "toggle \"" + this.variable.phrase + "\" to true";
            }
            else {
                result = "toggle \"" + this.variable.phrase + "\" to false";
            }
            return result;
        }
    }
    exports.TSToggleVariableCommand = TSToggleVariableCommand;
});
