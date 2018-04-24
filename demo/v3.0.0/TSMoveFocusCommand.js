define(["require", "exports", "./TSAbstractMoveFocusCommand", "./TSVariable"], function (require, exports, TSAbstractMoveFocusCommand_1, TSVariable_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TSMoveFocusCommand extends TSAbstractMoveFocusCommand_1.TSAbstractMoveFocusCommand {
        doCommand() {
            this.oldFocus.setState(TSVariable_1.TSVariableState.kAbsent);
            this.domain.world.focus = this.newFocus;
            this.newFocus.setState(TSVariable_1.TSVariableState.kPresent);
            this.updateForChanges();
            TSAbstractMoveFocusCommand_1.TSAbstractMoveFocusCommand.prototype.doCommand.call(this);
        }
        undoCommand() {
            this.newFocus.setState(this.newFocusOldState);
            this.domain.world.focus = this.oldFocus;
            this.oldFocus.setState(this.oldFocusOldState);
            this.updateForChanges();
            TSAbstractMoveFocusCommand_1.TSAbstractMoveFocusCommand.prototype.undoCommand.call(this);
        }
        description() {
            let result = "";
            result = "move focus to " + this.newFocus.phrase;
            return result;
        }
    }
    exports.TSMoveFocusCommand = TSMoveFocusCommand;
});
