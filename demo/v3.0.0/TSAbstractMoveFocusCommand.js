define(["require", "exports", "./KfCommand"], function (require, exports, KfCommand_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TSAbstractMoveFocusCommand extends KfCommand_1.KfCommand {
        constructor(domain, newFocus) {
            super();
            this.domain = domain;
            this.newFocus = newFocus;
            this.newFocusOldState = newFocus.getState();
            if (this.domain.world.focus !== null) {
                this.oldFocus = this.domain.world.focus;
                this.oldFocusOldState = this.oldFocus.getState();
            }
            else {
                this.oldFocus = newFocus;
                this.oldFocusOldState = newFocus.getState();
            }
        }
        updateForChanges() {
            this.domain.world.updateAvailable();
            this.domain.speechSystem.listenForAvailableCommands();
        }
        shiftsFocus() {
            let result = false;
            result = (this.newFocus !== this.domain.world.emptyEntry) && (this.newFocus !== this.oldFocus);
            return result;
        }
    }
    exports.TSAbstractMoveFocusCommand = TSAbstractMoveFocusCommand;
});
