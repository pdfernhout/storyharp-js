define(["require", "exports", "./TSAbstractMoveFocusCommand", "./common", "./TSVariable"], function (require, exports, TSAbstractMoveFocusCommand_1, common_1, TSVariable_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TSDoCommandPhraseCommand extends TSAbstractMoveFocusCommand_1.TSAbstractMoveFocusCommand {
        constructor(domain, commandPhrase) {
            const world = domain.world;
            const changedVariables = [];
            let newFocus = world.emptyEntry;
            const oldLastSaidTextWithMacros = domain.speechSystem.lastSaidTextWithMacros;
            let newLastSaidTextWithMacros = "";
            const oldFirstCommandDoneForLastCommandPhrase = world.firstCommandDoneForLastCommandPhrase;
            let newFirstCommandDoneForLastCommandPhrase = -1;
            for (let i = 0; i < world.rules.length; i++) {
                const rule = world.rules[i];
                if (rule.available && common_1.compareTextIgnoreCase(rule.command.phrase, commandPhrase)) {
                    domain.ruleEditorForm.lastCommand = rule;
                    if (newFirstCommandDoneForLastCommandPhrase === -1) {
                        newFirstCommandDoneForLastCommandPhrase = i;
                    }
                    const ruleResult = rule.recordReplyMoveChanges(changedVariables, newLastSaidTextWithMacros);
                    newLastSaidTextWithMacros = ruleResult.totalReply;
                    if (ruleResult.contextToFocusTo) {
                        newFocus = ruleResult.contextToFocusTo;
                    }
                }
            }
            if (commandPhrase.startsWith("$")) {
                commandPhrase = commandPhrase.substring(1);
            }
            super(domain, newFocus);
            this.newFocus = newFocus;
            this.oldLastSaidTextWithMacros = oldLastSaidTextWithMacros;
            this.newLastSaidTextWithMacros = newLastSaidTextWithMacros;
            this.oldFirstCommandDoneForLastCommandPhrase = oldFirstCommandDoneForLastCommandPhrase;
            this.newFirstCommandDoneForLastCommandPhrase = newFirstCommandDoneForLastCommandPhrase;
            this.commandPhrase = commandPhrase;
            this.changedVariables = changedVariables;
        }
        doCommand() {
            let i;
            this.domain.consoleForm.addLineToTranscript("> " + this.commandPhrase, common_1.Color.clBlue);
            this.domain.consoleForm.addLineToTranscript(this.domain.speechSystem.stripMacros(this.newLastSaidTextWithMacros), common_1.Color.clBlack);
            this.domain.consoleForm.scrollTranscriptEndIntoView();
            this.domain.speechSystem.sayTextWithMacros(this.newLastSaidTextWithMacros);
            this.domain.speechSystem.lastSaidTextWithMacros = this.newLastSaidTextWithMacros;
            this.domain.world.firstCommandDoneForLastCommandPhrase = this.newFirstCommandDoneForLastCommandPhrase;
            if (this.newFocus !== this.domain.world.emptyEntry) {
                this.oldFocus.setState(TSVariable_1.TSVariableState.kAbsent);
                this.domain.world.focus = this.newFocus;
                this.newFocus.setState(TSVariable_1.TSVariableState.kPresent);
            }
            for (i = 0; i < this.changedVariables.length; i++) {
                this.changedVariables[i].doChange();
            }
            this.updateForChanges();
            this.domain.speechSystem.checkForSayOptionsMacro();
            super.doCommand();
        }
        undoCommand() {
            let i;
            let undoPhrase;
            this.domain.consoleForm.addLineToTranscript("> undo", common_1.Color.clBlue);
            undoPhrase = "(You decide not to say \"" + this.commandPhrase + "\")";
            this.domain.consoleForm.addLineToTranscript(undoPhrase, common_1.Color.clBlue);
            this.domain.consoleForm.scrollTranscriptEndIntoView();
            this.domain.speechSystem.speakText(undoPhrase);
            this.domain.speechSystem.lastSaidTextWithMacros = this.oldLastSaidTextWithMacros;
            this.domain.world.firstCommandDoneForLastCommandPhrase = this.oldFirstCommandDoneForLastCommandPhrase;
            for (let i = this.changedVariables.length - 1; i >= 0; i--) {
                this.changedVariables[i].undoChange();
            }
            if (this.newFocus !== this.domain.world.emptyEntry) {
                this.newFocus.setState(this.newFocusOldState);
                this.domain.world.focus = this.oldFocus;
                this.oldFocus.setState(this.oldFocusOldState);
            }
            this.updateForChanges();
            super.undoCommand();
        }
        redoCommand() {
            this.domain.consoleForm.addLineToTranscript("> redo", common_1.Color.clBlue);
            const redoPhrase = "(You decide to say \"" + this.commandPhrase + "\" anyway)";
            this.domain.consoleForm.addLineToTranscript(redoPhrase, common_1.Color.clBlue);
            this.domain.consoleForm.scrollTranscriptEndIntoView();
            this.domain.speechSystem.speakText(redoPhrase);
            this.domain.speechSystem.lastSaidTextWithMacros = this.newLastSaidTextWithMacros;
            this.domain.world.firstCommandDoneForLastCommandPhrase = this.newFirstCommandDoneForLastCommandPhrase;
            if (this.newFocus !== this.domain.world.emptyEntry) {
                this.oldFocus.setState(TSVariable_1.TSVariableState.kAbsent);
                this.domain.world.focus = this.newFocus;
                this.newFocus.setState(TSVariable_1.TSVariableState.kPresent);
            }
            for (let i = 0; i < this.changedVariables.length; i++) {
                this.changedVariables[i].doChange();
            }
            this.updateForChanges();
            super.doCommand();
        }
        description() {
            let result = "";
            result = "command: " + this.commandPhrase;
            return result;
        }
    }
    exports.TSDoCommandPhraseCommand = TSDoCommandPhraseCommand;
});
