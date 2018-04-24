define(["require", "exports", "./TSDraggableObject", "./TSVariable", "./TSChangedVariableWrapper", "./TSDesiredStateVariableWrapper"], function (require, exports, TSDraggableObject_1, TSVariable_1, TSChangedVariableWrapper_1, TSDesiredStateVariableWrapper_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    var TSRuleField;
    (function (TSRuleField) {
        TSRuleField[TSRuleField["kRuleContext"] = 0] = "kRuleContext";
        TSRuleField[TSRuleField["kRuleCommand"] = 1] = "kRuleCommand";
        TSRuleField[TSRuleField["kRuleReply"] = 2] = "kRuleReply";
        TSRuleField[TSRuleField["kRuleMove"] = 3] = "kRuleMove";
        TSRuleField[TSRuleField["kRuleRequirements"] = 4] = "kRuleRequirements";
        TSRuleField[TSRuleField["kLastRuleField"] = 5] = "kLastRuleField";
        TSRuleField[TSRuleField["kRuleChanges"] = 5] = "kRuleChanges";
    })(TSRuleField = exports.TSRuleField || (exports.TSRuleField = {}));
    const textToDisplayForEmptyCommandPhrase = "...new command...";
    class TSRule extends TSDraggableObject_1.TSDraggableObject {
        constructor() {
            super(...arguments);
            this.requirements = [];
            this.reply = "";
            this.changes = [];
            this.available = false;
            this.requirementsString = "";
            this.changesString = "";
            this.useagesRemoved = false;
        }
        displayName() {
            return this.command.phrase || textToDisplayForEmptyCommandPhrase;
        }
        displayNamePrefixed(showPrefix) {
            let result = "";
            if (showPrefix) {
                result = "> ";
            }
            result = result + (this.command.phrase || textToDisplayForEmptyCommandPhrase);
            return result;
        }
        addUseages() {
            this.context.contextUseages += 1;
            for (let i = 0; i < this.requirements.length; i++) {
                this.requirements[i].variable.requirementsUseages += 1;
            }
            this.command.commandUseages += 1;
            this.move.moveUseages += 1;
            for (let i = 0; i < this.changes.length; i++) {
                this.changes[i].variable.changesUseages += 1;
            }
            this.useagesRemoved = false;
        }
        removeUseages() {
            this.context.contextUseages -= 1;
            for (let i = 0; i < this.requirements.length; i++) {
                this.requirements[i].variable.requirementsUseages -= 1;
            }
            this.command.commandUseages -= 1;
            this.move.moveUseages -= 1;
            for (let i = 0; i < this.changes.length; i++) {
                this.changes[i].variable.changesUseages -= 1;
            }
            this.useagesRemoved = true;
        }
        setContext(aString) {
            if (this.context) {
                this.context.contextUseages -= 1;
            }
            this.context = this.world.findOrCreateVariable(aString, false);
            this.context.contextUseages += 1;
        }
        setRequirements(aString) {
            this.requirementsString = aString;
            for (let i = 0; i < this.requirements.length; i++) {
                this.requirements[i].variable.requirementsUseages -= 1;
            }
            this.requirements.length = 0;
            this.compile(aString, this.requirements);
            for (let i = 0; i < this.requirements.length; i++) {
                this.requirements[i].variable.requirementsUseages += 1;
            }
        }
        setCommand(aString) {
            if (this.command) {
                this.command.commandUseages -= 1;
            }
            this.command = this.world.findOrCreateVariable(aString, false);
            this.command.commandUseages += 1;
        }
        setReply(aString) {
            const safeString = aString.replace("\r", " ").replace("\n", " ");
            this.reply = safeString;
        }
        setMove(aString) {
            if (this.move) {
                this.move.moveUseages -= 1;
            }
            this.move = this.world.findOrCreateVariable(aString, false);
            this.move.moveUseages += 1;
        }
        setChanges(aString) {
            this.changesString = aString;
            for (let i = 0; i < this.changes.length; i++) {
                this.changes[i].variable.changesUseages -= 1;
            }
            this.changes.length = 0;
            this.compile(aString, this.changes);
            for (let i = 0; i < this.changes.length; i++) {
                this.changes[i].variable.changesUseages += 1;
            }
        }
        compile(aString, list) {
            let remaining = aString.trim();
            while (remaining.length > 0) {
                let phrase;
                let rest;
                const position = remaining.indexOf("&");
                if (position >= 0) {
                    phrase = remaining.substring(0, position);
                    rest = remaining.substring(position + 1);
                }
                else {
                    phrase = remaining;
                    rest = "";
                }
                phrase = phrase.trim();
                let desiredState;
                if (phrase.indexOf("~") === 0) {
                    desiredState = TSVariable_1.TSVariableState.kAbsent;
                    phrase = phrase.substring(1);
                }
                else {
                    desiredState = TSVariable_1.TSVariableState.kPresent;
                }
                const variable = this.world.findOrCreateVariable(phrase, false);
                const wrapper = new TSDesiredStateVariableWrapper_1.TSDesiredStateVariableWrapper(variable, desiredState);
                list.push(wrapper);
                remaining = rest.trim();
            }
        }
        decompile(list) {
            let result = "";
            for (let i = 0; i < list.length; i++) {
                const wrapper = list[i];
                const item = wrapper.leader() + wrapper.variable.phrase;
                if (result !== "") {
                    result = result + " & " + item;
                }
                else {
                    result = item;
                }
            }
            return result;
        }
        decompileRequirements() {
            return this.decompile(this.requirements);
        }
        decompileChanges() {
            return this.decompile(this.changes);
        }
        updateAvailable() {
            this.available = false;
            if (this.context === this.world.emptyEntry) {
                return;
            }
            if (this.context.state !== TSVariable_1.TSVariableState.kPresent) {
                return;
            }
            for (let i = 0; i < this.requirements.length; i++) {
                const wrapper = this.requirements[i];
                if (wrapper.variable.getState() !== wrapper.desiredState) {
                    return;
                }
            }
            this.available = true;
        }
        recordReplyMoveChanges(changedVariablesList, totalReply) {
            let contextToFocusTo = null;
            if ((totalReply !== "") && (this.reply !== "")) {
                totalReply = totalReply + " ";
            }
            totalReply = totalReply + this.reply;
            if (this.move !== this.world.emptyEntry) {
                contextToFocusTo = this.move;
            }
            for (let i = 0; i < this.changes.length; i++) {
                const desiredStateWrapper = this.changes[i];
                const changedVariableWrapper = new TSChangedVariableWrapper_1.TSChangedVariableWrapper(desiredStateWrapper.variable, desiredStateWrapper.desiredState);
                changedVariablesList.push(changedVariableWrapper);
            }
            return { totalReply, contextToFocusTo };
        }
        setTextForField(col, text) {
            switch (col) {
                case TSRuleField.kRuleContext:
                    this.setContext(text);
                    break;
                case TSRuleField.kRuleCommand:
                    this.setCommand(text);
                    break;
                case TSRuleField.kRuleReply:
                    this.setReply(text);
                    break;
                case TSRuleField.kRuleMove:
                    this.setMove(text);
                    break;
                case TSRuleField.kRuleRequirements:
                    this.setRequirements(text);
                    break;
                case TSRuleField.kRuleChanges:
                    this.setChanges(text);
                    break;
                default:
                    throw new Error("Unexpected case");
            }
        }
        getTextForField(col) {
            let result = "";
            switch (col) {
                case TSRuleField.kRuleContext:
                    result = this.context.phrase;
                    break;
                case TSRuleField.kRuleCommand:
                    result = this.command.phrase;
                    break;
                case TSRuleField.kRuleReply:
                    result = this.reply;
                    break;
                case TSRuleField.kRuleMove:
                    result = this.move.phrase;
                    break;
                case TSRuleField.kRuleRequirements:
                    result = this.requirementsString;
                    break;
                case TSRuleField.kRuleChanges:
                    result = this.changesString;
                    break;
                default:
                    throw new Error("Unexpected case");
            }
            return result;
        }
        static headerForField(col) {
            let result = "";
            switch (col) {
                case TSRuleField.kRuleContext:
                    result = "Context";
                    break;
                case TSRuleField.kRuleCommand:
                    result = "Command";
                    break;
                case TSRuleField.kRuleReply:
                    result = "Reply";
                    break;
                case TSRuleField.kRuleMove:
                    result = "Move";
                    break;
                case TSRuleField.kRuleRequirements:
                    result = "Requirements";
                    break;
                case TSRuleField.kRuleChanges:
                    result = "Changes";
                    break;
                default:
                    throw new Error("Unexpected case");
            }
            return result;
        }
        usesVariableInList(variable, list) {
            for (let i = 0; i < list.length; i++) {
                const wrapper = list[i];
                if (wrapper.variable === variable) {
                    return true;
                }
            }
            return false;
        }
        usesVariableFor(variable, col) {
            let result = false;
            result = false;
            switch (col) {
                case TSRuleField.kRuleContext:
                    result = this.context === variable;
                    break;
                case TSRuleField.kRuleCommand:
                    result = this.command === variable;
                    break;
                case TSRuleField.kRuleReply:
                    console.log("Is this an error?");
                    result = false;
                    break;
                case TSRuleField.kRuleMove:
                    result = this.move === variable;
                    break;
                case TSRuleField.kRuleRequirements:
                    result = this.usesVariableInList(variable, this.requirements);
                    break;
                case TSRuleField.kRuleChanges:
                    result = this.usesVariableInList(variable, this.changes);
                    break;
                default:
                    throw new Error("Unexpected case");
            }
            return result;
        }
        variableInList(n, list) {
            let result;
            if (n < 0) {
                n = 0;
            }
            if (n < list.length) {
                const wrapper = list[n];
                result = wrapper.variable;
            }
            else {
                result = this.world.emptyEntry;
            }
            return result;
        }
        variableForFieldWithSelections(col, requirementsIndex, changesIndex) {
            let result = this.world.emptyEntry;
            switch (col) {
                case TSRuleField.kRuleContext:
                    result = this.context;
                    break;
                case TSRuleField.kRuleCommand:
                    result = this.command;
                    break;
                case TSRuleField.kRuleReply:
                    console.log("Is this an error too?");
                    result = this.world.emptyEntry;
                    break;
                case TSRuleField.kRuleMove:
                    result = this.move;
                    break;
                case TSRuleField.kRuleRequirements:
                    result = this.variableInList(requirementsIndex, this.requirements);
                    break;
                case TSRuleField.kRuleChanges:
                    result = this.variableInList(changesIndex, this.changes);
                    break;
                default:
                    throw new Error("Unexpected case");
            }
            return result;
        }
        variableForField(col) {
            const result = this.variableForFieldWithSelections(col, 0, 0);
            return result;
        }
        setPosition(value) {
            const [firstPart, secondPart, thirdPart] = value.split("|");
            super.setPosition(firstPart);
            this.context.setPosition(secondPart);
            this.move.setPosition(thirdPart);
        }
    }
    exports.TSRule = TSRule;
});
