define(["require", "exports", "./common", "./TPoint", "./TRect", "./TSVariable", "./TSRule", "./TSDragRecord"], function (require, exports, common_1, TPoint_1, TRect_1, TSVariable_1, TSRule_1, TSDragRecord_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    var ExportRulesOption;
    (function (ExportRulesOption) {
        ExportRulesOption[ExportRulesOption["kSaveAllRules"] = 0] = "kSaveAllRules";
        ExportRulesOption[ExportRulesOption["kSaveOnlySelectedRules"] = 1] = "kSaveOnlySelectedRules";
    })(ExportRulesOption = exports.ExportRulesOption || (exports.ExportRulesOption = {}));
    function swapIntegers(a, b) {
        return [b, a];
    }
    class TWorld {
        constructor(goodPositionCallback) {
            this.emptyEntry = new TSVariable_1.TSVariable();
            this.variables = [];
            this.rules = [];
            this.focus = null;
            this.previousFocus = null;
            this.firstCommandDoneForLastCommandPhrase = 0;
            this.lastVariableCreated = "";
            if (goodPositionCallback) {
                this.goodPositionCallback = goodPositionCallback;
            }
            else {
                this.goodPositionCallback = () => new TPoint_1.TPoint(0, 0);
            }
        }
        resetVariableValues() {
            if (this.variables) {
                for (let i = 0; i < this.variables.length; i++) {
                    this.variables[i].state = TSVariable_1.TSVariableState.kAbsent;
                }
            }
        }
        resetVariablesAndRules() {
            if (this.rules) {
                this.rules.length = 0;
            }
            if (this.variables) {
                this.variables.length = 0;
            }
        }
        newRule() {
            const result = new TSRule_1.TSRule();
            result.world = this;
            result.context = this.emptyEntry;
            result.command = this.emptyEntry;
            result.move = this.emptyEntry;
            result.position = this.goodPositionCallback();
            this.rules.push(result);
            return result;
        }
        findVariable(aString) {
            if (aString === "") {
                return this.emptyEntry;
            }
            for (let i = 0; i < this.variables.length; i++) {
                if (common_1.compareTextIgnoreCase(this.variables[i].phrase, aString)) {
                    const result = this.variables[i];
                    if (result.phrase !== aString) {
                        result.setPhrase(aString);
                    }
                    return result;
                }
            }
            return null;
        }
        findOrCreateVariable(aString, madeByMacro) {
            const match = this.findVariable(aString.trim());
            if (match !== null) {
                return match;
            }
            const result = new TSVariable_1.TSVariable();
            result.setPhrase(aString.trim());
            result.state = TSVariable_1.TSVariableState.kAbsent;
            result.position = this.goodPositionCallback();
            this.variables.push(result);
            this.lastVariableCreated = aString;
            return result;
        }
        setInitialFocus() {
            if (this.rules.length > 0) {
                this.focus = this.rules[0].context;
                this.previousFocus = this.rules[0].context;
                this.focus.state = TSVariable_1.TSVariableState.kPresent;
                this.updateAvailable();
            }
            else {
                this.focus = null;
                this.previousFocus = null;
            }
        }
        loadWorldFromFileContents(contents) {
            const lines = contents.split(/\r?\n/);
            if (lines.length && lines[lines.length - 1] === "")
                lines.pop();
            function readln() {
                const result = lines.shift();
                if (result !== undefined)
                    return result;
                throw new Error("Unexpected EOF loading file");
            }
            function eof() {
                return !lines.length;
            }
            let count = 0;
            const header = readln();
            if ((header !== "; world file version 1.0") && (header !== "; StoryHarp world file version 1.3")) {
                throw new Error("File header for world file is not correct");
            }
            while (!eof()) {
                let value = readln();
                if ((value !== "") && (value[0] === ";")) {
                    continue;
                }
                if (value !== "====================") {
                    return false;
                }
                if (count === 0) {
                    value = readln();
                    value = readln();
                    value = readln();
                    value = readln();
                    value = readln();
                    value = readln();
                    value = readln();
                }
                else {
                    const rule = this.newRule();
                    value = readln();
                    rule.setContext(value.trim());
                    value = readln();
                    rule.setCommand(value.trim());
                    value = readln();
                    rule.setReply(value.trim());
                    value = readln();
                    rule.setMove(value.trim());
                    value = readln();
                    rule.setChanges(value.trim());
                    value = readln();
                    rule.setRequirements(value.trim());
                    value = readln();
                    rule.setPosition(value.trim());
                }
                count = count + 1;
            }
            return true;
        }
        saveWorldToFileContents(rulesToExport) {
            const lines = [];
            function writeln(...sections) {
                lines.push(sections.join(""));
            }
            writeln("; StoryHarp world file version 1.3");
            writeln("====================");
            for (let i = 0; i < 6; i++) {
                writeln(TSRule_1.TSRule.headerForField(i));
            }
            writeln("map positions");
            for (let i = 0; i < this.rules.length; i++) {
                const rule = this.rules[i];
                if (rulesToExport === ExportRulesOption.kSaveOnlySelectedRules && !rule.selected) {
                    continue;
                }
                writeln("====================");
                writeln(rule.context.phrase);
                writeln(rule.command.phrase);
                writeln(rule.reply);
                writeln(rule.move.phrase);
                writeln(rule.changesString);
                writeln(rule.requirementsString);
                writeln(rule.position.X, ",", rule.position.Y, "|", rule.context.position.X, ",", rule.context.position.Y, "|", rule.move.position.X, ",", rule.move.position.Y);
            }
            return lines.join("\n") + "\n";
        }
        newSession() {
            this.resetVariableValues();
            this.setInitialFocus();
        }
        loadSessionFromFileContents(worldFileName, contents) {
            let focusNameRead;
            let previousFocusNameRead;
            const lines = contents.split(/\r?\n/);
            if (lines.length && lines[lines.length - 1] === "")
                lines.pop();
            function readln() {
                const result = lines.shift();
                if (result !== undefined)
                    return result;
                throw new Error("Unexpected EOF loading file");
            }
            function eof() {
                return !lines.length;
            }
            let header;
            this.resetVariableValues();
            header = readln();
            if (header !== "; session file version 1.0") {
                throw new Error("File header for session file is not correct");
            }
            header = readln();
            if (header !== "============ Variables for world =================") {
                return false;
            }
            const worldFileNameRead = readln();
            if (worldFileNameRead !== worldFileName) {
                throw new Error("worldFileNameRead !== worldFileName");
            }
            header = readln();
            if (header !== "============ Focus ===============================") {
                return false;
            }
            focusNameRead = readln();
            previousFocusNameRead = readln();
            header = readln();
            if (header !== "============ Variables ===========================") {
                return false;
            }
            while (!eof()) {
                const variableNameRead = readln().trim();
                const variable = this.findOrCreateVariable(variableNameRead, false);
                variable.state = TSVariable_1.TSVariableState.kPresent;
            }
            this.focus = this.findOrCreateVariable(focusNameRead, false);
            this.previousFocus = this.findOrCreateVariable(previousFocusNameRead, false);
            this.updateAvailable();
            return true;
        }
        saveSessionToFileContents(worldFileName) {
            const lines = [];
            function writeln(...sections) {
                lines.push(sections.join(""));
            }
            writeln("; session file version 1.0");
            writeln("============ Variables for world =================");
            writeln(worldFileName);
            writeln("============ Focus ===============================");
            writeln(this.focus ? this.focus.phrase : "");
            writeln(this.previousFocus ? this.previousFocus.phrase : "");
            writeln("============ Variables ===========================");
            for (let i = 0; i < this.variables.length; i++) {
                const variable = this.variables[i];
                if (variable.state === TSVariable_1.TSVariableState.kPresent) {
                    writeln(variable.phrase);
                }
            }
            return lines.join("\n") + "\n";
        }
        updateAvailable() {
            for (let i = 0; i < this.rules.length; i++) {
                const rule = this.rules[i];
                rule.updateAvailable();
            }
        }
        setFocusTo(contextToFocusOn) {
            if (contextToFocusOn !== null) {
                this.previousFocus = this.focus;
                if (this.previousFocus) {
                    this.previousFocus.setState(TSVariable_1.TSVariableState.kAbsent);
                }
                this.focus = contextToFocusOn;
                this.focus.setState(TSVariable_1.TSVariableState.kPresent);
            }
        }
        deselectAllExcept(exceptObject) {
            let result = false;
            for (let i = 0; i < this.rules.length; i++) {
                const rule = this.rules[i];
                if ((rule.selected) && (rule !== exceptObject)) {
                    rule.selected = false;
                    result = true;
                }
            }
            for (let i = 0; i < this.variables.length; i++) {
                const variable = this.variables[i];
                if ((variable.selected) && (variable !== exceptObject)) {
                    variable.selected = false;
                    result = true;
                }
            }
            return result;
        }
        addDragRecordsToList(dragRecords) {
            for (let i = 0; i < this.rules.length; i++) {
                const rule = this.rules[i];
                if ((rule.selected)) {
                    dragRecords.push(new TSDragRecord_1.TSDragRecord(rule));
                }
            }
            for (let i = 0; i < this.variables.length; i++) {
                const variable = this.variables[i];
                if ((variable.selected)) {
                    dragRecords.push(new TSDragRecord_1.TSDragRecord(variable));
                }
            }
        }
        selectAvailable() {
            for (let i = 0; i < this.rules.length; i++) {
                const rule = this.rules[i];
                rule.selected = rule.available;
            }
        }
        firstAvailable() {
            let result = new TSRule_1.TSRule();
            for (let i = 0; i < this.rules.length; i++) {
                const rule = this.rules[i];
                if (rule.available) {
                    return rule;
                }
            }
            return null;
        }
        selectInRectangle(rect) {
            let intersection;
            let rule;
            let variable;
            rect = rect.copy();
            if (rect.Right < rect.Left) {
                [rect.Left, rect.Right] = swapIntegers(rect.Left, rect.Right);
            }
            if (rect.Bottom < rect.Top) {
                [rect.Top, rect.Bottom] = swapIntegers(rect.Top, rect.Bottom);
            }
            for (let i = 0; i < this.rules.length; i++) {
                const rule = this.rules[i];
                if (rule.bounds().intersects(rect)) {
                    rule.selected = true;
                }
            }
            for (let i = 0; i < this.variables.length; i++) {
                const variable = this.variables[i];
                if (variable.bounds().intersects(rect)) {
                    variable.selected = true;
                }
            }
        }
        firstSelectedVariable() {
            for (let i = 0; i < this.variables.length; i++) {
                const variable = this.variables[i];
                if (variable.selected) {
                    return variable;
                }
            }
            return null;
        }
        firstSelectedObject() {
            for (let i = 0; i < this.variables.length; i++) {
                const variable = this.variables[i];
                if (variable.selected) {
                    return variable;
                }
            }
            for (let i = 0; i < this.rules.length; i++) {
                const rule = this.rules[i];
                if (rule.selected) {
                    return rule;
                }
            }
            return null;
        }
        getContexts() {
            const result = [];
            for (let i = 0; i < this.variables.length; i++) {
                const variable = this.variables[i];
                if (variable.contextUseages > 0) {
                    result.push(variable);
                }
            }
            return result;
        }
        getContextNames() {
            return this.getContexts().map(variable => variable.phrase).sort();
        }
        getVariableNames() {
            const variableNames = new Set();
            this.variables.map(variable => variableNames.add(variable.phrase));
            return [...variableNames].sort();
        }
        getCommandNames() {
            const commandNames = new Set();
            this.rules.map(rule => commandNames.add(rule.command.phrase));
            return [...commandNames].sort();
        }
        boundsRect() {
            let result = new TRect_1.TRect(0, 0, 0, 0);
            function addToBounds(rect) {
                if (result.Left > rect.Left) {
                    result.Left = rect.Left;
                }
                if (result.Right < rect.Right) {
                    result.Right = rect.Right;
                }
                if (result.Top > rect.Top) {
                    result.Top = rect.Top;
                }
                if (result.Bottom < rect.Bottom) {
                    result.Bottom = rect.Bottom;
                }
            }
            for (let i = 0; i < this.variables.length; i++) {
                const node = this.variables[i];
                addToBounds(node.bounds());
            }
            for (let i = 0; i < this.rules.length; i++) {
                const node = this.rules[i];
                addToBounds(node.bounds());
            }
            const inflate = 10;
            return new TRect_1.TRect(result.Left - inflate, result.Top - inflate, result.Right + inflate, result.Bottom + inflate);
        }
        updateVariablesForIndexInVariables() {
            for (let i = 0; i < this.variables.length; i++) {
                const variable = this.variables[i];
                variable.indexInVariables = i;
            }
        }
    }
    exports.TWorld = TWorld;
});
