define(["require", "exports", "./common", "./KfCommand"], function (require, exports, common_1, KfCommand_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TSNewRulesCommand extends KfCommand_1.KfCommand {
        constructor(domain) {
            super();
            this.rules = [];
            this.creator = "";
            this.domain = domain;
        }
        addRule(rule) {
            this.rules.push(rule);
        }
        doCommand() {
            super.doCommand();
            this.domain.ruleEditorForm.scrollGridSelectionsIntoView(common_1.ScrollIntoViewDirection.kFromBottom);
            for (let rule of this.rules) {
                this.domain.addToLog("--- new rule");
                let textForRule = [
                    rule.context.phrase,
                    rule.command.phrase,
                    rule.reply,
                    rule.move.phrase,
                    rule.changesString,
                    rule.requirementsString
                ].join("\n");
                this.domain.addToLog(textForRule);
            }
        }
        undoCommand() {
            for (let i = 0; i < this.rules.length; i++) {
                const rule = this.rules[i];
                common_1.arrayRemove(this.domain.world.rules, rule);
                rule.selected = false;
                rule.removeUseages();
            }
            super.undoCommand();
            if (this.domain.editedRule) {
                if (this.rules.indexOf(this.domain.editedRule) >= 0) {
                    this.domain.editRule(null);
                }
            }
        }
        redoCommand() {
            this.domain.world.deselectAllExcept(null);
            for (let i = 0; i < this.rules.length; i++) {
                const rule = this.rules[i];
                rule.selected = true;
                this.domain.world.rules.push(rule);
                rule.addUseages();
            }
            super.doCommand();
            this.domain.ruleEditorForm.scrollGridSelectionsIntoView(common_1.ScrollIntoViewDirection.kFromBottom);
            if (this.rules.length > 0) {
                this.domain.editRule(this.rules[this.rules.length - 1], common_1.ScrollIntoViewDirection.kFromBottom, true);
            }
        }
        description() {
            let result = "";
            if (this.rules.length > 1) {
                result = "new rules";
            }
            else if (this.rules.length === 1) {
                result = "new rule";
            }
            else {
                result = "new rule";
            }
            if (this.creator !== "") {
                result = result + " from " + this.creator;
            }
            if (this.creator === "duplicating") {
                result = "duplicate rule";
            }
            return result;
        }
    }
    exports.TSNewRulesCommand = TSNewRulesCommand;
});
