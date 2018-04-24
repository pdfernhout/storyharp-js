define(["require", "exports", "./common", "./KfCommand", "./TSIndexChangeRuleWrapper"], function (require, exports, common_1, KfCommand_1, TSIndexChangeRuleWrapper_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TSMoveRulesCommand extends KfCommand_1.KfCommand {
        constructor(domain) {
            super();
            this.ruleWrappers = [];
            this.action = "";
            this.domain = domain;
        }
        addRule(rule, newIndex) {
            const wrapper = new TSIndexChangeRuleWrapper_1.TSIndexChangeRuleWrapper(rule, newIndex);
            this.ruleWrappers.push(wrapper);
        }
        doCommand() {
            for (let i = 0; i < this.ruleWrappers.length; i++) {
                const wrapper = this.ruleWrappers[i];
                wrapper.doChange();
            }
            super.doCommand();
            if (this.action === "raise") {
                this.domain.ruleEditorForm.scrollGridSelectionsIntoView(common_1.ScrollIntoViewDirection.kFromTop);
            }
            else {
                this.domain.ruleEditorForm.scrollGridSelectionsIntoView(common_1.ScrollIntoViewDirection.kFromBottom);
            }
        }
        undoCommand() {
            this.domain.world.deselectAllExcept(null);
            for (let i = this.ruleWrappers.length - 1; i >= 0; i--) {
                const wrapper = this.ruleWrappers[i];
                wrapper.rule.selected = true;
                wrapper.undoChange();
            }
            super.undoCommand();
            if (this.action === "raise") {
                this.domain.ruleEditorForm.scrollGridSelectionsIntoView(common_1.ScrollIntoViewDirection.kFromBottom);
            }
            else {
                this.domain.ruleEditorForm.scrollGridSelectionsIntoView(common_1.ScrollIntoViewDirection.kFromTop);
            }
        }
        redoCommand() {
            this.domain.world.deselectAllExcept(null);
            for (let i = 0; i < this.ruleWrappers.length; i++) {
                const wrapper = this.ruleWrappers[i];
                wrapper.rule.selected = true;
                wrapper.doChange();
            }
            super.doCommand();
            if (this.action === "raise") {
                this.domain.ruleEditorForm.scrollGridSelectionsIntoView(common_1.ScrollIntoViewDirection.kFromTop);
            }
            else {
                this.domain.ruleEditorForm.scrollGridSelectionsIntoView(common_1.ScrollIntoViewDirection.kFromBottom);
            }
        }
        description() {
            let result = "";
            if (this.ruleWrappers.length > 1) {
                result = "rules";
            }
            else if (this.ruleWrappers.length === 1) {
                result = "rule";
            }
            else {
                result = "rule";
            }
            if (this.action !== "") {
                result = this.action + " " + result;
            }
            else {
                result = "move " + result;
            }
            return result;
        }
    }
    exports.TSMoveRulesCommand = TSMoveRulesCommand;
});
