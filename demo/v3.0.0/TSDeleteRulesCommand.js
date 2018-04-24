define(["require", "exports", "./common", "./KfCommand", "./TSIndexChangeRuleWrapper"], function (require, exports, common_1, KfCommand_1, TSIndexChangeRuleWrapper_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TSDeleteRulesCommand extends KfCommand_1.KfCommand {
        constructor(domain) {
            super();
            this.ruleWrappers = [];
            this.domain = domain;
        }
        addRule(rule, newIndex) {
            const wrapper = new TSIndexChangeRuleWrapper_1.TSIndexChangeRuleWrapper(rule, newIndex);
            this.ruleWrappers.push(wrapper);
        }
        doCommand() {
            for (let i = this.ruleWrappers.length - 1; i >= 0; i--) {
                const wrapper = this.ruleWrappers[i];
                if (wrapper.rule === this.domain.editedRule) {
                    this.domain.editRule(null);
                }
                wrapper.rule.removeUseages();
                wrapper.doChange();
            }
            super.doCommand();
        }
        undoCommand() {
            this.domain.world.deselectAllExcept(null);
            for (let i = 0; i < this.ruleWrappers.length; i++) {
                const wrapper = this.ruleWrappers[i];
                wrapper.rule.addUseages();
                wrapper.undoChange();
                wrapper.rule.selected = true;
            }
            if (this.ruleWrappers.length > 0) {
                this.domain.editRule(this.ruleWrappers[0].rule, common_1.ScrollIntoViewDirection.kFromTop, true);
            }
            super.undoCommand();
        }
        redoCommand() {
            this.domain.world.deselectAllExcept(null);
            for (let i = this.ruleWrappers.length - 1; i >= 0; i--) {
                const wrapper = this.ruleWrappers[i];
                if ((wrapper.rule === this.domain.editedRule)) {
                    this.domain.editRule(null);
                }
                wrapper.rule.removeUseages();
                wrapper.doChange();
            }
            super.doCommand();
        }
        description() {
            let result = "";
            if (this.ruleWrappers.length > 1) {
                result = "delete rules";
            }
            else if (this.ruleWrappers.length === 1) {
                result = "delete rule";
            }
            else {
                result = "delete rule";
            }
            return result;
        }
    }
    exports.TSDeleteRulesCommand = TSDeleteRulesCommand;
});
