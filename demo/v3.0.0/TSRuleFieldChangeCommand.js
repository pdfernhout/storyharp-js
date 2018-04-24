define(["require", "exports", "./common", "./KfCommand", "./TSRule"], function (require, exports, common_1, KfCommand_1, TSRule_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TSRuleFieldChangeCommand extends KfCommand_1.KfCommand {
        constructor(domain, rule, field, newValue) {
            super();
            this.domain = domain;
            this.rule = rule;
            this.field = field;
            this.oldValue = rule.getTextForField(field);
            this.newValue = newValue;
        }
        updateEditorForChange() {
            this.domain.editRule(this.rule, common_1.ScrollIntoViewDirection.kFromTop, true);
        }
        doCommand() {
            this.domain.world.lastVariableCreated = "";
            this.rule.setTextForField(this.field, this.newValue);
            this.domain.addToLog("--- edit rule #" + (this.domain.world.rules.indexOf(this.rule) + 1) + " " + TSRule_1.TSRuleField[this.field].substring(5));
            this.domain.addToLog(this.rule.getTextForField(this.field));
            this.updateEditorForChange();
            super.doCommand();
        }
        undoCommand() {
            this.rule.setTextForField(this.field, this.oldValue);
            this.updateEditorForChange();
            this.domain.ruleEditorForm.selectEditorField(this.field);
            super.undoCommand();
        }
        redoCommand() {
            this.rule.setTextForField(this.field, this.newValue);
            this.updateEditorForChange();
            this.domain.ruleEditorForm.selectEditorField(this.field);
            super.doCommand();
        }
        description() {
            let result = "";
            result = "Change " + TSRule_1.TSRule.headerForField(this.field) + " For Rule " + (this.domain.world.rules.indexOf(this.rule) + 1);
            return result;
        }
    }
    exports.TSRuleFieldChangeCommand = TSRuleFieldChangeCommand;
});
