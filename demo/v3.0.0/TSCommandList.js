define(["require", "exports", "./KfCommandList", "./TSToggleVariableCommand", "./TSMoveFocusCommand", "./TSDoCommandPhraseCommand", "./TSRule", "./TSRuleFieldChangeCommand", "./TSDeleteRulesCommand", "./TSMoveRulesCommand"], function (require, exports, KfCommandList_1, TSToggleVariableCommand_1, TSMoveFocusCommand_1, TSDoCommandPhraseCommand_1, TSRule_1, TSRuleFieldChangeCommand_1, TSDeleteRulesCommand_1, TSMoveRulesCommand_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class TSCommandList extends KfCommandList_1.KfCommandList {
        constructor(domain) {
            super();
            this.domain = domain;
        }
        toggleVariable(variable) {
            const result = new TSToggleVariableCommand_1.TSToggleVariableCommand(this.domain, variable);
            this.doCommand(result);
            return result;
        }
        moveFocus(newFocus) {
            const result = new TSMoveFocusCommand_1.TSMoveFocusCommand(this.domain, newFocus);
            this.doCommand(result);
            return result;
        }
        doCommandPhrase(commandPhrase) {
            const result = new TSDoCommandPhraseCommand_1.TSDoCommandPhraseCommand(this.domain, commandPhrase);
            this.doCommand(result);
            return result;
        }
        ruleFieldChange(rule, field, newValue) {
            if ((field === TSRule_1.TSRuleField.kRuleContext) || (field === TSRule_1.TSRuleField.kRuleMove)) {
                if (rule.getTextForField(field).startsWith("new context ")) {
                    if (this.domain.world.findVariable(newValue) === null) {
                        const newContextOrMove = this.domain.world.findOrCreateVariable(newValue, false);
                        newContextOrMove.position = rule.context.position;
                    }
                }
            }
            const result = new TSRuleFieldChangeCommand_1.TSRuleFieldChangeCommand(this.domain, rule, field, newValue);
            this.doCommand(result);
            return result;
        }
        deleteSelectedRules() {
            const command = new TSDeleteRulesCommand_1.TSDeleteRulesCommand(this.domain);
            for (let i = 0; i < this.domain.world.rules.length; i++) {
                const rule = this.domain.world.rules[i];
                if (rule.selected) {
                    command.addRule(rule, -1);
                }
            }
            if (command.ruleWrappers.length > 0) {
                this.doCommand(command);
            }
        }
        raiseSelectedRules() {
            const command = new TSMoveRulesCommand_1.TSMoveRulesCommand(this.domain);
            command.action = "raise";
            let moving = false;
            for (let i = 1; i < this.domain.world.rules.length; i++) {
                const rule = this.domain.world.rules[i];
                if (rule.selected) {
                    if (!moving) {
                        const higherRule = this.domain.world.rules[i - 1];
                        if (!higherRule.selected) {
                            moving = true;
                        }
                    }
                    if (moving) {
                        command.addRule(rule, i - 1);
                    }
                }
                else {
                    moving = true;
                }
            }
            if (command.ruleWrappers.length > 0) {
                this.doCommand(command);
            }
        }
        lowerSelectedRules() {
            const command = new TSMoveRulesCommand_1.TSMoveRulesCommand(this.domain);
            command.action = "lower";
            let moving = false;
            for (let i = this.domain.world.rules.length - 2; i >= 0; i--) {
                const rule = this.domain.world.rules[i];
                if (rule.selected) {
                    if (!moving) {
                        const lowerRule = this.domain.world.rules[i + 1];
                        if (!lowerRule.selected) {
                            moving = true;
                        }
                    }
                    if (moving) {
                        command.addRule(rule, i + 1);
                    }
                }
                else {
                    moving = true;
                }
            }
            if (command.ruleWrappers.length > 0) {
                this.doCommand(command);
            }
        }
    }
    exports.TSCommandList = TSCommandList;
});
