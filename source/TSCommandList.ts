import { int } from "./common"
import { KfCommandList } from "./KfCommandList"
import { TSVariable } from "./TSVariable"
import { TSToggleVariableCommand } from "./TSToggleVariableCommand"
import { TSMoveFocusCommand } from "./TSMoveFocusCommand"
import { TSDoCommandPhraseCommand } from "./TSDoCommandPhraseCommand"
import { TSRule, TSRuleField } from "./TSRule"
import { TSRuleFieldChangeCommand } from "./TSRuleFieldChangeCommand"
import { TWorld } from "./TWorld"

// TODO: Fix these as imports
import { RuleEditorForm, ChangeLogForm, ConsoleForm } from "./fixTypes"
import { TSDeleteRulesCommand } from "./TSDeleteRulesCommand";
import { TSMoveRulesCommand } from "./TSMoveRulesCommand";

export class TSCommandList extends KfCommandList {
    world: TWorld

    constructor(world: TWorld) {
        super()
        this.world = world
    }
    
    toggleVariable(consoleForm: ConsoleForm, variable: TSVariable): TSToggleVariableCommand {
        const result = new TSToggleVariableCommand(this.world, consoleForm, variable)
        this.doCommand(result)
        return result
    }
    
    moveFocus(consoleForm: ConsoleForm, newFocus: TSVariable): TSMoveFocusCommand {
        const result = new TSMoveFocusCommand(this.world, consoleForm, newFocus)
        this.doCommand(result)
        return result
    }
    
    doCommandPhrase(consoleForm: ConsoleForm, ruleEditorForm: RuleEditorForm, commandPhrase: string): TSDoCommandPhraseCommand {
        const result = new TSDoCommandPhraseCommand(this.world, consoleForm, ruleEditorForm, commandPhrase)
        this.doCommand(result)
        return result
    }
    
    ruleFieldChange(ruleEditorForm: RuleEditorForm, changeLogForm: ChangeLogForm, consoleForm: ConsoleForm, rule: TSRule, field: int, newValue: string): TSRuleFieldChangeCommand {
        if ((field === TSRuleField.kRuleContext) || (field === TSRuleField.kRuleMove)) {
            if (rule.getTextForField(field).startsWith("new context ")) {
                if (this.world.findVariable(newValue) === null) {
                    const newContextOrMove: TSVariable = this.world.findOrCreateVariable(newValue, false)
                    newContextOrMove.position = rule.context.position
                }
            }
        }
        const result = new TSRuleFieldChangeCommand(this.world, ruleEditorForm, changeLogForm, consoleForm, rule, field, newValue)
        this.doCommand(result)
        return result
    }
    
    deleteSelectedRules(ruleEditorForm: RuleEditorForm): void {
        const command: TSDeleteRulesCommand = new TSDeleteRulesCommand(this.world, ruleEditorForm)
        
        for (let i = 0; i < this.world.rules.length; i++) {
            const rule: TSRule = this.world.rules[i]
            if (rule.selected) {
                command.addRule(rule, -1)
            }
        }
        if (command.ruleWrappers.length > 0) {
            this.doCommand(command)
        }
    }
    
    raiseSelectedRules(ruleEditorForm: RuleEditorForm): void {
        const command: TSMoveRulesCommand = new TSMoveRulesCommand(this.world, ruleEditorForm)

        command.action = "raise"
        let moving = false
        for (let i = 1; i < this.world.rules.length; i++) {
            //skip first
            const rule: TSRule = this.world.rules[i]
            if (rule.selected) {
                if (!moving) {
                    const higherRule: TSRule = this.world.rules[i - 1]
                    if (!higherRule.selected) {
                        moving = true
                    }
                }
                if (moving) {
                    command.addRule(rule, i - 1)
                }
            } else {
                moving = true
            }
        }
        if (command.ruleWrappers.length > 0) {
            this.doCommand(command)
        }
    }
    
    lowerSelectedRules(ruleEditorForm: RuleEditorForm): void {
        const command = new TSMoveRulesCommand(this.world, ruleEditorForm)

        command.action = "lower"
        let moving = false
        for (let i = this.world.rules.length - 2; i >= 0; i--) {
            //skip first
            const rule: TSRule = this.world.rules[i]
            if (rule.selected) {
                if (!moving) {
                    const lowerRule: TSRule = this.world.rules[i + 1]
                    if (!lowerRule.selected) {
                        moving = true
                    }
                }
                if (moving) {
                    command.addRule(rule, i + 1)
                }
            } else {
                moving = true
            }
        }
        if (command.ruleWrappers.length > 0) {
            this.doCommand(command)
        }
    }
    
}
