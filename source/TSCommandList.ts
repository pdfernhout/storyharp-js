import { int } from "./common"
import { TWorld } from "./TWorld"
import { KfCommandList } from "./KfCommandList"
import { TSVariable } from "./TSVariable"
import { TSToggleVariableCommand } from "./TSToggleVariableCommand"
import { TSMoveFocusCommand } from "./TSMoveFocusCommand"
import { TSDoCommandPhraseCommand } from "./TSDoCommandPhraseCommand"
import { TSRule, TSRuleField } from "./TSRule"
import { TSRuleFieldChangeCommand } from "./TSRuleFieldChangeCommand"
import { TSDeleteRulesCommand } from "./TSDeleteRulesCommand"
import { TSMoveRulesCommand } from "./TSMoveRulesCommand"
import { TSDomain } from "./TSDomain"

export class TSCommandList extends KfCommandList {
    domain: TSDomain

    constructor(domain: TSDomain) {
        super()
        this.domain = domain
    }
    
    toggleVariable(variable: TSVariable): TSToggleVariableCommand {
        const result = new TSToggleVariableCommand(this.domain, variable)
        this.doCommand(result)
        return result
    }
    
    moveFocus(newFocus: TSVariable): TSMoveFocusCommand {
        const result = new TSMoveFocusCommand(this.domain, newFocus)
        this.doCommand(result)
        return result
    }
    
    doCommandPhrase(commandPhrase: string): TSDoCommandPhraseCommand {
        const result = new TSDoCommandPhraseCommand(this.domain, commandPhrase)
        this.doCommand(result)
        return result
    }
    
    ruleFieldChange(rule: TSRule, field: TSRuleField, newValue: string): TSRuleFieldChangeCommand {
        if ((field === TSRuleField.kRuleContext) || (field === TSRuleField.kRuleMove)) {
            if (rule.getTextForField(field).startsWith("new context ")) {
                if (this.domain.world.findVariable(newValue) === null) {
                    const newContextOrMove: TSVariable = this.domain.world.findOrCreateVariable(newValue, false)
                    newContextOrMove.position = rule.context.position
                }
            }
        }
        const result = new TSRuleFieldChangeCommand(this.domain, rule, field, newValue)
        this.doCommand(result)
        return result
    }
    
    deleteSelectedRules(): void {
        const command: TSDeleteRulesCommand = new TSDeleteRulesCommand(this.domain)
        
        for (let i = 0; i < this.domain.world.rules.length; i++) {
            const rule: TSRule = this.domain.world.rules[i]
            if (rule.selected) {
                command.addRule(rule, -1)
            }
        }
        if (command.ruleWrappers.length > 0) {
            this.doCommand(command)
        }
    }
    
    raiseSelectedRules(): void {
        const command: TSMoveRulesCommand = new TSMoveRulesCommand(this.domain)

        command.action = "raise"
        let moving = false
        for (let i = 1; i < this.domain.world.rules.length; i++) {
            //skip first
            const rule: TSRule = this.domain.world.rules[i]
            if (rule.selected) {
                if (!moving) {
                    const higherRule: TSRule = this.domain.world.rules[i - 1]
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
    
    lowerSelectedRules(): void {
        const command = new TSMoveRulesCommand(this.domain)

        command.action = "lower"
        let moving = false
        for (let i = this.domain.world.rules.length - 2; i >= 0; i--) {
            //skip first
            const rule: TSRule = this.domain.world.rules[i]
            if (rule.selected) {
                if (!moving) {
                    const lowerRule: TSRule = this.domain.world.rules[i + 1]
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
