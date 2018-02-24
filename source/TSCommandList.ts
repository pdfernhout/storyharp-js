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

export class TSCommandList extends KfCommandList {
    world: TWorld

    constructor(world: TWorld) {
        super()
        this.world = world
    }
    
    toggleVariable(consoleForm: ConsoleForm, variable: TSVariable): TSToggleVariableCommand {
        const result= new TSToggleVariableCommand(this.world, consoleForm, variable)
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
    
}
