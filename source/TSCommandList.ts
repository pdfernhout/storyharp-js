import { int } from "./common"
import { KfCommandList } from "./KfCommandList"
import { TSVariable /*, TSVariableState */ } from "./TSVariable"
import { TSToggleVariableCommand, TSMoveFocusCommand, TSDoCommandPhrase } from "./usvariablecommands"
import { TSRule, TSRuleField } from "./TSRule"
import { TSRuleFieldChange } from "./uscommands"
import { TWorld } from "./TWorld"

export class TSCommandList extends KfCommandList {
    
    toggleVariable(variable: TSVariable): TSToggleVariableCommand {
        const result= new TSToggleVariableCommand(variable)
        this.doCommand(result)
        return result
    }
    
    moveFocus(newFocus: TSVariable): TSMoveFocusCommand {
        const result = new TSMoveFocusCommand(newFocus)
        this.doCommand(result)
        return result
    }
    
    doCommandPhrase(commandPhrase: string): TSDoCommandPhrase {
        const result = new TSDoCommandPhrase(commandPhrase)
        this.doCommand(result)
        return result
    }
    
    ruleFieldChange(world: TWorld, rule: TSRule, field: int, newValue: string): TSRuleFieldChange {
        if ((field === TSRuleField.kRuleContext) || (field === TSRuleField.kRuleMove)) {
            if (rule.getTextForField(field).startsWith("new context ")) {
                if (world.findVariable(newValue) === null) {
                    const newContextOrMove: TSVariable = world.findOrCreateVariable(newValue, false)
                    newContextOrMove.position = rule.context.position
                }
            }
        }
        const result = new TSRuleFieldChange(rule, field, newValue)
        this.doCommand(result)
        return result
    }
    
}
