import { int } from "./common"
import { KfCommandList } from "./KfCommandList"
import { TSVariable /*, TSVariableState */ } from "./TSVariable"
import { TSToggleVariableCommand, TSMoveFocusCommand, TSDoCommandPhrase } from "./usvariablecommands"
import { TSRule, TSRuleField } from "./TSRule"
import { TSRuleFieldChange } from "./uscommands"
import { TWorld } from "./TWorld"

export class TSCommandList extends KfCommandList {
    world: TWorld

    constructor(world: TWorld) {
        super()
        this.world = world
    }
    
    toggleVariable(variable: TSVariable): TSToggleVariableCommand {
        const result= new TSToggleVariableCommand(this.world, variable)
        this.doCommand(result)
        return result
    }
    
    moveFocus(newFocus: TSVariable): TSMoveFocusCommand {
        const result = new TSMoveFocusCommand(this.world, newFocus)
        this.doCommand(result)
        return result
    }
    
    doCommandPhrase(commandPhrase: string): TSDoCommandPhrase {
        const result = new TSDoCommandPhrase(this.world, commandPhrase)
        this.doCommand(result)
        return result
    }
    
    ruleFieldChange(rule: TSRule, field: int, newValue: string): TSRuleFieldChange {
        if ((field === TSRuleField.kRuleContext) || (field === TSRuleField.kRuleMove)) {
            if (rule.getTextForField(field).startsWith("new context ")) {
                if (this.world.findVariable(newValue) === null) {
                    const newContextOrMove: TSVariable = this.world.findOrCreateVariable(newValue, false)
                    newContextOrMove.position = rule.context.position
                }
            }
        }
        const result = new TSRuleFieldChange(this.world, rule, field, newValue)
        this.doCommand(result)
        return result
    }
    
}
