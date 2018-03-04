import { int } from "./common"
import { TSDraggableObject } from "./TSDraggableObject"
import { TWorld } from "./TWorld"
import { TSRuleField } from "./TSRule"

export enum TSVariableState { kPresent, kAbsent }

export type TSVariableDisplayOptions = boolean[] /* 5 + 1 */

export class TSVariable extends TSDraggableObject {
    phrase: string = ""
    state: TSVariableState = TSVariableState.kAbsent
    contextUseages: int = 0
    requirementsUseages: int = 0
    commandUseages: int = 0
    moveUseages: int = 0
    changesUseages: int = 0
    // for java creation
    indexInVariables: int = 0
    
    displayName(): string {
        return this.phrase
    }
    
    setPhrase(aPhrase: string): void {
        this.phrase = aPhrase
    }
    
    setState(newState: TSVariableState): void {
        // TODO: Should we make a defensive copy?
        this.state = newState
    }
    
    getState(): TSVariableState {
        // TODO: Should we make a defensive copy?
        return this.state
    }
    
    hasUseagesForField(col: int): boolean {
        let result = false
        switch (col) {
            case TSRuleField.kRuleContext:
                result = (this.contextUseages > 0) || (this.moveUseages > 0)
                break
            case TSRuleField.kRuleCommand:
                result = this.commandUseages > 0
                break
            case TSRuleField.kRuleReply:
                result = false
                break
            case TSRuleField.kRuleMove:
                result = this.moveUseages > 0
                break
            case TSRuleField.kRuleRequirements:
                result = this.requirementsUseages > 0
                break
            case TSRuleField.kRuleChanges:
                result = this.changesUseages > 0
                break
            default:
                throw new Error("Unexpected case")
        }
        return result
    }
    
    meetsDisplayOptions(displayOptions: TSVariableDisplayOptions): boolean {
        for (let i = 0; i <= 5; i++) {
            if (i === TSRuleField.kRuleCommand) {
                // don't display commands for now - used to display rules
                continue
            }
            if (this.hasUseagesForField(i) && displayOptions[i]) {
                return true
            }
        }
        return false
    }
}
