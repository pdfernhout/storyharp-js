import { TSVariable, TSVariableState } from "./TSVariable"

export class TSDesiredStateVariableWrapper {
    variable: TSVariable
    desiredState: TSVariableState

    constructor(variable: TSVariable, desiredState: TSVariableState) {
        this.variable = variable
        this.desiredState = desiredState
    }
    
    leader(): string {
        if (this.desiredState === TSVariableState.kAbsent) {
            return "~"
        } else {
            return ""
        }
    }
    
    displayLeader(): string {
        if (this.desiredState === TSVariableState.kAbsent) {
            return "~"
        } else {
            return "  "
        }
    }
    
    invertDesiredState(): void {
        if (this.desiredState === TSVariableState.kAbsent) {
            this.desiredState = TSVariableState.kPresent
        } else {
            this.desiredState = TSVariableState.kAbsent
        }
    }
    
    displayString(): string {
        return this.displayLeader() + this.variable.phrase
    }
}
