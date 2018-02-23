import { TSVariable, TSVariableState } from "./TSVariable"

export class TSChangedVariableWrapper {
    variable: TSVariable
    oldState: TSVariableState
    newState: TSVariableState
    
    constructor(variable: TSVariable, newState: TSVariableState) {
        this.variable = variable
        this.newState = newState
        this.oldState = variable.getState()
    }
    
    doChange(): void {
        this.variable.setState(this.newState)
    }
    
    undoChange(): void {
        this.variable.setState(this.oldState)
    }
    
}
