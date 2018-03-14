import { TWorld } from "./TWorld"
import { KfCommand } from "./KfCommand"
import { TSVariable, TSVariableState } from "./TSVariable"
import { TSDomain } from "./TSDomain"

export class TSToggleVariableCommand extends KfCommand {
    domain: TSDomain
    variable: TSVariable = new TSVariable()
    oldState: TSVariableState
    newState: TSVariableState
    
    constructor(domain: TSDomain, variable: TSVariable) {
        super()
        this.domain = domain
        this.variable = variable
        this.oldState = variable.getState()
        if (this.oldState === TSVariableState.kPresent) {
            this.newState = TSVariableState.kAbsent
        } else {
            this.newState = TSVariableState.kPresent
        }
    }
    
    setVariableStateWithUpdate(state: TSVariableState): void {
        this.variable.setState(state)
        /* TODO: Implement a current focused variable in variables list box and update this code
        if (this.domain.consoleForm.ShowOnlyTrueVariablesButton.Down) {
            this.domain.consoleForm.VariablesListBox.ItemIndex = this.domain.consoleForm.VariablesListBox.Items.IndexOfObject(this.variable)
        }
        */
        this.domain.world.updateAvailable()
        this.domain.speechSystem.listenForAvailableCommands()
    }
    
    doCommand(): void {
        this.setVariableStateWithUpdate(this.newState)
        KfCommand.prototype.doCommand.call(this)
    }
    
    undoCommand(): void {
        this.setVariableStateWithUpdate(this.oldState)
        KfCommand.prototype.undoCommand.call(this)
    }
    
    description(): string {
        let result = ""
        if (this.newState === TSVariableState.kPresent) {
            result = "toggle \"" + this.variable.phrase + "\" to true"
        } else {
            result = "toggle \"" + this.variable.phrase + "\" to false"
        }
        return result
    }
    
}
