import { TWorld } from "./TWorld";
import { KfCommand } from "./KfCommand";
import { TSVariable, TSVariableState } from "./TSVariable";

// TODO: Fix these as imports
import { ConsoleForm } from "./fixTypes"

export class TSToggleVariableCommand extends KfCommand {
    world: TWorld
    consoleForm: ConsoleForm
    variable: TSVariable = new TSVariable()
    oldState: TSVariableState
    newState: TSVariableState
    
    constructor(world: TWorld, consoleForm: ConsoleForm, variable: TSVariable) {
        super()
        this.world = world
        this.consoleForm = consoleForm
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
        if (this.consoleForm.ShowOnlyTrueVariablesButton.Down) {
            this.consoleForm.updateVariables()
            this.consoleForm.VariablesListBox.ItemIndex = this.consoleForm.VariablesListBox.Items.IndexOfObject(this.variable)
            this.consoleForm.VariablesListBox.Invalidate()
        } else {
            this.consoleForm.VariablesListBox.Invalidate()
        }
        this.world.updateAvailable()
        this.consoleForm.speechSystem.listenForAvailableCommands()
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
