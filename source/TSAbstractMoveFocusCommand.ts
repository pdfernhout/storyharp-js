import { TWorld } from "./TWorld";
import { TSVariable, TSVariableState } from "./TSVariable";
import { KfCommand } from "./KfCommand";

// TODO: Fix these as imports
import { ConsoleForm } from "./fixTypes"

// need to have abstract base so TSDoCommandPhrase can defer updating till after changes
export class TSAbstractMoveFocusCommand extends KfCommand {
    world: TWorld
    consoleForm: ConsoleForm
    oldFocus: TSVariable
    oldFocusOldState: TSVariableState
    newFocus: TSVariable
    newFocusOldState: TSVariableState
    
    constructor(world: TWorld, consoleForm: ConsoleForm, newFocus: TSVariable) {
        super()
        this.world = world
        this.consoleForm = consoleForm
        // the old states are stored for undo in case author has been toggling them individually
        this.newFocus = newFocus
        this.newFocusOldState = newFocus.getState()
        if (this.world.focus !== null) {
            this.oldFocus = this.world.focus
            this.oldFocusOldState = this.oldFocus.getState()
        } else {
            this.oldFocus = newFocus
            this.oldFocusOldState = newFocus.getState()
        }
    }
    
    updateForChanges(): void {
        this.world.updateAvailable()
        this.consoleForm.speechSystem.listenForAvailableCommands()
        this.consoleForm.updateVariables()
        this.consoleForm.VariablesListBox.Invalidate()
    }
    
    shiftsFocus(): boolean {
        let result = false
        result = (this.newFocus !== this.world.emptyEntry) && (this.newFocus !== this.oldFocus)
        return result
    }
    
}
