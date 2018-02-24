import { TSAbstractMoveFocusCommand } from "./TSAbstractMoveFocusCommand"
import { TSVariableState } from "./TSVariable"

export class TSMoveFocusCommand extends TSAbstractMoveFocusCommand {
    
    doCommand(): void {
        this.oldFocus.setState(TSVariableState.kAbsent)
        this.world.focus = this.newFocus
        this.newFocus.setState(TSVariableState.kPresent)
        this.updateForChanges()
        TSAbstractMoveFocusCommand.prototype.doCommand.call(this)
    }
    
    undoCommand(): void {
        this.newFocus.setState(this.newFocusOldState)
        this.world.focus = this.oldFocus
        this.oldFocus.setState(this.oldFocusOldState)
        this.updateForChanges()
        TSAbstractMoveFocusCommand.prototype.undoCommand.call(this)
    }
    
    description(): string {
        let result = ""
        result = "move focus to " + this.newFocus.phrase
        return result
    }
    
}
