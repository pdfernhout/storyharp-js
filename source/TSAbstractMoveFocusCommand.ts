import { TWorld } from "./TWorld"
import { TSVariable, TSVariableState } from "./TSVariable"
import { KfCommand } from "./KfCommand"
import { TSDomain } from "./TSDomain";

// need to have abstract base so TSDoCommandPhraseCommand can defer updating till after changes
export class TSAbstractMoveFocusCommand extends KfCommand {
    domain: TSDomain
    oldFocus: TSVariable
    oldFocusOldState: TSVariableState
    newFocus: TSVariable
    newFocusOldState: TSVariableState
    
    constructor(domain: TSDomain, newFocus: TSVariable) {
        super()
        this.domain = domain
        // the old states are stored for undo in case author has been toggling them individually
        this.newFocus = newFocus
        this.newFocusOldState = newFocus.getState()
        if (this.domain.world.focus !== null) {
            this.oldFocus = this.domain.world.focus
            this.oldFocusOldState = this.oldFocus.getState()
        } else {
            this.oldFocus = newFocus
            this.oldFocusOldState = newFocus.getState()
        }
    }
    
    updateForChanges(): void {
        this.domain.world.updateAvailable()
        this.domain.speechSystem.listenForAvailableCommands()
    }
    
    shiftsFocus(): boolean {
        let result = false
        result = (this.newFocus !== this.domain.world.emptyEntry) && (this.newFocus !== this.oldFocus)
        return result
    }
    
}
