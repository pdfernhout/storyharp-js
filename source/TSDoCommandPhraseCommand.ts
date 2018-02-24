import { TSAbstractMoveFocusCommand } from "./TSAbstractMoveFocusCommand"
import { TSChangedVariableWrapper } from "./TSChangedVariableWrapper"
import { int, compareTextIgnoreCase, Color } from "./common"
import { TWorld } from "./TWorld"
import { TSRule } from "./TSRule"
import { TSVariableState } from "./TSVariable"

// TODO: Fix these as imports
import { ConsoleForm, RuleEditorForm } from "./fixTypes"

export class TSDoCommandPhrase extends TSAbstractMoveFocusCommand {
    commandPhrase: string
    changedVariables: TSChangedVariableWrapper[]
    oldLastSaidTextWithMacros: string
    newLastSaidTextWithMacros: string
    oldFirstCommandDoneForLastCommandPhrase: int
    newFirstCommandDoneForLastCommandPhrase: int
    
    constructor(world: TWorld, consoleForm: ConsoleForm, ruleEditorForm: RuleEditorForm, commandPhrase: string) {
        // determine what would need to change - including new focus and all variables
        // Reorganized with temporary variables to work around TypeScript limitation
        // of no access to this before super constructor called.
        const changedVariables: TSChangedVariableWrapper[] = []
        let newFocus = world.emptyEntry
        const oldLastSaidTextWithMacros = consoleForm.speechSystem.lastSaidTextWithMacros
        let newLastSaidTextWithMacros = ""
        const oldFirstCommandDoneForLastCommandPhrase = world.firstCommandDoneForLastCommandPhrase
        let newFirstCommandDoneForLastCommandPhrase = -1
        for (let i = 0; i < world.rules.length; i++) {
            const rule: TSRule = world.rules[i]
            if (rule.available && compareTextIgnoreCase(rule.command.phrase, commandPhrase)) {
                ruleEditorForm.lastCommand = rule
                if (newFirstCommandDoneForLastCommandPhrase === -1) {
                    newFirstCommandDoneForLastCommandPhrase = i
                }
                const ruleResult = rule.recordReplyMoveChanges(changedVariables, newLastSaidTextWithMacros)
                newLastSaidTextWithMacros = ruleResult.totalReply
                if (ruleResult.contextToFocusTo) {
                    newFocus = ruleResult.contextToFocusTo
                }
            }
        }

        if (commandPhrase.startsWith("$")) {
            // elimitate leading $
            commandPhrase = commandPhrase.substring(1)
        }

        super(world, consoleForm, newFocus)

        this.newFocus = newFocus
        this.oldLastSaidTextWithMacros = oldLastSaidTextWithMacros
        this.newLastSaidTextWithMacros = newLastSaidTextWithMacros
        this.oldFirstCommandDoneForLastCommandPhrase  = oldFirstCommandDoneForLastCommandPhrase 
        this.newFirstCommandDoneForLastCommandPhrase = newFirstCommandDoneForLastCommandPhrase
        this.commandPhrase = commandPhrase
        this.changedVariables = changedVariables
    }
    
    doCommand(): void {
        let i: int
        
        this.consoleForm.addLineToTranscript("> " + this.commandPhrase, Color.clRed)
        this.consoleForm.addLineToTranscript(this.consoleForm.speechSystem.stripMacros(this.newLastSaidTextWithMacros), Color.clBlue)
        this.consoleForm.scrollTranscriptEndIntoView()
        this.consoleForm.speechSystem.sayTextWithMacros(this.newLastSaidTextWithMacros)
        //common with undo
        this.consoleForm.speechSystem.lastSaidTextWithMacros = this.newLastSaidTextWithMacros
        this.world.firstCommandDoneForLastCommandPhrase = this.newFirstCommandDoneForLastCommandPhrase
        if (this.newFocus !== this.world.emptyEntry) {
            this.oldFocus.setState(TSVariableState.kAbsent)
            this.world.focus = this.newFocus
            this.newFocus.setState(TSVariableState.kPresent)
        }
        for (i = 0; i < this.changedVariables.length; i++) {
            this.changedVariables[i].doChange()
        }
        this.updateForChanges()
        this.consoleForm.speechSystem.checkForSayOptionsMacro()
        super.doCommand()
    }
    
    undoCommand(): void {
        let i: int
        let undoPhrase: string
        
        this.consoleForm.addLineToTranscript("> undo", Color.clRed)
        //  undoPhrase := 'It is as if "' + commandPhrase + '" had never been said.';
        undoPhrase = "(You decide not to say \"" + this.commandPhrase + "\")"
        this.consoleForm.addLineToTranscript(undoPhrase, Color.clBlue)
        this.consoleForm.scrollTranscriptEndIntoView()
        this.consoleForm.speechSystem.speakText(undoPhrase)
        this.consoleForm.speechSystem.lastSaidTextWithMacros = this.oldLastSaidTextWithMacros
        this.world.firstCommandDoneForLastCommandPhrase = this.oldFirstCommandDoneForLastCommandPhrase
        for (let i = this.changedVariables.length - 1; i >= 0; i--) {
            this.changedVariables[i].undoChange()
        }
        if (this.newFocus !== this.world.emptyEntry) {
            this.newFocus.setState(this.newFocusOldState)
            this.world.focus = this.oldFocus
            this.oldFocus.setState(this.oldFocusOldState)
        }
        this.updateForChanges()
        super.undoCommand()
    }
    
    redoCommand(): void { 
        this.consoleForm.addLineToTranscript("> redo", Color.clRed)
        const redoPhrase = "(You decide to say \"" + this.commandPhrase + "\" anyway)"
        this.consoleForm.addLineToTranscript(redoPhrase, Color.clBlue)
        this.consoleForm.scrollTranscriptEndIntoView()
        this.consoleForm.speechSystem.speakText(redoPhrase)
        //common with do
        this.consoleForm.speechSystem.lastSaidTextWithMacros = this.newLastSaidTextWithMacros
        this.world.firstCommandDoneForLastCommandPhrase = this.newFirstCommandDoneForLastCommandPhrase
        if (this.newFocus !== this.world.emptyEntry) {
            this.oldFocus.setState(TSVariableState.kAbsent)
            this.world.focus = this.newFocus
            this.newFocus.setState(TSVariableState.kPresent)
        }
        for (let i = 0; i < this.changedVariables.length; i++) {
            this.changedVariables[i].doChange()
        }
        this.updateForChanges()
        super.doCommand()
    }
    
    description(): string {
        let result = ""
        result = "command: " + this.commandPhrase
        return result
    }
    
}
