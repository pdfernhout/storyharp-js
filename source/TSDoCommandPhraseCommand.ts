import { TSAbstractMoveFocusCommand } from "./TSAbstractMoveFocusCommand"
import { TSChangedVariableWrapper } from "./TSChangedVariableWrapper"
import { int, compareTextIgnoreCase, Color } from "./common"
import { TSRule } from "./TSRule"
import { TSVariableState } from "./TSVariable"
import { TSDomain } from "./TSDomain"

export class TSDoCommandPhraseCommand extends TSAbstractMoveFocusCommand {
    commandPhrase: string
    changedVariables: TSChangedVariableWrapper[]
    oldLastSaidTextWithMacros: string
    newLastSaidTextWithMacros: string
    oldFirstCommandDoneForLastCommandPhrase: int
    newFirstCommandDoneForLastCommandPhrase: int
    
    constructor(domain: TSDomain, commandPhrase: string) {
        // determine what would need to change - including new focus and all variables
        // Reorganized with temporary variables to work around TypeScript limitation
        // of no access to this before super constructor called.
        const world = domain.world
        const changedVariables: TSChangedVariableWrapper[] = []
        let newFocus = world.emptyEntry
        const oldLastSaidTextWithMacros = domain.speechSystem.lastSaidTextWithMacros
        let newLastSaidTextWithMacros = ""
        const oldFirstCommandDoneForLastCommandPhrase = world.firstCommandDoneForLastCommandPhrase
        let newFirstCommandDoneForLastCommandPhrase = -1
        for (let i = 0; i < world.rules.length; i++) {
            const rule: TSRule = world.rules[i]
            if (rule.available && compareTextIgnoreCase(rule.command.phrase, commandPhrase)) {
                domain.ruleEditorForm.lastCommand = rule
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

        super(domain, newFocus)

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
        
        this.domain.consoleForm.addLineToTranscript("> " + this.commandPhrase, Color.clBlue)
        this.domain.consoleForm.addLineToTranscript(this.domain.speechSystem.stripMacros(this.newLastSaidTextWithMacros), Color.clBlack)
        this.domain.consoleForm.scrollTranscriptEndIntoView()
        this.domain.speechSystem.sayTextWithMacros(this.newLastSaidTextWithMacros)
        //common with undo
        this.domain.speechSystem.lastSaidTextWithMacros = this.newLastSaidTextWithMacros
        this.domain.world.firstCommandDoneForLastCommandPhrase = this.newFirstCommandDoneForLastCommandPhrase
        if (this.newFocus !== this.domain.world.emptyEntry) {
            this.oldFocus.setState(TSVariableState.kAbsent)
            this.domain.world.focus = this.newFocus
            this.newFocus.setState(TSVariableState.kPresent)
        }
        for (i = 0; i < this.changedVariables.length; i++) {
            this.changedVariables[i].doChange()
        }
        this.updateForChanges()
        this.domain.speechSystem.checkForSayOptionsMacro()
        super.doCommand()
    }
    
    undoCommand(): void {
        let i: int
        let undoPhrase: string
        
        this.domain.consoleForm.addLineToTranscript("> undo", Color.clBlue)
        //  undoPhrase := 'It is as if "' + commandPhrase + '" had never been said.'
        undoPhrase = "(You decide not to say \"" + this.commandPhrase + "\")"
        this.domain.consoleForm.addLineToTranscript(undoPhrase, Color.clBlue)
        this.domain.consoleForm.scrollTranscriptEndIntoView()
        this.domain.speechSystem.speakText(undoPhrase)
        this.domain.speechSystem.lastSaidTextWithMacros = this.oldLastSaidTextWithMacros
        this.domain.world.firstCommandDoneForLastCommandPhrase = this.oldFirstCommandDoneForLastCommandPhrase
        for (let i = this.changedVariables.length - 1; i >= 0; i--) {
            this.changedVariables[i].undoChange()
        }
        if (this.newFocus !== this.domain.world.emptyEntry) {
            this.newFocus.setState(this.newFocusOldState)
            this.domain.world.focus = this.oldFocus
            this.oldFocus.setState(this.oldFocusOldState)
        }
        this.updateForChanges()
        super.undoCommand()
    }
    
    redoCommand(): void { 
        this.domain.consoleForm.addLineToTranscript("> redo", Color.clBlue)
        const redoPhrase = "(You decide to say \"" + this.commandPhrase + "\" anyway)"
        this.domain.consoleForm.addLineToTranscript(redoPhrase, Color.clBlue)
        this.domain.consoleForm.scrollTranscriptEndIntoView()
        this.domain.speechSystem.speakText(redoPhrase)
        //common with do
        this.domain.speechSystem.lastSaidTextWithMacros = this.newLastSaidTextWithMacros
        this.domain.world.firstCommandDoneForLastCommandPhrase = this.newFirstCommandDoneForLastCommandPhrase
        if (this.newFocus !== this.domain.world.emptyEntry) {
            this.oldFocus.setState(TSVariableState.kAbsent)
            this.domain.world.focus = this.newFocus
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
