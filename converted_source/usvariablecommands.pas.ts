// unit usvariablecommands

from conversion_common import *
import usruleeditorform
import usdomain
import usconsoleform
import usworld
import ucommand
import delphi_compatability

const usvariablecommands = usvariablecommands || {}


export class TSToggleVariableCommand {
    variable: TSVariable = new TSVariable()
    oldState: TSVariableState = new TSVariableState()
    newState: TSVariableState = new TSVariableState()
    TSToggleVariableCommand.prototype = new KfCommand()
    TSToggleVariableCommand.prototype.constructor = TSToggleVariableCommand
    
    // ----------------------------- TSToggleVariableCommand --------------------- 
    createWithVariable(variable: TSVariable): void {
        this.create()
        this.variable = variable
        this.oldState = variable.getState()
        if (this.oldState === usworld.TSVariableState.kPresent) {
            this.newState = usworld.TSVariableState.kAbsent
        } else {
            this.newState = usworld.TSVariableState.kPresent
        }
    }
    
    setVariableStateWithUpdate(state: TSVariableState): void {
        this.variable.setState(state)
        if (usconsoleform.ConsoleForm.ShowOnlyTrueVariablesButton.Down) {
            usconsoleform.ConsoleForm.updateVariables()
            usconsoleform.ConsoleForm.VariablesListBox.ItemIndex = usconsoleform.ConsoleForm.VariablesListBox.Items.IndexOfObject(this.variable)
            usconsoleform.ConsoleForm.VariablesListBox.Invalidate()
        } else {
            usconsoleform.ConsoleForm.VariablesListBox.Invalidate()
        }
        usdomain.domain.world.updateAvailable()
        usconsoleform.ConsoleForm.speechSystem.listenForAvailableCommands()
    }
    
    doCommand(): void {
        this.setVariableStateWithUpdate(this.newState)
        KfCommand.prototype.doCommand.call(this)
    }
    
    undoCommand(): void {
        this.setVariableStateWithUpdate(this.oldState)
        KfCommand.prototype.undoCommand.call(this)
    }
    
    description(): String {
        let result = ""
        if (this.newState === usworld.TSVariableState.kPresent) {
            result = "toggle \"" + this.variable.phrase + "\" to true"
        } else {
            result = "toggle \"" + this.variable.phrase + "\" to false"
        }
        return result
    }
    
}

// need to have abstract base so TSDoCommandPhrase can defer updating till after changes
export class TSAbstractMoveFocusCommand {
    oldFocus: TSVariable = new TSVariable()
    oldFocusOldState: TSVariableState = new TSVariableState()
    newFocus: TSVariable = new TSVariable()
    newFocusOldState: TsVariableState = new TsVariableState()
    TSAbstractMoveFocusCommand.prototype = new KfCommand()
    TSAbstractMoveFocusCommand.prototype.constructor = TSAbstractMoveFocusCommand
    
    // ----------------------------- TSAbstractMoveFocusCommand --------------------- 
    createWithNewFocus(newFocus: TSVariable): void {
        this.create()
        // the old states are stored for undo in case author has been toggling them individually
        this.newFocus = newFocus
        this.newFocusOldState = newFocus.getState()
        if (usdomain.domain.world.focus !== null) {
            this.oldFocus = usdomain.domain.world.focus
            this.oldFocusOldState = this.oldFocus.getState()
        } else {
            this.oldFocus = newFocus
            this.oldFocusOldState = newFocus.getState()
        }
    }
    
    updateForChanges(): void {
        usdomain.domain.world.updateAvailable()
        usconsoleform.ConsoleForm.speechSystem.listenForAvailableCommands()
        usconsoleform.ConsoleForm.updateVariables()
        usconsoleform.ConsoleForm.VariablesListBox.Invalidate()
    }
    
    shiftsFocus(): boolean {
        let result = false
        result = (this.newFocus !== usdomain.domain.world.emptyEntry) && (this.newFocus !== this.oldFocus)
        return result
    }
    
}

export class TSMoveFocusCommand {
    TSMoveFocusCommand.prototype = new TSAbstractMoveFocusCommand()
    TSMoveFocusCommand.prototype.constructor = TSMoveFocusCommand
    
    // ----------------------------- TSMoveFocusCommand --------------------- 
    doCommand(): void {
        this.oldFocus.setState(usworld.TSVariableState.kAbsent)
        usdomain.domain.world.focus = this.newFocus
        this.newFocus.setState(usworld.TSVariableState.kPresent)
        this.updateForChanges()
        TSAbstractMoveFocusCommand.prototype.doCommand.call(this)
    }
    
    undoCommand(): void {
        this.newFocus.setState(this.newFocusOldState)
        usdomain.domain.world.focus = this.oldFocus
        this.oldFocus.setState(this.oldFocusOldState)
        this.updateForChanges()
        TSAbstractMoveFocusCommand.prototype.undoCommand.call(this)
    }
    
    description(): String {
        let result = ""
        result = "move focus to " + this.newFocus.phrase
        return result
    }
    
}

export class TSDoCommandPhrase {
    commandPhrase: String = ""
    changedVariables: TList = new TList()
    oldLastSaidTextWithMacros: String = ""
    newLastSaidTextWithMacros: String = ""
    oldFirstCommandDoneForLastCommandPhrase: int = 0
    newFirstCommandDoneForLastCommandPhrase: int = 0
    TSDoCommandPhrase.prototype = new TSAbstractMoveFocusCommand()
    TSDoCommandPhrase.prototype.constructor = TSDoCommandPhrase
    
    // ----------------------------- TSDoCommandPhrase -------------------------------
    createWithCommandPhrase(commandPhrase: String): void {
        let i: int
        let rule: TSRule
        
        this.commandPhrase = commandPhrase
        this.changedVariables = delphi_compatability.TList().Create()
        // determine what would need to change - including new focus and all variables
        this.newFocus = usdomain.domain.world.emptyEntry
        this.oldLastSaidTextWithMacros = usconsoleform.ConsoleForm.speechSystem.lastSaidTextWithMacros
        this.newLastSaidTextWithMacros = ""
        this.oldFirstCommandDoneForLastCommandPhrase = usdomain.domain.world.firstCommandDoneForLastCommandPhrase
        this.newFirstCommandDoneForLastCommandPhrase = -1
        if (usdomain.domain.world.rules.Count > 0) {
            for (i = 0; i <= usdomain.domain.world.rules.Count - 1; i++) {
                rule = usworld.TSRule(usdomain.domain.world.rules.Items[i])
                if (rule.available && (UNRESOLVED.AnsiCompareText(rule.command.phrase, commandPhrase) === 0)) {
                    usruleeditorform.RuleEditorForm.lastCommand = rule
                    if (this.newFirstCommandDoneForLastCommandPhrase === -1) {
                        this.newFirstCommandDoneForLastCommandPhrase = i
                    }
                    this.newLastSaidTextWithMacros = rule.recordReplyMoveChanges(this.changedVariables, this.newLastSaidTextWithMacros, this.newFocus)
                }
            }
        }
        if ((len(this.commandPhrase) > 1) && (this.commandPhrase[1] === "$")) {
            // elimitate leading $
            this.commandPhrase = UNRESOLVED.copy(this.commandPhrase, 2, len(this.commandPhrase))
        }
        this.createWithNewFocus(this.newFocus)
    }
    
    destroy(): void {
        let i: int
        
        for (i = 0; i <= this.changedVariables.Count - 1; i++) {
            usworld.TSChangedVariableWrapper(this.changedVariables[i]).free
        }
        this.changedVariables.free
        this.changedVariables = null
        TSAbstractMoveFocusCommand.prototype.destroy.call(this)
    }
    
    doCommand(): void {
        let i: int
        
        usconsoleform.ConsoleForm.addLineToTranscript("> " + this.commandPhrase, delphi_compatability.clRed)
        usconsoleform.ConsoleForm.addLineToTranscript(usconsoleform.ConsoleForm.speechSystem.stripMacros(this.newLastSaidTextWithMacros), delphi_compatability.clBlue)
        usconsoleform.ConsoleForm.scrollTranscriptEndIntoView()
        usconsoleform.ConsoleForm.speechSystem.sayTextWithMacros(this.newLastSaidTextWithMacros)
        //common with undo
        usconsoleform.ConsoleForm.speechSystem.lastSaidTextWithMacros = this.newLastSaidTextWithMacros
        usdomain.domain.world.firstCommandDoneForLastCommandPhrase = this.newFirstCommandDoneForLastCommandPhrase
        if (this.newFocus !== usdomain.domain.world.emptyEntry) {
            this.oldFocus.setState(usworld.TSVariableState.kAbsent)
            usdomain.domain.world.focus = this.newFocus
            this.newFocus.setState(usworld.TSVariableState.kPresent)
        }
        for (i = 0; i <= this.changedVariables.Count - 1; i++) {
            usworld.TSChangedVariableWrapper(this.changedVariables[i]).doChange()
        }
        this.updateForChanges()
        usconsoleform.ConsoleForm.speechSystem.checkForSayOptionsMacro()
        TSAbstractMoveFocusCommand.prototype.doCommand.call(this)
    }
    
    undoCommand(): void {
        let i: int
        let undoPhrase: String
        
        usconsoleform.ConsoleForm.addLineToTranscript("> undo", delphi_compatability.clRed)
        //  undoPhrase := 'It is as if "' + commandPhrase + '" had never been said.';
        undoPhrase = "(You decide not to say \"" + this.commandPhrase + "\")"
        usconsoleform.ConsoleForm.addLineToTranscript(undoPhrase, delphi_compatability.clBlue)
        usconsoleform.ConsoleForm.scrollTranscriptEndIntoView()
        usconsoleform.ConsoleForm.speechSystem.speakText(undoPhrase)
        usconsoleform.ConsoleForm.speechSystem.lastSaidTextWithMacros = this.oldLastSaidTextWithMacros
        usdomain.domain.world.firstCommandDoneForLastCommandPhrase = this.oldFirstCommandDoneForLastCommandPhrase
        for (i = this.changedVariables.Count - 1; i >= 0; i--) {
            usworld.TSChangedVariableWrapper(this.changedVariables[i]).undoChange()
        }
        if (this.newFocus !== usdomain.domain.world.emptyEntry) {
            this.newFocus.setState(this.newFocusOldState)
            usdomain.domain.world.focus = this.oldFocus
            this.oldFocus.setState(this.oldFocusOldState)
        }
        this.updateForChanges()
        TSAbstractMoveFocusCommand.prototype.undoCommand.call(this)
    }
    
    redoCommand(): void {
        let i: int
        let redoPhrase: String
        
        usconsoleform.ConsoleForm.addLineToTranscript("> redo", delphi_compatability.clRed)
        redoPhrase = "(You decide to say \"" + this.commandPhrase + "\" anyway)"
        usconsoleform.ConsoleForm.addLineToTranscript(redoPhrase, delphi_compatability.clBlue)
        usconsoleform.ConsoleForm.scrollTranscriptEndIntoView()
        usconsoleform.ConsoleForm.speechSystem.speakText(redoPhrase)
        //common with do
        usconsoleform.ConsoleForm.speechSystem.lastSaidTextWithMacros = this.newLastSaidTextWithMacros
        usdomain.domain.world.firstCommandDoneForLastCommandPhrase = this.newFirstCommandDoneForLastCommandPhrase
        if (this.newFocus !== usdomain.domain.world.emptyEntry) {
            this.oldFocus.setState(usworld.TSVariableState.kAbsent)
            usdomain.domain.world.focus = this.newFocus
            this.newFocus.setState(usworld.TSVariableState.kPresent)
        }
        for (i = 0; i <= this.changedVariables.Count - 1; i++) {
            usworld.TSChangedVariableWrapper(this.changedVariables[i]).doChange()
        }
        this.updateForChanges()
        TSAbstractMoveFocusCommand.prototype.doCommand.call(this)
    }
    
    description(): String {
        let result = ""
        result = "command: " + this.commandPhrase
        return result
    }
    
}

