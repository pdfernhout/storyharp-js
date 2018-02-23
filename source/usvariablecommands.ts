// unit usvariablecommands

import { int, compareTextIgnoreCase, Color } from "./common"
import { KfCommand } from "./KfCommand"
import { TSVariable, TSVariableState } from "./TSVariable"
import { TSChangedVariableWrapper } from "./TSChangedVariableWrapper"
import { TSRule } from "./TSRule"

// TODO: FIX THIS
const usconsoleform: any = {};
const usdomain: any = {};
const usruleeditorform: any = {};

/*
from conversion_common import *
import usruleeditorform
import usdomain
import usconsoleform
import usworld
import ucommand
import delphi_compatability
*/

export class TSToggleVariableCommand extends KfCommand {
    variable: TSVariable = new TSVariable()
    oldState: TSVariableState
    newState: TSVariableState
    
    // ----------------------------- TSToggleVariableCommand --------------------- 
    constructor(variable: TSVariable) {
        super()
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

// need to have abstract base so TSDoCommandPhrase can defer updating till after changes
export class TSAbstractMoveFocusCommand extends KfCommand {
    oldFocus: TSVariable
    oldFocusOldState: TSVariableState
    newFocus: TSVariable
    newFocusOldState: TSVariableState
    
    // ----------------------------- TSAbstractMoveFocusCommand --------------------- 
    constructor(newFocus: TSVariable) {
        super()
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

export class TSMoveFocusCommand extends TSAbstractMoveFocusCommand {
    
    // ----------------------------- TSMoveFocusCommand --------------------- 
    doCommand(): void {
        this.oldFocus.setState(TSVariableState.kAbsent)
        usdomain.domain.world.focus = this.newFocus
        this.newFocus.setState(TSVariableState.kPresent)
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
    
    description(): string {
        let result = ""
        result = "move focus to " + this.newFocus.phrase
        return result
    }
    
}

export class TSDoCommandPhrase extends TSAbstractMoveFocusCommand {
    commandPhrase: string
    changedVariables: TSChangedVariableWrapper[]
    oldLastSaidTextWithMacros: string
    newLastSaidTextWithMacros: string
    oldFirstCommandDoneForLastCommandPhrase: int
    newFirstCommandDoneForLastCommandPhrase: int
    
    // ----------------------------- TSDoCommandPhrase -------------------------------
    constructor(commandPhrase: string) {
        // determine what would need to change - including new focus and all variables
        // Reorganized with temporary variables to work around TypeScript limitation
        // of no access to this before super constructor called.
        const changedVariables: TSChangedVariableWrapper[] = []
        let newFocus = usdomain.domain.world.emptyEntry
        const oldLastSaidTextWithMacros = usconsoleform.ConsoleForm.speechSystem.lastSaidTextWithMacros
        let newLastSaidTextWithMacros = ""
        const oldFirstCommandDoneForLastCommandPhrase = usdomain.domain.world.firstCommandDoneForLastCommandPhrase
        let newFirstCommandDoneForLastCommandPhrase = -1
        for (let i = 0; i < usdomain.domain.world.rules.length; i++) {
            const rule: TSRule = usdomain.domain.world.rules[i]
            if (rule.available && compareTextIgnoreCase(rule.command.phrase, commandPhrase)) {
                usruleeditorform.RuleEditorForm.lastCommand = rule
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

        super(newFocus)

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
        
        usconsoleform.ConsoleForm.addLineToTranscript("> " + this.commandPhrase, Color.clRed)
        usconsoleform.ConsoleForm.addLineToTranscript(usconsoleform.ConsoleForm.speechSystem.stripMacros(this.newLastSaidTextWithMacros), Color.clBlue)
        usconsoleform.ConsoleForm.scrollTranscriptEndIntoView()
        usconsoleform.ConsoleForm.speechSystem.sayTextWithMacros(this.newLastSaidTextWithMacros)
        //common with undo
        usconsoleform.ConsoleForm.speechSystem.lastSaidTextWithMacros = this.newLastSaidTextWithMacros
        usdomain.domain.world.firstCommandDoneForLastCommandPhrase = this.newFirstCommandDoneForLastCommandPhrase
        if (this.newFocus !== usdomain.domain.world.emptyEntry) {
            this.oldFocus.setState(TSVariableState.kAbsent)
            usdomain.domain.world.focus = this.newFocus
            this.newFocus.setState(TSVariableState.kPresent)
        }
        for (i = 0; i < this.changedVariables.length; i++) {
            this.changedVariables[i].doChange()
        }
        this.updateForChanges()
        usconsoleform.ConsoleForm.speechSystem.checkForSayOptionsMacro()
        super.doCommand()
    }
    
    undoCommand(): void {
        let i: int
        let undoPhrase: string
        
        usconsoleform.ConsoleForm.addLineToTranscript("> undo", Color.clRed)
        //  undoPhrase := 'It is as if "' + commandPhrase + '" had never been said.';
        undoPhrase = "(You decide not to say \"" + this.commandPhrase + "\")"
        usconsoleform.ConsoleForm.addLineToTranscript(undoPhrase, Color.clBlue)
        usconsoleform.ConsoleForm.scrollTranscriptEndIntoView()
        usconsoleform.ConsoleForm.speechSystem.speakText(undoPhrase)
        usconsoleform.ConsoleForm.speechSystem.lastSaidTextWithMacros = this.oldLastSaidTextWithMacros
        usdomain.domain.world.firstCommandDoneForLastCommandPhrase = this.oldFirstCommandDoneForLastCommandPhrase
        for (let i = this.changedVariables.length - 1; i >= 0; i--) {
            this.changedVariables[i].undoChange()
        }
        if (this.newFocus !== usdomain.domain.world.emptyEntry) {
            this.newFocus.setState(this.newFocusOldState)
            usdomain.domain.world.focus = this.oldFocus
            this.oldFocus.setState(this.oldFocusOldState)
        }
        this.updateForChanges()
        super.undoCommand()
    }
    
    redoCommand(): void { 
        usconsoleform.ConsoleForm.addLineToTranscript("> redo", Color.clRed)
        const redoPhrase = "(You decide to say \"" + this.commandPhrase + "\" anyway)"
        usconsoleform.ConsoleForm.addLineToTranscript(redoPhrase, Color.clBlue)
        usconsoleform.ConsoleForm.scrollTranscriptEndIntoView()
        usconsoleform.ConsoleForm.speechSystem.speakText(redoPhrase)
        //common with do
        usconsoleform.ConsoleForm.speechSystem.lastSaidTextWithMacros = this.newLastSaidTextWithMacros
        usdomain.domain.world.firstCommandDoneForLastCommandPhrase = this.newFirstCommandDoneForLastCommandPhrase
        if (this.newFocus !== usdomain.domain.world.emptyEntry) {
            this.oldFocus.setState(TSVariableState.kAbsent)
            usdomain.domain.world.focus = this.newFocus
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

