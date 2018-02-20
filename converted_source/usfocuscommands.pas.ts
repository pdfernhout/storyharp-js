// unit usfocuscommands

from conversion_common import *
import usdomain
import quickfillcombobox
import usmodelchanges
import usworld
import ucommand
import delphi_compatability

const usfocuscommands = usfocuscommands || {}

// record
interface TSelectionInformation {
    text: string
    selStart: int
    selLength: int
}

function RecordSelectionInformation(selectionInformation: TSelectionInformation, control: TControl): void {
    if (control instanceof delphi_compatability.TCustomEdit) {
        selectionInformation.text = (control).Text
        selectionInformation.selStart = (control).selStart
        selectionInformation.selLength = (control).selLength
    } else if (control instanceof quickfillcombobox.TQuickFillComboBox) {
        selectionInformation.text = (control).Text
        selectionInformation.selStart = (control).selStart
        selectionInformation.selLength = (control).selLength
    } else {
        // error
        throw new GeneralException.create("RecordSelectionInformation: unsupported control class")
    }
}

function RestoreSelectionInformation(selectionInformation: TSelectionInformation, control: TControl): void {
    if (control instanceof delphi_compatability.TCustomEdit) {
        (control).Text = selectionInformation.text
        (control).selStart = selectionInformation.selStart
        (control).selLength = selectionInformation.selLength
    } else if (control instanceof quickfillcombobox.TQuickFillComboBox) {
        (control).Text = selectionInformation.text
        (control).selStart = selectionInformation.selStart
        (control).selLength = selectionInformation.selLength
    } else {
        // error
        throw new GeneralException.create("RestoreSelectionInformation: unsupported control class")
    }
}


export class TFocusShiftAction {
    control: TWinControl = new TWinControl()
    form: TForm = new TForm()
    modelChange: TModelChange = new TModelChange()
    
    ///////////////////////////// TFocusShiftAction //////////////////////////
    createWithFormControlModelChange(form: TForm, control: TWinControl, modelChange: TModelChange): void {
        this.form = form
        this.control = control
        this.modelChange = modelChange
    }
    
    destroy(): void {
        this.modelChange.free
        TObject.prototype.destroy.call(this)
    }
    
    // param ignored
    recordState(selectionInformation: TSelectionInformation): void {
        // subclasses should override
    }
    
    // param ignored
    restoreStateWithFocus(): void {
        // form.Active := true;
        this.control.SetFocus()
        // subclasses may override
    }
    
    // assume focus shifted is handled by system the first time
    // assume do must be called before other component is focused to if it is off the form
    doShift(): void {
        if (this.modelChange !== null) {
            // subclasses may override
            this.modelChange.doChange()
        }
    }
    
    undoShift(): void {
        if (this.modelChange !== null) {
            // subclasses may override
            this.modelChange.undoChange()
        }
    }
    
    redoShift(): void {
        if (this.modelChange !== null) {
            this.modelChange.redoChange()
        }
    }
    
}

export class TFocusExitAction {
    TFocusExitAction.prototype = new TFocusShiftAction()
    TFocusExitAction.prototype.constructor = TFocusExitAction
    
    //////////////////////////// TFocusExitAction ////////////////////////////////
    doShift(): void {
        TFocusShiftAction.prototype.doShift.call(this)
    }
    
    // exit moves focus on undo
    undoShift(): void {
        this.restoreStateWithFocus()
        // need to restore selectionInformation in form
        TFocusShiftAction.prototype.undoShift.call(this)
    }
    
    redoShift(): void {
        TFocusShiftAction.prototype.redoShift.call(this)
    }
    
}

export class TFocusEnterAction {
    TFocusEnterAction.prototype = new TFocusShiftAction()
    TFocusEnterAction.prototype.constructor = TFocusEnterAction
    
    //////////////////////////// TFocusEnterAction ////////////////////////////////
    doShift(): void {
        TFocusShiftAction.prototype.doShift.call(this)
    }
    
    undoShift(): void {
        TFocusShiftAction.prototype.undoShift.call(this)
    }
    
    // enter moves focus on redo
    redoShift(): void {
        this.restoreStateWithFocus()
        TFocusShiftAction.prototype.redoShift.call(this)
    }
    
}

export class TTextFocusExitAction {
    selectionInformation: TSelectionInformation = new TSelectionInformation()
    TTextFocusExitAction.prototype = new TFocusExitAction()
    TTextFocusExitAction.prototype.constructor = TTextFocusExitAction
    
    //////////////////////////// TTextFocusExitAction ////////////////////////////////
    recordState(selectionInformation: TSelectionInformation): void {
        TFocusExitAction.prototype.recordState.call(this, selectionInformation)
        this.selectionInformation = selectionInformation
        //RecordSelectionInformation(selectionInformation, control);
    }
    
    restoreStateWithFocus(): void {
        TFocusExitAction.prototype.restoreStateWithFocus.call(this)
        RestoreSelectionInformation(this.selectionInformation, this.control)
    }
    
}

export class TTextFocusEnterAction {
    TTextFocusEnterAction.prototype = new TFocusEnterAction()
    TTextFocusEnterAction.prototype.constructor = TTextFocusEnterAction
    
}

export class TFocusShiftCommand {
    focusExitAction: TFocusExitAction = new TFocusExitAction()
    focusEnterAction: TFocusEnterAction = new TFocusEnterAction()
    TFocusShiftCommand.prototype = new KfCommand()
    TFocusShiftCommand.prototype.constructor = TFocusShiftCommand
    
    //////////////////////////// TTextFocusEnterAction ////////////////////////////////
    //////////////////////////// TFocusShiftCommand ////////////////////////////////
    createWithFocusExitAction(focusExitAction: TFocusExitAction): void {
        this.create()
        this.focusExitAction = focusExitAction
        //self.focusExitAction.recordState;
        // responsibility of component being focused to call setFocusEnterAction
    }
    
    setFocusEnterAction(focusEnterAction: TFocusEnterAction): void {
        let selectionInformation: TSelectionInformation
        
        this.focusEnterAction = focusEnterAction
        // param ignored
        this.focusEnterAction.recordState(selectionInformation)
    }
    
    destroy(): void {
        this.focusExitAction.free
        this.focusEnterAction.free
        KfCommand.prototype.destroy.call(this)
    }
    
    doCommand(): void {
        KfCommand.prototype.doCommand.call(this)
        usdomain.domain.beginUpdate()
        this.focusExitAction.doShift()
        if (this.focusEnterAction !== null) {
            this.focusEnterAction.doShift()
        }
        usdomain.domain.endUpdate()
    }
    
    undoCommand(): void {
        KfCommand.prototype.undoCommand.call(this)
        usdomain.domain.beginUpdate()
        if (this.focusEnterAction !== null) {
            this.focusEnterAction.undoShift()
        }
        this.focusExitAction.undoShift()
        usdomain.domain.endUpdate()
    }
    
    redoCommand(): void {
        KfCommand.prototype.doCommand.call(this)
        usdomain.domain.beginUpdate()
        this.focusExitAction.redoShift()
        if (this.focusEnterAction !== null) {
            this.focusEnterAction.redoShift()
        }
        usdomain.domain.endUpdate()
    }
    
    description(): string {
        let result = ""
        result = "editor focus shift"
        return result
    }
    
}

