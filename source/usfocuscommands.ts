// unit usfocuscommands

import { int } from "./common"
import { KfCommand } from "./KfCommand"

// TODO: Remove
const usdomain: any = {}
type TWinControl = any
type TForm = any
type TModelChange = any
class TControl {Text: string; selStart: number; selLength: number}
class TCustomEdit extends TControl {}
class TQuickFillComboBox extends TControl {}

export interface TSelectionInformation {
    text: string
    selStart: int
    selLength: int
}

function RecordSelectionInformation(selectionInformation: TSelectionInformation, control: TControl): void {
    if (control instanceof TCustomEdit) {
        selectionInformation.text = control.Text
        selectionInformation.selStart = control.selStart
        selectionInformation.selLength = control.selLength
    } else if (control instanceof TQuickFillComboBox) {
        selectionInformation.text = control.Text
        selectionInformation.selStart = control.selStart
        selectionInformation.selLength = control.selLength
    } else {
        // error
        throw new Error("RecordSelectionInformation: unsupported control class")
    }
}

function RestoreSelectionInformation(selectionInformation: TSelectionInformation, control: TControl): void {
    if (control instanceof TCustomEdit) {
        control.Text = selectionInformation.text
        control.selStart = selectionInformation.selStart
        control.selLength = selectionInformation.selLength
    } else if (control instanceof TQuickFillComboBox) {
        control.Text = selectionInformation.text
        control.selStart = selectionInformation.selStart
        control.selLength = selectionInformation.selLength
    } else {
        // error
        throw new Error("RestoreSelectionInformation: unsupported control class")
    }
}

export class TFocusShiftAction {
    control: TWinControl
    form: TForm
    modelChange: TModelChange
    
    // createWithFormControlModelChange
    constructor(form: TForm, control: TWinControl, modelChange: TModelChange) {
        this.form = form
        this.control = control
        this.modelChange = modelChange
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

export class TFocusExitAction extends TFocusShiftAction {
    
    doShift(): void {
        super.doShift()
    }
    
    // exit moves focus on undo
    undoShift(): void {
        this.restoreStateWithFocus()
        // need to restore selectionInformation in form
        super.undoShift()
    }
    
    redoShift(): void {
        super.redoShift()
    }
    
}

export class TFocusEnterAction extends TFocusShiftAction {
    
    doShift(): void {
        super.doShift()
    }
    
    undoShift(): void {
        super.undoShift()
    }
    
    // enter moves focus on redo
    redoShift(): void {
        this.restoreStateWithFocus()
        super.redoShift()
    }
    
}

export class TTextFocusExitAction extends TFocusExitAction {
    selectionInformation: TSelectionInformation
    
    recordState(selectionInformation: TSelectionInformation): void {
        super.recordState(selectionInformation)
        this.selectionInformation = selectionInformation
        // RecordSelectionInformation(selectionInformation, control)
    }
    
    restoreStateWithFocus(): void {
        super.restoreStateWithFocus()
        RestoreSelectionInformation(this.selectionInformation, this.control)
    }
    
}

export class TTextFocusEnterAction extends TFocusEnterAction {
    
}

export class TFocusShiftCommand extends KfCommand {
    focusExitAction: TFocusExitAction
    focusEnterAction: TFocusEnterAction
    
    constructor(focusExitAction: TFocusExitAction) {
        super()
        this.focusExitAction = focusExitAction
        //self.focusExitAction.recordState;
        // responsibility of component being focused to call setFocusEnterAction
    }
    
    setFocusEnterAction(focusEnterAction: TFocusEnterAction): void {
        let selectionInformation: TSelectionInformation = {text: "", selStart: 0, selLength: 0}
        
        this.focusEnterAction = focusEnterAction
        // param ignored
        this.focusEnterAction.recordState(selectionInformation)
    }
    
    doCommand(): void {
        super.doCommand()
        usdomain.domain.beginUpdate()
        this.focusExitAction.doShift()
        if (this.focusEnterAction !== null) {
            this.focusEnterAction.doShift()
        }
        usdomain.domain.endUpdate()
    }
    
    undoCommand(): void {
        super.undoCommand()
        usdomain.domain.beginUpdate()
        if (this.focusEnterAction !== null) {
            this.focusEnterAction.undoShift()
        }
        this.focusExitAction.undoShift()
        usdomain.domain.endUpdate()
    }
    
    redoCommand(): void {
        super.doCommand()
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
