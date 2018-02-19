// unit QuickFillComboBox

from conversion_common import *
import delphi_compatability

const QuickFillComboBox = QuickFillComboBox || {}

//Must be published before Items
// pdf added
// pdf added
// pdf added
// PDF PORT changed from "register" as there was a name conflict with grammar; won't be called anyway
function RegisterIt() {
    UNRESOLVED.RegisterComponents("Speech", {TQuickFillComboBox, })
}


export class TQuickFillComboBox {
    lastMatch: String = ""
    FMustBeInList: boolean = false
    FEntryRequired: boolean = false
    TQuickFillComboBox.prototype = new TCustomComboBox()
    TQuickFillComboBox.prototype.constructor = TQuickFillComboBox
    
    // pdf hack test remove
    // pdf hack - test remove
    CBNEditChange(MSG: Tmessage): void {
        let i: int
        
        TCustomComboBox.prototype.CBNEditChange.call(this)
        i = 0
    }
    
    findMatch(match: String): int {
        let result = 0
        let i: int
        let j: int
        let test: String
        let matches: boolean
        
        result = -1
        if (match === "") {
            return result
        }
        if (this.Items.Count === 0) {
            return result
        }
        for (i = 0; i <= this.Items.Count - 1; i++) {
            matches = true
            test = this.Items[i]
            for (j = 1; j <= len(match); j++) {
                if (test[j] !== match[j]) {
                    matches = false
                    break
                }
            }
            if (matches === true) {
                result = i
                return result
            }
        }
        return result
    }
    
    quickFillComboBoxKeyPress(Key: byte): void {
        let index: int
        let startText: String
        let atEnd: boolean
        
        if (this.Text === "") {
            //reset if empty
            this.lastMatch = ""
        }
        //compensate for selection about to replace
        atEnd = (this.SelStart + this.SelLength) === len(this.Text)
        if (!atEnd) {
            if ((this.Text !== this.lastMatch) || (this.SelLength !== 0)) {
                if (this.FMustBeInList) {
                    if ((Key === 8) && (this.SelStart > 0)) {
                        this.SelStart = this.SelStart - 1
                    }
                    //eat key
                    Key = 0
                } else {
                    this.lastMatch = ""
                }
                if ((this.Text === "") && this.FEntryRequired && (this.Items.Count > 0)) {
                    //eat key
                    Key = 0
                    this.lastMatch = this.Items[0]
                    this.Text = this.lastMatch
                }
                return Key
            }
        }
        if (Key === 8) {
            if (this.SelLength === 0) {
                startText = UNRESOLVED.Copy(this.Text, 1, this.SelStart - 1)
                if (this.SelStart > 0) {
                    this.SelStart = this.SelStart - 1
                }
            } else {
                startText = UNRESOLVED.Copy(this.Text, 1, this.SelStart)
            }
        } else if (Key < 32) {
            //don't process control keys - low asciii values
            this.lastMatch = ""
            if ((this.Text === "") && this.FEntryRequired && (this.Items.Count > 0)) {
                this.lastMatch = this.Items[0]
                this.Text = this.lastMatch
            }
            return Key
        } else {
            startText = UNRESOLVED.Copy(this.Text, 1, this.SelStart) + String.fromCharCode(Key)
        }
        Key = 0
        if (startText === "") {
            if (this.FEntryRequired && (this.Items.Count > 0)) {
                this.lastMatch = this.Items[0]
                this.Text = this.lastMatch
            } else {
                this.Text = ""
                this.lastMatch = ""
            }
            return Key
        }
        index = this.findMatch(startText)
        if (index >= 0) {
            //   if (index < 0) and EntryRequired and (self.items.count > 0) then
            //     begin
            //     index := 0;
            //     startText := Copy(;
            //     end;
            this.lastMatch = this.Items[index]
            this.Text = this.lastMatch
            this.SelStart = len(startText)
        } else {
            if (!this.FMustBeInList) {
                this.lastMatch = ""
                this.Text = startText
                this.SelStart = len(startText)
            }
        }
        return Key
    }
    
    // PDF PORT changed Message to TheMessage as used in with and gramamr did not like that
    ComboWndProc(TheMessage: TMessage, ComboWnd: HWnd, ComboProc: Pointer): void {
        try {
            //FIX unresolved WITH expression: TheMessage
            switch (UNRESOLVED.Msg) {
                case UNRESOLVED.WM_CHAR:
                    if (this.Style !== delphi_compatability.TComboBoxStyle.csDropDownList) {
                        UNRESOLVED.TWMKey(TheMessage).charCode = this.quickFillComboBoxKeyPress(UNRESOLVED.TWMKey(TheMessage).charCode)
                    }
                    break
                case UNRESOLVED.CBN_EDITUPDATE:
                    UNRESOLVED.Dispatch(TheMessage)
                    break
                case UNRESOLVED.WM_LBUTTONUP:
                    UNRESOLVED.Dispatch(TheMessage)
                    break
            TCustomComboBox.prototype.ComboWndProc.call(this, TheMessage, ComboWnd, ComboProc)
        } catch (Exception e) {
            delphi_compatability.Application.HandleException(this)
        }
    }
    
    Change(): void {
        if (this.FMustBeInList && (this.Text !== "")) {
            if ((this.Items.Count <= 0)) {
                this.Text = ""
                return
            }
            if (this.findMatch(this.Text) < 0) {
                this.lastMatch = this.Items[0]
                this.Text = this.lastMatch
                this.SelStart = 0
                this.SelLength = 0
            }
        }
        if ((this.Text === "") && this.FEntryRequired && (this.Items.Count > 0)) {
            this.lastMatch = this.Items[0]
            this.Text = this.lastMatch
        }
        TCustomComboBox.prototype.Change.call(this)
    }
    
}

