import * as m from "mithril"
import { int, expander } from "./common"

// TODO: Unfinished. Just has drop down not quick fill.
// TODO: Use or remove quickfill code.
export class TQuickFillComboBox {
    lastMatch: string = ""
    FMustBeInList: boolean = false
    FEntryRequired: boolean = false
    Items: string[] = []
    Text: string = ""
    menuOpen = false
    size = 20
    onchangeCallback: (event: { target: HTMLInputElement }) => {}

    constructor(vnode: m.Vnode) {
        this.Text = (<any>vnode.attrs).value
        this.onchangeCallback = (<any>vnode.attrs).onchange
        this.Items = (<any>vnode.attrs).choices
        this.FMustBeInList = (<any>vnode.attrs).mustBeInList || false
        this.FEntryRequired = (<any>vnode.attrs).required || false
    }

    view(vnode: m.Vnode) {
        const extraStyling = (<any>vnode.attrs).extraStyling || ""
        return m("div.ml1.dib.relative",
            m("input" + extraStyling, {
                value: this.Text,
                // oninput: (event: { target: HTMLInputElement }) => this.Text = event.target.value,
                onchange: (event: { target: HTMLInputElement }) => {
                    this.Text = event.target.value
                    if (this.onchangeCallback) this.onchangeCallback(event)
                },
                oncreate: (vnode: any) => this.size = (<HTMLInputElement>(vnode.dom)).size,
                onupdate: (vnode: any) => this.size = (<HTMLInputElement>(vnode.dom)).size,
            }),
            m("button", {
                onclick: () => this.menuOpen = !this.menuOpen
            }, expander(this.menuOpen)),
            this.menuOpen
                ? m("ul.absolute.bg-light-gray.overflow-auto",
                    {
                        style: {
                            // Override Chrome user agent settings
                            "-webkit-margin-before": "0em",
                            "-webkit-margin-after": "0em",
                            "list-style-type": "none",
                            "margin": "0",
                            "padding": "0.25rem",
                            "width": "" + this.size + "rem",
                            "max-height": "10rem",
                            "box-shadow": "0px 8px 16px 0px rgba(0,0,0,0.2)",
                            "z-index": 1,
                        },
                    },
                    this.Items.map(item => m("li.hover-bg-light-blue", {
                        onclick: () => {
                            this.menuOpen = false
                            this.Text = item
                            if (this.onchangeCallback) this.onchangeCallback(<any>{target: {value: item}})
                        }
                    }, item))
                )
                : []
        )
    }

    /*
    
    // pdf hack test remove
    // pdf hack - test remove
    CBNEditChange(MSG: Tmessage): void {
        let i: int
        
        TCustomComboBox.prototype.CBNEditChange.call(this)
        i = 0
    }
    
    // returns -1 if no match, otherwise returns the index of the match
    findMatch(match: string): int {
        if (match === "") {
            return -1
        }
        if (this.Items.length === 0) {
            return -1
        }
        for (let i = 0; i < this.Items.length; i++) {
            const item = this.Items[i]
            if (item.substring(0, match.length) === match) {
                return i
            }
        }
        return -1
    }
    
    quickFillComboBoxKeyPress(Key: string): void {
        let index: int
        let startText: string
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
    */

}
