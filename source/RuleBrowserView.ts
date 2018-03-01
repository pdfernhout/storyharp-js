import * as m from "mithril"
import { int } from "./common"
import { TSVariable } from "./TSVariable"
import { TSRuleField, TSRule } from "./TSRule"
import { Glyph } from "./VariablesView"

export class RuleBrowserView {
    domain: any
    browseBy = TSRuleField.kRuleContext
    selectedVariable: TSVariable | null = null

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
        this.setOrganizeByField(TSRuleField.kRuleContext)
    }

    viewFirstListBox() {
        let label = TSRule.headerForField(this.browseBy).toLowerCase()
        if (!label.endsWith("s")) {
            label += "s"
        }

        return m("div",
            m("div",
                {
                    onclick: () => this.firstListBoxImageClick(),
                },
                this.glyphForBrowseBy(),
                " All ",
                label
            ),
            m("div.ba.pa1",
                this.domain.world.variables
                    .filter((variable: TSVariable) => variable.hasUseagesForField(this.browseBy))
                    .sort((a: TSVariable, b: TSVariable) => a.phrase.localeCompare(b.phrase)) 
                    .map((variable: TSVariable) => m("div.ma1" + (variable === this.selectedVariable ? ".ba" : ""), 
                        {
                            onclick: () => this.selectedVariable = variable
                        },
                        variable.phrase
                    ))
            )
        )
    }
    
    viewSecondListBox() {
        let displayFieldType: int
        let glyph: string
        
        if (this.browseBy === TSRuleField.kRuleCommand) {
            displayFieldType = TSRuleField.kRuleContext
            glyph = Glyph.context
        } else {
            displayFieldType = TSRuleField.kRuleCommand
            glyph = Glyph.command
        }

        let selectedItemString = TSRule.headerForField(this.browseBy).toLowerCase()
        if (selectedItemString.endsWith("s")) {
            // remove plural 's' for singular use
            selectedItemString = selectedItemString.substring(0, selectedItemString.length - 1)
        }

        const caption = TSRule.headerForField(displayFieldType) + "s with selected " + selectedItemString

        let rules: TSRule[] = []

        if (this.selectedVariable) {
            for (let i = 0; i < this.domain.world.rules.length; i++) {
                const rule: TSRule = this.domain.world.rules[i]
                if (rule.usesVariableFor(this.selectedVariable, this.browseBy)) {
                    rules.push(rule)
                }
            }
        }

        return m("div",
            caption,
            m("div.ba.pa1",
                rules.map(rule => m("div.ma1",
                    rule.variableForField(displayFieldType).phrase
                ))
            )
        )
    }

    /*
    FirstListBoxDrawItem(Control: TWinControl, index: int, Rect: TRect, State: TOwnerDrawState): void {
        let i: int
        let focused: boolean
        let rule: TSRule
        let variable: TSVariable
        
        if ((index < 0) || (index > this.FirstListBox.Items.Count - 1)) {
            return
        }
        focused = false
        if (delphi_compatability.TOwnerDrawStateType.odSelected in State) {
            for (i = 0; i <= this.SecondListBox.Items.Count - 1; i++) {
                rule = UNRESOLVED.TObject(this.SecondListBox.Items.Objects[i])
                focused = (rule === this.rule) && rule.selected
                if (focused) {
                    break
                }
            }
        }
        variable = UNRESOLVED.TObject(this.FirstListBox.Items.Objects[index])
        if (variable === null) {
            return
        }
        this.drawBrowserListBoxItem(this.FirstListBox, variable.phrase, index, Rect, delphi_compatability.TOwnerDrawStateType.odSelected in State, focused)
    }
    
    SecondListBoxDrawItem(Control: TWinControl, index: int, Rect: TRect, State: TOwnerDrawState): void {
        let selected: boolean
        let focused: boolean
        let rule: TSRule
        let displayFieldType: int
        let displayString: string
        
        if ((index < 0) || (index > this.SecondListBox.Items.Count - 1)) {
            return
        }
        rule = UNRESOLVED.TObject(this.SecondListBox.Items.Objects[index])
        selected = rule.selected
        focused = rule === this.rule
        if (this.organizeByField === TSRuleField.kRuleCommand) {
            displayFieldType = TSRuleField.kRuleContext
        } else {
            displayFieldType = TSRuleField.kRuleCommand
        }
        displayString = rule.variableForField(displayFieldType).phrase
        this.drawBrowserListBoxItem(this.SecondListBox, displayString, index, Rect, selected, focused)
    }
    
    drawBrowserListBoxItem(Control: TWinControl, displayString: string, index: int, Rect: TRect, selected: boolean, focused: boolean): void {
        let listBox: TListBox
        
        if (delphi_compatability.Application.terminated) {
            //cText: array[0..255] of Char;
            return
        }
        listBox = Control
        if (listBox === null) {
            return
        }
        if ((index < 0) || (index > listBox.Items.Count - 1)) {
            return
        }
        setCanvasColorsForSelection(listBox.Canvas, selected, focused, false)
        listBox.Canvas.FillRect(Rect)
        //strPCopy(cText, displayString);
        // margin for text 
        Rect.left = Rect.left + 2
        UNRESOLVED.DrawText(listBox.Canvas.Handle, displayString, len(displayString), Rect, delphi_compatability.DT_LEFT)
    }
    
    FirstListBoxMouseDown(Sender: TObject, Button: TMouseButton, Shift: TShiftState, X: int, Y: int): void {
        this.FirstListBox.Invalidate()
        this.loadSecondListBox()
    }
    
    SecondListBoxMouseDown(Sender: TObject, Button: TMouseButton, Shift: TShiftState, X: int, Y: int): void {
        let rule: TSRule
        let shiftRule: TSRule
        let index: int
        let i: int
        
        if (this.SecondListBox.ItemIndex < 0) {
            return
        }
        index = this.SecondListBox.ItemAtPos(Point(X, Y), true)
        if (index < 0) {
            usdomain.domain.world.deselectAllExcept(null)
            this.FirstListBox.Invalidate()
            this.SecondListBox.Invalidate()
            return
        }
        rule = UNRESOLVED.TObject(this.SecondListBox.Items.Objects[this.SecondListBox.ItemIndex])
        if (rule === null) {
            return
        }
        if ((delphi_compatability.TShiftStateEnum.ssShift in Shift)) {
            if ((this.lastBrowserSingleRuleIndex >= 0) && (this.lastBrowserSingleRuleIndex <= this.SecondListBox.Items.Count - 1) && (this.lastSingleRuleIndex !== index)) {
                // shift
                usdomain.domain.world.deselectAllExcept(rule)
                if (this.lastBrowserSingleRuleIndex < index) {
                    for (i = this.lastBrowserSingleRuleIndex; i <= index; i++) {
                        shiftRule = UNRESOLVED.TObject(this.SecondListBox.Items.Objects[i])
                        shiftRule.selected = true
                    }
                } else if (this.lastBrowserSingleRuleIndex > index) {
                    for (i = this.lastBrowserSingleRuleIndex; i >= index; i--) {
                        shiftRule = UNRESOLVED.TObject(this.SecondListBox.Items.Objects[i])
                        shiftRule.selected = true
                    }
                }
            }
            // control
        } else if ((delphi_compatability.TShiftStateEnum.ssCtrl in Shift)) {
            // just click
            rule.selected = !rule.selected
        } else {
            if (!rule.selected) {
                usdomain.domain.world.deselectAllExcept(rule)
                rule.selected = true
                this.lastBrowserSingleRuleIndex = index
            } else {
                // do nothing except maybe drag...
            }
        }
        if (rule.selected && (this.rule !== rule) && !(delphi_compatability.TShiftStateEnum.ssCtrl in Shift) && !(delphi_compatability.TShiftStateEnum.ssShift in Shift)) {
            this.editRule(rule)
        }
        this.FirstListBox.Invalidate()
        this.SecondListBox.Invalidate()
    }
    
    fillListBox(listBox: TListBox, list: TList): void {
        let i: int
        let wrapper: TSDesiredStateVariableWrapper
        
        // OK for requirements because is parent class
        listBox.Items.Clear()
        if (this.rule !== null) {
            for (i = 0; i <= list.Count - 1; i++) {
                wrapper = list[i]
                listBox.Items.AddObject(wrapper.displayString(), wrapper)
            }
            listBox.Items.Add("")
        }
    }
    
    logicalStatementForListBox(listBox: TListBox): string {
        let result = ""
        let i: int
        
        result = ""
        for (i = 0; i <= listBox.Items.Count - 2; i++) {
            if (result !== "") {
                // use 2 because last is always blank for adding
                result = result + " & " + trim(listBox.Items[i])
            } else {
                result = trim(listBox.Items[i])
            }
        }
        return result
    }
    
    SpeedButtonClick(Sender: TObject): void {
        this.commitChangesToRule()
        this.switchToPage(this.TabSheetBrowse)
        this.setOrganizeByField((Sender).tag)
    }
    */

    firstListBoxImageClick() {
        switch (this.browseBy) {
            // A circle of changes
            case TSRuleField.kRuleContext:
                this.setOrganizeByField(TSRuleField.kRuleCommand)
                break
            case TSRuleField.kRuleCommand:
                this.setOrganizeByField(TSRuleField.kRuleMove)
                break
            case TSRuleField.kRuleMove:
                this.setOrganizeByField(TSRuleField.kRuleRequirements)
                break
            case TSRuleField.kRuleRequirements:
                this.setOrganizeByField(TSRuleField.kRuleChanges)
                break
            case TSRuleField.kRuleChanges:
                this.setOrganizeByField(TSRuleField.kRuleContext)
                break
            default:
                throw new Error("Unexpected default: " + this.browseBy)
        }
    }

    setOrganizeByField(newValue: TSRuleField): void {
        if ((newValue < 0) || (newValue > TSRuleField.kLastRuleField)) {
            throw new Error("unexpectd value for organizeByField: " + newValue)
        }

        /*
        usdomain.domain.options.browseBy = newValue
        this.MenuBrowseByContext.checked = newValue === TSRuleField.kRuleContext
        this.MenuBrowseByCommand.checked = newValue === TSRuleField.kRuleCommand
        this.MenuBrowseByMove.checked = newValue === TSRuleField.kRuleMove
        this.MenuBrowseByRequirements.checked = newValue === TSRuleField.kRuleRequirements
        this.MenuBrowseByChanges.checked = newValue === TSRuleField.kRuleChanges
        */

        // if organizeByField <> newValue then
        this.browseBy = newValue
        
        if (this.domain.editedRule !== null) {
            const variable: TSVariable = this.domain.editedRule.variableForFieldWithSelections(
                this.browseBy,
                0, /* TODO: this.RequirementsListBox.ItemIndex, */
                0, /* TODO: this.ChangesListBox.ItemIndex */
            )
            this.selectedVariable = variable
        } else {
            this.selectedVariable = null
        }

        /*
        this.loadSecondListBox()
        this.SecondListBox.ItemIndex = this.SecondListBox.Items.IndexOfObject(this.rule)
        */
    }

    glyphForBrowseBy() {
        switch(this.browseBy) {
            case TSRuleField.kRuleContext:
                return Glyph.context
            case TSRuleField.kRuleCommand:
                return Glyph.command
            case TSRuleField.kRuleMove:
                return Glyph.move
            case TSRuleField.kRuleRequirements:
                return Glyph.requirements
            case TSRuleField.kRuleChanges:
                return Glyph.changes
            default:
                throw new Error("unexpected case")
        }
    }

    view() {
        return m("div.flex.flex-row",
            m("div.w-30",
                this.viewFirstListBox(),
            ),
            /*
            m("div.SplitterLists.TSplitter",
                {
                },
            ),
            */
            m("div.w-70",
                this.viewSecondListBox(),
            ),
        )
    }
}
