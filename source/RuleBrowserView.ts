import * as m from "mithril"

export class RuleBrowserView {
    domain: any

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    /*
    loadFirstListBox(): void {
        let i: int
        let variable: TSVariable
        
        this.FirstListBox.Clear()
        this.firstListBoxLabel.Caption = "All " + lowercase(usworld.TSRule.headerForField(this.organizeByField))
        if (this.firstListBoxLabel.Caption[len(this.firstListBoxLabel.Caption)] !== "s") {
            this.firstListBoxLabel.Caption = this.firstListBoxLabel.Caption + "s"
        }
        for (i = 0; i <= usdomain.domain.world.variables.Count - 1; i++) {
            variable = usdomain.domain.world.variables[i]
            if (variable.hasUseagesForField(this.organizeByField)) {
                this.FirstListBox.Items.AddObject(variable.phrase, variable)
            }
        }
    }
    
    loadSecondListBox(): void {
        let i: int
        let variable: TSVariable
        let rule: TSRule
        let displayFieldType: int
        let selectedItemString: string
        
        this.SecondListBox.Clear()
        if (this.organizeByField === usworld.kRuleCommand) {
            displayFieldType = usworld.kRuleContext
            this.SecondListBoxImage.Picture.Bitmap = this.ContextSpeedButton.Glyph
        } else {
            displayFieldType = usworld.kRuleCommand
            this.SecondListBoxImage.Picture.Bitmap = this.CommandSpeedButton.Glyph
        }
        selectedItemString = lowercase(usworld.TSRule.headerForField(this.organizeByField))
        if (selectedItemString[len(selectedItemString)] === "s") {
            // remove plural 's' for singular use
            selectedItemString = UNRESOLVED.copy(selectedItemString, 1, len(selectedItemString) - 1)
        }
        this.SecondListBoxLabel.Caption = usworld.TSRule.headerForField(displayFieldType) + "s with selected " + selectedItemString
        if (this.FirstListBox.ItemIndex < 0) {
            return
        }
        variable = UNRESOLVED.TObject(this.FirstListBox.Items.Objects[this.FirstListBox.ItemIndex])
        for (i = 0; i <= usdomain.domain.world.rules.Count - 1; i++) {
            rule = usdomain.domain.world.rules[i]
            if (rule.usesVariableFor(variable, this.organizeByField)) {
                this.SecondListBox.Items.AddObject(rule.variableForField(displayFieldType).phrase, rule)
            }
        }
    }
    
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
        if (this.organizeByField === usworld.kRuleCommand) {
            displayFieldType = usworld.kRuleContext
        } else {
            displayFieldType = usworld.kRuleCommand
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
    
    setOrganizeByField(newValue: int): void {
        let variable: TSVariable
        
        if ((newValue < 0) || (newValue > usworld.kLastRuleField)) {
            return
        }
        usdomain.domain.options.browseBy = newValue
        this.MenuBrowseByContext.checked = newValue === usworld.kRuleContext
        this.MenuBrowseByCommand.checked = newValue === usworld.kRuleCommand
        this.MenuBrowseByMove.checked = newValue === usworld.kRuleMove
        this.MenuBrowseByRequirements.checked = newValue === usworld.kRuleRequirements
        this.MenuBrowseByChanges.checked = newValue === usworld.kRuleChanges
        if (newValue === usworld.kRuleContext) {
            this.firstListBoxImage.Picture.Bitmap = usconsoleform.ConsoleForm.ContextButton.Glyph
        }
        if (newValue === usworld.kRuleCommand) {
            this.firstListBoxImage.Picture.Bitmap = usconsoleform.ConsoleForm.CommandButton.Glyph
        }
        if (newValue === usworld.kRuleMove) {
            this.firstListBoxImage.Picture.Bitmap = usconsoleform.ConsoleForm.MoveButton.Glyph
        }
        if (newValue === usworld.kRuleRequirements) {
            this.firstListBoxImage.Picture.Bitmap = usconsoleform.ConsoleForm.RequirementsButton.Glyph
        }
        if (newValue === usworld.kRuleChanges) {
            this.firstListBoxImage.Picture.Bitmap = usconsoleform.ConsoleForm.ChangesButton.Glyph
        }
        // if organizeByField <> newValue then
        this.organizeByField = newValue
        this.loadFirstListBox()
        if (this.rule !== null) {
            variable = this.rule.variableForFieldWithSelections(this.organizeByField, this.RequirementsListBox.ItemIndex, this.ChangesListBox.ItemIndex)
            this.FirstListBox.ItemIndex = this.FirstListBox.Items.IndexOfObject(variable)
        }
        this.loadSecondListBox()
        this.SecondListBox.ItemIndex = this.SecondListBox.Items.IndexOfObject(this.rule)
    }
    
    firstListBoxImageClick(Sender: TObject): void {
        switch (this.organizeByField) {
            case usworld.kRuleContext:
                this.setOrganizeByField(usworld.kRuleCommand)
                break
            case usworld.kRuleCommand:
                this.setOrganizeByField(usworld.kRuleMove)
                break
            case usworld.kRuleMove:
                this.setOrganizeByField(usworld.kRuleRequirements)
                break
            case usworld.kRuleRequirements:
                this.setOrganizeByField(usworld.kRuleChanges)
                break
            case usworld.kRuleChanges:
                this.setOrganizeByField(usworld.kRuleContext)
                break
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
        console.log("firstListBoxImageClick")
    }

    view() {
        return m("TTabSheet.TabSheetBrowse.TTabSheet",
            {
            },
            "Browser",
            m("div.PanelLists.TPanel",
                {
                },
                "PanelLists",
                m("div.PanelFirstList.TPanel",
                    {
                    },
                    m("Group.Group.g00000002",
                        m("img.firstListBoxImage.TImage",
                            {
                                onclick: () => this.firstListBoxImageClick(),
                            },
                        ),
                        m("div.firstListBoxLabel.TLabel",
                            {
                            },
                            "Contexts",
                        ),
                    ),
                    m("TListBox.FirstListBox.TListBox",
                        {
                        },
                    ),
                ),
                m("div.SplitterLists.TSplitter",
                    {
                    },
                ),
                m("div.PanelSecondList.TPanel",
                    {
                    },
                    m("Group.Group.g00000002",
                        m("img.SecondListBoxImage.TImage",
                            {
                            },
                        ),
                        m("div.SecondListBoxLabel.TLabel",
                            {
                            },
                            "Commands",
                        ),
                    ),
                    m("TListBox.SecondListBox.TListBox",
                        {
                        },
                    ),
                ),
            ),
        )
    }
}
