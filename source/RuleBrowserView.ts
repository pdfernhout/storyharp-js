import * as m from "mithril"
import { int } from "./common"
import { TSVariable } from "./TSVariable"
import { TSRuleField, TSRule } from "./TSRule"
import { Glyph } from "./VariablesView"

export class RuleBrowserView {
    domain: any
    browseBy = TSRuleField.kRuleContext
    selectedVariable: TSVariable | null = null
    lastBrowserSingleRuleIndex = 0
    ruleSubset: TSRule[] = []

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
                    .map((variable: TSVariable) => m("div.mt1" + (variable === this.selectedVariable ? ".bg-light-blue" : ""), 
                        {
                            onclick: () => this.selectedVariable = variable
                        },
                        // TODO: maybe? iterating over all rules in second list, then:
                        // focused = (rule === this.rule) && rule.selected
                        // not sure how this changes the look: 
                        // setCanvasColorsForSelection(listBox.Canvas, selected, focused, false)
                        variable.phrase
                    ))
            )
        )
    }
    
    styleForSelected(rule: TSRule): string {
        return rule.selected 
            ? (rule === this.domain.editedRule 
                ? ".ba.bw2.bg-light-blue" 
                : ".ba.bw2")
            : (rule === this.domain.editedRule 
                ? ".ba.bw2.b--white.bg-light-blue" 
                : ".ba.bw2.b--white")
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

        this.ruleSubset = rules

        return m("div",
            caption,
            m("div.ba.pa1",
                rules.map((rule, index) => m("div" + this.styleForSelected(rule),
                    {
                        style: "user-select: none",
                        onclick: (event: MouseEvent) => this.ruleClicked(event, rule, index)
                    },
                    rule.variableForField(displayFieldType).phrase
                ))
            )
        )
    }

    ruleClicked(event: MouseEvent, rule: TSRule, index: int): any {
        if (event.shiftKey) {
            console.log("shift clicked", this.lastBrowserSingleRuleIndex, this.ruleSubset.length, index)
            if ((this.lastBrowserSingleRuleIndex >= 0)
                && (this.lastBrowserSingleRuleIndex < this.ruleSubset.length)
                // Was bug in Delphi version where used lastBrowserSingleRuleIndex !== index instead
                && (this.lastBrowserSingleRuleIndex !== index)
            ) {
                // shift
                this.domain.world.deselectAllExcept(rule)
                if (this.lastBrowserSingleRuleIndex < index) {
                    for (let i = this.lastBrowserSingleRuleIndex; i <= index; i++) {
                        const shiftRule: TSRule = this.ruleSubset[i]
                        shiftRule.selected = true
                    }
                } else if (this.lastBrowserSingleRuleIndex > index) {
                    for (let i = this.lastBrowserSingleRuleIndex; i >= index; i--) {
                        const shiftRule: TSRule = this.ruleSubset[i]
                        shiftRule.selected = true
                    }
                }
            } else {
                console.log("skipped")
            }
        } else if (event.ctrlKey) {
            rule.selected = !rule.selected
        } else {
            // plain click
            if (!rule.selected) {
                this.domain.world.deselectAllExcept(rule)
                rule.selected = true
                this.lastBrowserSingleRuleIndex = index
            } else {
                // do nothing except maybe drag...
            }
        }
        if (rule.selected && (this.domain.editedRule !== rule) && !(event.ctrlKey) && !(event.shiftKey)) {
            // this.editRule(rule)
            this.domain.editedRule = rule
        }
    }


    /*
    
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

        /* TODO or remove
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
                // TODO -- maybe move back to lists for requirments and changes?
                0, /* TODO: this.RequirementsListBox.ItemIndex, */
                0, /* TODO: this.ChangesListBox.ItemIndex */
            )
            this.selectedVariable = variable
        } else {
            this.selectedVariable = null
        }

        this.lastBrowserSingleRuleIndex = 0
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
