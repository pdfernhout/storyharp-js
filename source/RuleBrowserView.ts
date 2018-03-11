import * as m from "mithril"
import { int } from "./common"
import { TSVariable } from "./TSVariable"
import { TSRuleField, TSRule } from "./TSRule"
import { Glyph } from "./VariablesView"
import { TSDomain } from "./TSDomain"

export class RuleBrowserView {
    domain: TSDomain
    browseBy = TSRuleField.kRuleContext
    selectedVariable: TSVariable | null = null
    lastBrowserSingleRuleIndex = 0
    ruleSubset: TSRule[] = []

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
        this.domain.setOrganizeByField = this.setOrganizeByField.bind(this)
        this.setOrganizeByField(TSRuleField.kRuleContext)
    }

    viewFirstListBox() {
        let label = TSRule.headerForField(this.browseBy).toLowerCase()
        if (!label.endsWith("s")) {
            label += "s"
        }

        //  pendingMapScroll
        const scrollIntoViewIfNeeded = (vnode: any, variable: TSVariable) => {
            if (this.domain.pendingMapScroll && this.domain.editedRule && this.selectedVariable === variable) {
                (<HTMLElement>(vnode.dom)).scrollIntoView(true)
            }
        }

        // TODO: These two lists should be made navigable by keyboard
        return m("div.h-100.overflow-hidden",
            m("div",
                {
                    onclick: () => this.firstListBoxImageClick(),
                },
                this.glyphForBrowseBy(),
                " All ",
                label
            ),
            m("div.ba.pa1.overflow-auto",
                { style: "height: calc(100% - 1rem)" },
                this.domain.world.variables
                    .filter((variable: TSVariable) => variable.hasUseagesForField(this.browseBy))
                    .sort((a: TSVariable, b: TSVariable) => a.phrase.localeCompare(b.phrase)) 
                    .map((variable: TSVariable) => m("div.mt1" + (variable === this.selectedVariable ? ".bg-light-blue" : ""), 
                        {
                            key: variable,
                            onclick: () => this.selectedVariable = variable,
                            oncreate: (vnode: m.Vnode) => scrollIntoViewIfNeeded(vnode, variable),
                            onupdate: (vnode: m.Vnode) => scrollIntoViewIfNeeded(vnode, variable),
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
                ? ".ba.bw1.bg-light-blue" 
                : ".ba.bw1")
            : (rule === this.domain.editedRule 
                ? ".ba.bw1.b--white.bg-light-blue" 
                : ".ba.bw1.b--white")
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

        //  pendingMapScroll
        const scrollIntoViewIfNeeded = (vnode: any, rule: TSRule) => {
            if (this.domain.pendingMapScroll && this.domain.editedRule && this.domain.editedRule === rule) {
                (<HTMLElement>(vnode.dom)).scrollIntoView(true)
                this.domain.pendingMapScroll = false
            }
        }

        return m("div.h-100.overflow-hidden",
            m("div", caption),
            m("div.ba.pa1.overflow-auto",
                { style: "height: calc(100% - 1rem)" },
                rules.map((rule, index) => m("div" + this.styleForSelected(rule),
                    {
                        key: rule,
                        style: "user-select: none",
                        onclick: (event: MouseEvent) => this.ruleClicked(event, rule, index),
                        oncreate: (vnode: m.Vnode) => scrollIntoViewIfNeeded(vnode, rule),
                        onupdate: (vnode: m.Vnode) => scrollIntoViewIfNeeded(vnode, rule),
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
            this.domain.editRule(rule)
        }
    }

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
            this.selectedVariable = this.domain.editedRule.variableForFieldWithSelections(
                this.browseBy,
                // TODO -- maybe move back to lists for requirments and changes?
                0, /* TODO: this.RequirementsListBox.ItemIndex, */
                0, /* TODO: this.ChangesListBox.ItemIndex */
            )
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
        return m(".RuleBrowserView.div.flex.flex-row.h-100.overflow-hidden",
            m("div.w-30.h-100.overflow-hidden",
                this.viewFirstListBox(),
            ),
            /*
            m("div.SplitterLists.TSplitter",
                {
                },
            ),
            */
            m("div.w-70.h-100.overflow-hidden",
                this.viewSecondListBox(),
            ),
        )
    }
}
