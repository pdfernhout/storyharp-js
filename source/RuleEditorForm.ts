import * as m from "mithril"
import { FileUtils } from "./FileUtils"
import { TWorld } from "./TWorld"
import { IndividualRuleView } from "./IndividualRuleView"
import { RuleTableView } from "./RuleTableView"
import { RuleMapView } from "./RuleMapView"
import { RuleBrowserView } from "./RuleBrowserView"
import { ContextWizardView } from "./ContextWizardView"
import { CommandWizardView } from "./CommandWizardView"
import { LinkWizardView } from "./LinkWizardView"
import { TSDomain, WizardName, EditorName } from "./TSDomain"
import { notebookTabButton, ScrollIntoViewDirection } from "./common"
import { LogView } from "./LogView"
import { TSRule } from "./TSRule"

// TODO: POSSIBLE BUG: What happens to undo/redo for console when delete rules? Or change rule? Maybe just ignore?
// TODO: Should variables be deleted when they are no longer used by a rule?

export class RuleEditorForm {
    domain: TSDomain
    lastSearchString = ""

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    setCurrentWizard(event: any, wizardName: WizardName) {
        this.domain.currentEditorWizard = wizardName
        event.target.blur()
    }

    viewWizards() {
        const currentWizard = this.domain.currentEditorWizard
        const domain = this.domain

        return m("div.flex-auto.h-100.w-100.overflow-hidden",
            m("div.mb2",
                "Wizard:",
                m(notebookTabButton(currentWizard === "context"), { onclick: (event: Event) => this.setCurrentWizard(event, "context") }, "Context"),
                m(notebookTabButton(currentWizard === "command"),  { onclick: (event: Event) => this.setCurrentWizard(event, "command") }, "Command"),
                m(notebookTabButton(currentWizard === "link"),  { onclick: (event: Event) => this.setCurrentWizard(event, "link") }, "Link"),
            ),
            m("div.WizardHolder.w-100", { style: "height: calc(100% - 3rem)" },
                currentWizard === "context" ? m(ContextWizardView, <any>{domain: domain}) : [],
                currentWizard === "command" ? m(CommandWizardView, <any>{domain: domain}) : [],
                currentWizard === "link" ? m(LinkWizardView, <any>{domain: domain}) : [],
            )
        )
    }

    setCurrentView(event: any, editorName: EditorName) {
        this.domain.currentEditorView = editorName
        event.target.blur()
    }

    search() {
        const newSearchString = prompt("Search string? [case insensitive]", this.lastSearchString)
        if (!newSearchString) return
        this.lastSearchString = newSearchString
        // TODO: Support searching backwards
        // TODO: Pick fields to search on
        // TODO: Support mixed-case searches
        // TODO: Support regex searches
        this.searchForAndSelectRule(newSearchString, true, true)
    }

    searchForAndSelectRule(aText: string, ignoreCase: boolean, goDown: boolean): void {
        const domain = this.domain
        let match: boolean
        
        let ruleIndex = 0
        if (domain.editedRule) {
            ruleIndex = domain.world.rules.indexOf(domain.editedRule)
        }

        const matchText = ignoreCase ? aText.toLocaleLowerCase() : aText

        let count = 1
        let row = 0
        while ((count <= domain.world.rules.length)) {
            if (goDown) {
                row = (ruleIndex + count) % domain.world.rules.length
            } else {
                row = ((domain.world.rules.length * 2) + (ruleIndex - count)) % domain.world.rules.length
            }
            const rule: TSRule = domain.world.rules[row]

            let textForRule = [
                rule.context.phrase,
                rule.command.phrase,
                rule.reply,
                rule.move.phrase,
                rule.changesString,
                rule.requirementsString
            ].join("|")

            if (ignoreCase) textForRule = textForRule.toLocaleLowerCase()

            const match = textForRule.includes(matchText)

            if (match) {
                domain.world.deselectAllExcept(rule)
                domain.editRule(rule, ScrollIntoViewDirection.kFromTop, true)
                rule.selected = true
                return
            }
            count += 1
        }
        alert("Search string \"" + aText + "\" not found.")
    }

    view() {
        const currentView = this.domain.currentEditorView
        const domain = this.domain

        return m(".RuleEditorForm.ml3.flex.flex-column.flex-nowrap.overflow-hidden",
            { style: "height: calc(100% - 5rem)" },
            m("div.flex-none.mb2",
                m("span", "Rule Editor:"),
                m(notebookTabButton(currentView === "table"), { onclick: (event: Event) => this.setCurrentView(event, "table") }, "Table"),
                m(notebookTabButton(currentView === "map"),  { onclick: (event: Event) => this.setCurrentView(event, "map") }, "Map"),
                m(notebookTabButton(currentView === "browser"),  { onclick: (event: Event) => this.setCurrentView(event, "browser") }, "Browser"),
                m(notebookTabButton(currentView === "wizards"),  { onclick: (event: Event) => this.setCurrentView(event, "wizards") }, "Wizards"),
                m(notebookTabButton(currentView === "log"),  { onclick: (event: Event) => this.setCurrentView(event, "log") }, "Log"),
                m("button.ml4.w3", {
                    disabled: !domain.worldCommandList.isUndoEnabled(),
                    onclick: () => domain.worldCommandList.undoLast(),
                    title: "Undo " + domain.worldCommandList.undoDescription()
                }, "Undo"),
                m("button.ml1.w3", { 
                    disabled: !domain.worldCommandList.isRedoEnabled(),
                    onclick: () => domain.worldCommandList.redoLast(),
                    title: "Redo " + domain.worldCommandList.redoDescription()
                }, "Redo"),
            ),
            currentView === "wizards"
                ? this.viewWizards() 
                : [
                    // Seems to need the h-100 w-100 despite the flex auto -- maybe using that as desired size?
                    m("div.flex-auto.h-100.w-100.overflow-hidden",
                        currentView === "table" ? m(RuleTableView, <any>{domain: domain}) : [],
                        currentView === "map" ? m(RuleMapView, <any>{domain: domain}) : [],
                        currentView === "browser" ? m(RuleBrowserView, <any>{domain: domain}) : [],
                        currentView === "log" ? m(LogView, <any>{domain: domain}) : [],
                    ),
                    currentView !== "log" 
                        ? m("div.flex-none",
                            m(IndividualRuleView, <any>{domain: domain, ruleEditorForm: this}),
                        )
                        : []
                ],
        )
    }
}
