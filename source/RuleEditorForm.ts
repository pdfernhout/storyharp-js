import * as m from "mithril"
import { FileUtils } from "./FileUtils"
import { TWorld } from "./TWorld"
import { IndividualRuleView } from "./IndividualRuleView"
import { RuleTableView } from "./RuleTableView";
import { RuleMapView } from "./RuleMapView"
import { RuleBrowserView } from "./RuleBrowserView"
import { ContextWizardView } from "./ContextWizardView"
import { CommandWizardView } from "./CommandWizardView"
import { LinkWizardView } from "./LinkWizardView"

type ViewName = "table" | "map" | "browser" | "wizards"
type WizardName = "context" | "command" | "link"

// TODO: POSSIBLE BUG: What happens to undo/redo for console when delete rules? Or change rule? Maybe just ignore?
// TODO: Should variables be deleted when they are no longer used by a rule?

export class RuleEditorForm {
    domain: any
    currentView: ViewName = "table"
    currentWizard: WizardName = "context"

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    viewWizards() {
        const currentWizard = this.currentWizard
        const domain = this.domain

        function wizardButtonWithHighlight(selection: WizardName) {
            return "button.ml2.w4" + (currentWizard === selection ? ".bg-light-blue" : "")
        }

        return m("div",
            m("div.mt2",
                "Wizard:",
                m(wizardButtonWithHighlight("context"), { onclick: () => this.currentWizard = "context" }, "Context"),
                m(wizardButtonWithHighlight("command"),  { onclick: () => this.currentWizard = "command" }, "Command"),
                m(wizardButtonWithHighlight("link"),  { onclick: () => this.currentWizard = "link" }, "Link"),
            ),
            m("div.mt2",
                currentWizard === "context" ? m(ContextWizardView, <any>{domain: domain}) : [],
                currentWizard === "command" ? m(CommandWizardView, <any>{domain: domain}) : [],
                currentWizard === "link" ? m(LinkWizardView, <any>{domain: domain}) : [],
            )
        )
    }

    view() {
        const currentView = this.currentView
        const domain = this.domain

        function buttonWithHighlight(selection: ViewName) {
            return "button.ml2.w4" + (currentView === selection ? ".bg-light-blue" : "")
        }

        return m(".RuleEditorForm.ml3.flex.flex-column.flex-nowrap.overflow-hidden",
            { style: "height: calc(100% - 5rem)" },
            m("div.flex-none",
                m("span.b", "Rule Editor"),
                m(buttonWithHighlight("table"), { onclick: () => this.currentView = "table" }, "Table"),
                m(buttonWithHighlight("map"),  { onclick: () => this.currentView = "map" }, "Map"),
                m(buttonWithHighlight("browser"),  { onclick: () => this.currentView = "browser" }, "Browser"),
                m(buttonWithHighlight("wizards"),  { onclick: () => this.currentView = "wizards" }, "Wizards"),
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
            // TODO: Probably should wrap these with hidden divs so the component state is preserved
            currentView === "wizards" ? this.viewWizards() : [
                m("div.mt2.flex-auto.overflow-auto",
                    currentView === "table" ? m(RuleTableView, <any>{domain: domain}) : [],
                    currentView === "map" ? m(RuleMapView, <any>{domain: domain}) : [],
                    currentView === "browser" ? m(RuleBrowserView, <any>{domain: domain}) : [],
                ),
                m("div.flex-none",
                    m(IndividualRuleView, <any>{domain: domain})
                )
            ],
        )
    }
}
