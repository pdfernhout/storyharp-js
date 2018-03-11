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
import { TSDomain } from "./TSDomain"

type ViewName = "table" | "map" | "browser" | "wizards"
type WizardName = "context" | "command" | "link"

// TODO: POSSIBLE BUG: What happens to undo/redo for console when delete rules? Or change rule? Maybe just ignore?
// TODO: Should variables be deleted when they are no longer used by a rule?

export class RuleEditorForm {
    domain: TSDomain

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

        function wizardButtonWithHighlight(selection: WizardName) {
            return "button.ml2.w4.bb-0.br3.br--top" + (currentWizard === selection ? ".bg-white" : "")
        }

        return m("div.flex-auto.h-100.overflow-hidden",
            m("div.mt2",
                "Wizard:",
                m(wizardButtonWithHighlight("context"), { onclick: (event: Event) => this.setCurrentWizard(event, "context") }, "Context"),
                m(wizardButtonWithHighlight("command"),  { onclick: (event: Event) => this.setCurrentWizard(event, "command") }, "Command"),
                m(wizardButtonWithHighlight("link"),  { onclick: (event: Event) => this.setCurrentWizard(event, "link") }, "Link"),
            ),
            m("div.WizardHolder.mt2", { style: "height: calc(100% - 3rem)" },
                currentWizard === "context" ? m(ContextWizardView, <any>{domain: domain}) : [],
                currentWizard === "command" ? m(CommandWizardView, <any>{domain: domain}) : [],
                currentWizard === "link" ? m(LinkWizardView, <any>{domain: domain}) : [],
            )
        )
    }

    setCurrentView(event: any, viewName: ViewName) {
        this.domain.currentEditorView = viewName
        event.target.blur()
    }

    view() {
        const currentView = this.domain.currentEditorView
        const domain = this.domain

        function buttonWithHighlight(selection: ViewName) {
            return "button.ml2.w4.bb-0.br3.br--top" + (currentView === selection ? ".bg-white" : "")
        }

        return m(".RuleEditorForm.ml3.flex.flex-column.flex-nowrap.overflow-hidden",
            { style: "height: calc(100% - 5rem)" },
            m("div.flex-none",
                m("span", "Rule Editor:"),
                m(buttonWithHighlight("table"), { onclick: (event: Event) => this.setCurrentView(event, "table") }, "Table"),
                m(buttonWithHighlight("map"),  { onclick: (event: Event) => this.setCurrentView(event, "map") }, "Map"),
                m(buttonWithHighlight("browser"),  { onclick: (event: Event) => this.setCurrentView(event, "browser") }, "Browser"),
                m(buttonWithHighlight("wizards"),  { onclick: (event: Event) => this.setCurrentView(event, "wizards") }, "Wizards"),
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
                m("span.ml2.i", domain.isWorldFileChanged() ? `<changes: ${domain.worldChangeCount}>` : "")
            ),
            // TODO: Probably should wrap these with hidden divs so the component state is preserved
            currentView === "wizards"
                ? this.viewWizards() 
                : [
                    m("div.mt2.flex-auto.h-100.w-100.overflow-hidden",
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
