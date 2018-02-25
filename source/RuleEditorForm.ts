import * as m from "mithril"

type ViewName = "table" | "map" | "browser"

class RuleTableForm {
    view() {
        return m("div", "Unfinished RuleTableForm")
    }
}

class RuleMapForm {
    view() {
        return m("div", "Unfinished RuleMapForm")
    }
}

class RuleBrowserForm {
    view() {
        return m("div", "Unfinished RuleBrowserForm")
    }
}

export class RuleEditorForm {
    domain: any
    currentView: ViewName= "table"

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    view(vnode: m.Vnode) {
        const currentView = this.currentView
        const domain = (<any>vnode.attrs).domain

        function buttonWithHighlight(selection: ViewName) {
            return "button.ml2.w4" + (currentView === selection ? ".bg-light-blue" : "")
        }

        return m(".RuleEditorForm.ml3",
            m("span.b", "Rule Editor"),
            m(buttonWithHighlight("table"), { onclick: () => this.currentView = "table" }, "Table"),
            m(buttonWithHighlight("map"),  { onclick: () => this.currentView = "map" }, "Map"),
            m(buttonWithHighlight("browser"),  { onclick: () => this.currentView = "browser" }, "Browser"),
            // TODO: Probably should wrap these with hidden divs so the component state is preserved
            m("div.mt2",
                currentView === "table" ? m(RuleTableForm, <any>{domain: domain}) : [],
                currentView === "map" ? m(RuleMapForm, <any>{domain: domain}) : [],
                currentView === "browser" ? m(RuleBrowserForm, <any>{domain: domain}) : [],
            )
        )
    }
}
