import * as m from "mithril"

import { TWorld } from "./TWorld"
// import { TSRule, TSRuleField } from "./TSRule"
// import { TSCommandList } from "./TSCommandList"
// import { TSNewRulesCommand } from "./TSNewRulesCommand"

import { FileUtils } from "./FileUtils"
import { IndividualRuleView } from "./IndividualRuleView"
import { RuleTableView } from "./RuleTableView";

type ViewName = "table" | "map" | "browser"

// TODO: POSSIBLE BUG: What happens to undo/redo for console when delete rules? Or change rule? Maybe just ignore?
// TODO: Should variables be deleted when they are no longer used by a rule?

class RuleMapView {
    domain: any

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    view() {
        return m("div", "Unfinished RuleMapForm")
    }
}

class RuleBrowserView {
    domain: any

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

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

    load() {
        const world: TWorld = this.domain.world
        FileUtils.loadFromFile(false, (fileName: string, contents: string) => {
            console.log("chose", fileName)
            world.resetVariablesAndRules()
            const loaded = world.loadWorldFromFileContents(contents)
            console.log("load status", loaded)
            if (fileName.endsWith(".wld")) fileName = fileName.substring(0, fileName.length - 4)
            this.domain.loadedFileName = fileName
            this.domain.world.newSession()
            this.domain.sessionCommandList.clear()
            this.domain.worldCommandList.clear()
            this.domain.editedRule = null
            this.domain.lastSingleRuleIndex = 0
            m.redraw()
        })
    }

    save() {
        const world: TWorld = this.domain.world
        const fileName = this.domain.loadedFileName
        FileUtils.saveToFile(fileName, world.saveWorldToFileContents(false), ".wld", (fileName: string) => {
            console.log("written", fileName)
            this.domain.loadedFileName = fileName
            m.redraw()
        })
    }

    newWorld() {
        const fileName = prompt("What would you like to call your new world?")
        if (!fileName) return
        this.domain.loadedFileName = fileName
        const world: TWorld = this.domain.world
        world.resetVariablesAndRules()
        world.newSession()
        this.domain.sessionCommandList.clear()
        this.domain.worldCommandList.clear()
        this.domain.editedRule = null
        this.domain.lastSingleRuleIndex = 0
    }

    view(vnode: m.Vnode) {
        const currentView = this.currentView
        const domain = (<any>vnode.attrs).domain

        function buttonWithHighlight(selection: ViewName) {
            return "button.ml2.w4" + (currentView === selection ? ".bg-light-blue" : "")
        }

        return m(".RuleEditorForm.ml3.flex.flex-column.flex-nowrap.overflow-hidden",
            { style: "height: calc(100% - 7rem)" },
            m("div.flex-none",
                m("span.b", "Rule Editor"),
                m(buttonWithHighlight("table"), { onclick: () => this.currentView = "table" }, "Table"),
                m(buttonWithHighlight("map"),  { onclick: () => this.currentView = "map" }, "Map"),
                m(buttonWithHighlight("browser"),  { onclick: () => this.currentView = "browser" }, "Browser"),
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
                m("button.ml4.w3", { onclick: () => this.save() }, "Save"),
                m("button.ml1.w3", { onclick: () => this.load() }, "Load"),
                m("button.ml4.w3", { onclick: () => this.newWorld() }, "New"),
            ),
            // TODO: Probably should wrap these with hidden divs so the component state is preserved
            m("div.mt2.flex-auto.overflow-auto",
                currentView === "table" ? m(RuleTableView, <any>{domain: domain}) : [],
                currentView === "map" ? m(RuleMapView, <any>{domain: domain}) : [],
                currentView === "browser" ? m(RuleBrowserView, <any>{domain: domain}) : [],
            ),
            m("div.flex-none",
                m(IndividualRuleView, <any>{domain: domain})
            )
        )
    }
}
