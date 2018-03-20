import * as m from "mithril"

import { makeFileNameWithoutWldExtension, makeFileNameWithWldExtension } from "./common";
import { TSDomain } from "./TSDomain"
import { TWorld, ExportRulesOption } from "./TWorld"
import { FileUtils } from "./FileUtils"
import { TSJavaScriptWriter } from "./TSJavaScriptWriter"
import { TSNewRulesCommand } from "./TSNewRulesCommand"
import { TSRule } from "./TSRule"
import { TSVariable } from "./TSVariable"
import { toast } from "./ToastView"

function saveWorldToLocalFile(domain: TSDomain) {
    const world: TWorld = domain.world
    const fileName = makeFileNameWithoutWldExtension(domain.worldFileName)
    FileUtils.saveToFile(fileName, world.saveWorldToFileContents(ExportRulesOption.kSaveAllRules), ".wld", (fileName: string) => {
        domain.worldFileName = makeFileNameWithWldExtension(fileName)
        // TODO: Figure out how to support undo after save where changecount is not negative and confused when make new changes
        domain.resetWorldChangeCount()
        domain.worldCommandList.clear()
        m.redraw()
    })
}

function exportSelectedRulesToLocalFile(domain: TSDomain) {
    const world: TWorld = domain.world
    const fileName = makeFileNameWithoutWldExtension(domain.worldFileName)
    FileUtils.saveToFile(fileName, world.saveWorldToFileContents(ExportRulesOption.kSaveOnlySelectedRules), ".wld", (fileName: string) => {
        m.redraw()
    })
}

// TODO: Save As... But maybe can't do with browser file model?

export function confirmUnsavedChangesLoss(domain: TSDomain) {
    if (domain.isWorldFileChanged()) {
        if (!confirm("You have unsaved changes to the current world which will be lost; proceed anyway?")) {
            return false
        }
    }

    /* TODO: Maybe check for session changes if can save session
    if (domain.isSessionFileChanged()) {
        if (!confirm("You have unsaved changes to the current session which will be lost; proceed anyway?")) {
            return false
        }
    }
    */

    return true
}

export function loadWorldFromLocalFile(domain: TSDomain) {
    if (!confirmUnsavedChangesLoss(domain)) return

    const world: TWorld = domain.world
    FileUtils.loadFromFile(false, (fileName: string, contents: string) => {
        world.resetVariablesAndRules()

        const loaded = world.loadWorldFromFileContents(contents)
        domain.addToLog("--- Read: " + fileName + (loaded ? " OK" : " Failed"))
        if (!loaded) toast("Something went wrong loading file: " + fileName)

        domain.updateForNewOrLoadedWorld(fileName, true)

        m.redraw()
    })
}

function newWorld(domain: TSDomain) {
    if (!confirmUnsavedChangesLoss(domain)) return

    const fileName = prompt("What would you like to call your new world?")
    if (!fileName) return

    domain.world.resetVariablesAndRules()
    domain.updateForNewOrLoadedWorld(makeFileNameWithWldExtension(fileName), false)
}

function mergeWorldFromLocalFile(domain: TSDomain): void {    
    const world: TWorld = domain.world
    const oldRuleCount = world.rules.length
    const oldVariablesCount = world.variables.length

    FileUtils.loadFromFile(false, (fileName: string, contents: string) => {
        // Don't reset anything because we are merging
        const loaded = world.loadWorldFromFileContents(contents)
        domain.addToLog("--- Load for merge: " + fileName + (loaded ? " OK" : " Failed"))

        if (!loaded) {
            toast("Something went wrong merging file: " + fileName)
            world.rules.length = oldRuleCount
            world.variables.length = oldVariablesCount
            m.redraw()
            return
        }

        const newRulesCommand = new TSNewRulesCommand(domain)
        newRulesCommand.creator = "merging " + makeFileNameWithoutWldExtension(fileName)

        world.deselectAllExcept(null)

        for (let i = oldRuleCount; i < world.rules.length; i++) {
            // select new items
            const rule: TSRule = world.rules[i]
            newRulesCommand.addRule(rule)
            rule.selected = true
        }

        for (let i = oldVariablesCount; i < world.variables.length; i++) {
            const variable: TSVariable = world.variables[i]
            variable.selected = true
        }

        domain.worldCommandList.doCommand(newRulesCommand)

        if (world.rules.length > 0) {
            domain.editRule(world.rules[world.rules.length - 1])
        } else {
            domain.editRule(null)
        }

        m.redraw()
    })
}

async function generateHTML(domain: TSDomain) {
    const writer = new TSJavaScriptWriter()
    const programText = writer.writeJavaScriptProgram(domain.world)
    if (!programText) {
        toast("Some rules and contexts must be defined first")
        return
    }
    const template = await m.request("resources/template.html", {deserialize: (text) => text})
    const htmlFile = template
        .replace(/\/\/ START REPLACE[\s\S]*\/\/ END REPLACE/gm, programText)
        .replace("StoryHarp 3.0 Player", makeFileNameWithoutWldExtension(domain.worldFileName))

    /* Works unless popup windows are blocked:
    const newWindow = window.open()
    if (!newWindow) throw new Error("Could not open window")
    newWindow.document.open()
    newWindow.document.write(htmlFile)
    newWindow.document.close()
    */

    const fileName = makeFileNameWithoutWldExtension(domain.worldFileName)
    FileUtils.saveToFile(fileName, htmlFile, ".html", (fileName: string) => {
        console.log("written", fileName)
        domain.addToLog("--- Wrote: " + fileName)
        m.redraw()
    })
}

export class FileForm {
    domain: TSDomain

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }
    
    view() {
        const domain = this.domain
        
        return m("div.FileForm",
            m("p"),
            m("button.ml3", { title: "Make a new world", onclick: () => newWorld(domain) }, "Make a new world"),
            m("p"),
            m("button.ml3", { title: "Open a world file", onclick: () => loadWorldFromLocalFile(domain) }, "Load a world from local filesystem"),
            m("p"),
            m("button.ml3", { title: "Merge in another world file", onclick: () => mergeWorldFromLocalFile(domain) }, "Merge in another world from local filesystem"),
            m("p"),
            m("button.ml3", { title: "Save a world file", onclick: () => saveWorldToLocalFile(domain) }, "Save a world to local filesystem"),
            m("p"),
            m("button.ml3", { title: "Export selected rules to a world file", onclick: () => exportSelectedRulesToLocalFile(domain) }, "Export only selected rules to local filesystem"),
            m("p"),
            m("button.ml3", { title: "Generate a standalone HTML file for this world", onclick: () => generateHTML(domain) }, "Generate a standalone HTML file for this world"),
        )
    }
}
