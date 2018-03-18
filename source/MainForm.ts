import * as m from "mithril"

import { notebookTabButton, makeFileNameWithoutWldExtension, makeFileNameWithWldExtension } from "./common"
import { TSDomain, FormName } from "./TSDomain"
import { TWorld, ExportRulesOption } from "./TWorld"
import { AboutForm } from "./AboutForm"
import { DemoFilesForm } from "./DemoFilesForm"
import { RuleEditorForm } from "./RuleEditorForm"
import { FileUtils } from "./FileUtils"
import { TSJavaScriptWriter } from "./TSJavaScriptWriter"
import { ConsoleForm } from "./ConsoleForm"

function resetConsole(domain: TSDomain) {
    if (!confirm("Are you sure you want to restart the world?")) return
    domain.newSession()
}

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

function loadWorldFromLocalFile(domain: TSDomain) {
    if (!confirmUnsavedChangesLoss(domain)) return

    const world: TWorld = domain.world
    FileUtils.loadFromFile(false, (fileName: string, contents: string) => {
        world.resetVariablesAndRules()

        const loaded = world.loadWorldFromFileContents(contents)
        domain.addToLog("--- Read: " + fileName + (loaded ? " OK" : " Failed"))
        if (!loaded) alert("Something went wrong loading file: " + fileName)

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

async function generateHTML(domain: TSDomain) {
    const writer = new TSJavaScriptWriter()
    const programText = writer.writeJavaScriptProgram(domain.world)
    if (!programText) {
        alert("Some rules and contexts must be defined first")
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

export class MainForm {
    domain: TSDomain

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    view() {
        const domain: TSDomain = this.domain

        function setActiveForm(event: any, formName: FormName) {
            domain.activeForm = formName
            event.target.blur()
            if (formName === "console") {
                domain.world.updateAvailable()
            }
        }
    
        return m(".MainForm.ml3.h-100.overflow-hidden",
            m("div.mt1.mb1",
                m("span.f5.b", "StoryHarp:"),
                // m("span.ml1", "World:"),
                m("span.i.ml1", "" + makeFileNameWithoutWldExtension(domain.worldFileName))
            ),
            m("div.mb2",
                m(notebookTabButton(domain.activeForm === "console"), { onclick: (event: any) => setActiveForm(event, "console") }, "Player"),
                m(notebookTabButton(domain.activeForm === "ruleEditor"), { onclick: (event: any) => setActiveForm(event, "ruleEditor") }, "Editor"),
                m(notebookTabButton(domain.activeForm === "files"), { onclick: (event: any) => setActiveForm(event, "files") }, "Examples"),
                m(notebookTabButton(domain.activeForm === "about"), { onclick: (event: any) => setActiveForm(event, "about") }, "About"),
                (domain.activeForm !== "about" && domain.activeForm !== "files")
                    ? m("button.ml4", { title: "Open a world file", onclick: () => loadWorldFromLocalFile(domain) }, "Load")
                    : [],
                domain.activeForm === "console" 
                    ? m("button.ml2.mr4", { title: "Reset current world", onclick: () => resetConsole(domain) }, "Restart")
                    : [],
                domain.activeForm === "ruleEditor" 
                    ? [
                        m("button.ml1", { title: "Save a world file", onclick: () => saveWorldToLocalFile(domain) }, "Save"),
                        m("button.ml3", { title: "Generate a standalone HTML file for this world", onclick: () => generateHTML(domain) }, "Generate"),
                        m("button.ml3", { title: "Make a new world", onclick: () => newWorld(domain) }, "New"),
                    ]
                    : []
            ),
            domain.activeForm === "about" ? m(AboutForm) : [],
            domain.activeForm === "files" ? m(DemoFilesForm, <any>{domain: domain}) : [],
            domain.activeForm === "console" ? m(ConsoleForm, <any>{domain: domain}) : [],
            domain.activeForm === "ruleEditor" ? m(RuleEditorForm, <any>{domain: domain}) : [],
        )
    }
}
