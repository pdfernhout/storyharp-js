import * as m from "mithril"

import { Color, expander, makeFileNameWithWldExtension, makeFileNameWithoutWldExtension } from "./common"
import { TWorld, ExportRulesOption } from "./TWorld"
import { VariablesView } from "./VariablesView"
import { RuleEditorForm } from "./RuleEditorForm"
import { FileUtils } from "./FileUtils"
import { authoringHelp } from "./authoringHelp"
import { TSDomain, DemoEntry } from "./TSDomain"
import { storyHarpVersion } from "./version"

const firstRiddleAnswer = "say an answer for a riddle"

function availableCommands(world: TWorld, showRiddleAnswers=false): string[] {
    const result: string[] = []

    for (let rule of world.rules) {
        if (rule.available) {
            if (rule.command.phrase === "") {
                continue
            }
            let command = rule.command.phrase
            if (!showRiddleAnswers && rule.command.phrase.startsWith("$")) {
                // only list first riddle answer
                command = firstRiddleAnswer
            }
            if (result.indexOf(command) === -1) {
                result.push(command)
            }
        }
    }
    return result
}

function doCommand(domain: TSDomain, commandPhrase: string) {
    // console.log("doCommand", commandPhrase)
    if (commandPhrase === firstRiddleAnswer) {
        // for riddles - need to be reassembled into command string first
        const answer = prompt("Please enter the answer to a riddle. [case-sensitive]", "")
        if (!answer) return
        commandPhrase = "answer " + answer
    }
    let commandPhraseModified
    if ((availableCommands(domain.world, true).indexOf(commandPhrase) === -1)) {
        commandPhraseModified = "$" + commandPhrase
    } else {
        commandPhraseModified = commandPhrase
    }
    // console.log("test", commandPhraseModified, availableCommands(domain.world, true))
    if ((availableCommands(domain.world, true).indexOf(commandPhraseModified) === -1)) {
        if (commandPhrase.startsWith("$")) {
            // elimitate leading $
            commandPhrase = commandPhrase.substring(1)
        }
        domain.consoleForm.addLineToTranscript("> " + commandPhrase, Color.clBlue)
        // bug - or bad riddle answer.
        domain.consoleForm.addLineToTranscript("That accomplishes nothing.", Color.clBlack)
        return
    }   
    domain.sessionCommandList.doCommandPhrase(commandPhraseModified)

    // TODO: Track last command where editor switches the edited rule
    /*
    if domain.options.updateEditorAfterCommandDone then
        RuleEditorForm.trackLastCommand
        end
    */

}

function scrollIntoView() {
    const buttons = document.getElementById("undoRedoButtons")
    if (buttons) buttons.scrollIntoView()
}

function viewChoices(domain: TSDomain) {
    const commands = availableCommands(domain.world)
    Promise.resolve().then(scrollIntoView)
    return m("div", 
        m("hr"),
        commands.sort().map(command => m("div.ma2.dark-blue.hover-blue", {
            onclick: () => doCommand(domain, command),
        }, command)),
        m("hr"),
        m("div#undoRedoButtons.ma2",
            m("button.ml2.w4", {
                disabled: !domain.sessionCommandList.isUndoEnabled(),
                onclick: () => domain.sessionCommandList.undoLast(),
                title: "Undo " + domain.sessionCommandList.undoDescription()
            }, "Undo"),
            m("button.ml2.w4", { 
                disabled: !domain.sessionCommandList.isRedoEnabled(),
                onclick: () => domain.sessionCommandList.redoLast(),
                title: "Redo " + domain.sessionCommandList.redoDescription()
            }, "Redo"), 
        )
    )
}

type FormName = "about" | "console" | "files" | "ruleEditor"
let activeForm: FormName = "console"

function color(color: Color) {
    switch (color) {
        case Color.clBlue: return ".blue"
        case Color.clGreen: return ".green"
        case Color.clBlack: return ".black"
        case Color.clRed: return ".red"
        default: return ""
    }
}

function viewDemoFiles(domain: TSDomain) {
    return m("div.overflow-auto", { style: "height: calc(100% - 7rem)" },
        m("div", "Choose a demo world file to load:"),
        m("br"),
        m("table.ml2", { style: "border-spacing: 0.5rem" },
            m("tr",
                m("th", "Name"),
                m("th.ml2", "Description")
            ),
            domain.demoConfig.demoWorldFiles.map((entry: DemoEntry) => 
                m("tr.mt1", 
                    { onclick: () => {
                        if (!confirmUnsavedChangesLoss(domain)) return
                        domain.loadWorldFromServerData(entry.name).then((loaded) => {
                            if (loaded) activeForm = "console"
                        })
                    }
                },
                    m("td.nowrap.tr.f4", entry.name),
                    m("td.ml2.i", entry.description)
                )
            )
        )
    )
}

let showAuthoringHelp = false

function viewAbout(domain: TSDomain) {
    return m("div.overflow-auto", { style: "height: calc(100% - 7rem)" },
        m("p", `
            StoryHarp is an interactive environment for playing and creating
            interactive CYOA (choose your own adventure) stories.
        `),
        m("p"),
        m("div", { onclick: () => showAuthoringHelp = !showAuthoringHelp }, expander(showAuthoringHelp, "(Click for:) ") + "Authoring help"),
        showAuthoringHelp ? authoringHelp.split("\n\n").map(text => m("p", text)) : [],
        m("hr"),
        m("p", "StoryHarp 1.0 was originally a stand-alone desktop program in Delphi. Version 2.0 was in Java but was not released. Version 3.0 is web-based."),
        m("p"),
        m("p", "StoryHarp 3.0 Copyright Paul D. Fernhout and Cynthia F. Kurtz 1998-2008"),
        m("p", "StoryHarp is a trademark of Paul D. Fernhout and Cynthia F. Kurtz")
    )
}

function viewConsole(domain: TSDomain) {
    return m("div.overflow-auto", { style: "height: calc(100% - 5rem)" },
        m("div",
            domain.transcript.map((item: any) => m("div.mw6" + color(item.color), item.text)),
        ),
        viewChoices(domain),
        m(VariablesView, <any>{domain}),
    )
}

function resetConsole(domain: TSDomain) {
    if (!confirm("Are you sure you want to restart the world?")) return
    domain.newSession()
}

function saveWorldToLocalFile(domain: TSDomain) {
    const world: TWorld = domain.world
    const fileName = makeFileNameWithoutWldExtension(domain.worldFileName)
    FileUtils.saveToFile(fileName, world.saveWorldToFileContents(ExportRulesOption.kSaveAllRules), ".wld", (fileName: string) => {
        console.log("written", fileName)
        domain.worldFileName = makeFileNameWithWldExtension(fileName)
        domain.resetWorldChangeCount()
        m.redraw()
    })
}

function confirmUnsavedChangesLoss(domain: TSDomain) {
    if (domain.isWorldFileChanged()) {
        if (!confirm("You have unsaved changes to the current world which will be lost; proceed anyway?")) {
            return false
        }
    }
    return true
}

function loadWorldFromLocalFile(domain: TSDomain) {
    if (!confirmUnsavedChangesLoss(domain)) return

    const world: TWorld = domain.world
    FileUtils.loadFromFile(false, (fileName: string, contents: string) => {
        console.log("chose", fileName)

        world.resetVariablesAndRules()

        const loaded = world.loadWorldFromFileContents(contents)
        console.log("load status", loaded)

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

export function viewConsoleForm(domain: TSDomain) {

    function buttonWithHighlight(selection: FormName) {
        return "button.ml2" + (activeForm === selection ? ".bg-light-blue" : "")
    }

    return m(".ConsoleForm.ml3.h-100.overflow-hidden",
        m("div.mt1.mb2",
            m("span.f5.b.mr3.dib", "StoryHarp CYOA Player and Editor v" + storyHarpVersion),
            m("span", "World: "),
            m("span.i", "" + makeFileNameWithoutWldExtension(domain.worldFileName)),
        ),
        m("div.mb3",
            m(buttonWithHighlight("about"), { onclick: () => activeForm = "about" }, "About"),
            m(buttonWithHighlight("files"), { onclick: () => activeForm = "files" }, "Demos"),
            m(buttonWithHighlight("console"), { onclick: () => { activeForm = "console"; domain.world.updateAvailable() }}, "Console"),
            m(buttonWithHighlight("ruleEditor"), { onclick: () => activeForm = "ruleEditor" }, "Rule Editor"),
            (activeForm !== "about" && activeForm !== "files")
                ? m("button.ml4", { title: "Open a world file", onclick: () => loadWorldFromLocalFile(domain) }, "Load World")
                : [],
            activeForm === "console" 
                ? m("button.ml2.mr4", { title: "Reset current world", onclick: () => resetConsole(domain) }, "Restart World")
                : [],
            activeForm === "ruleEditor" 
                ? [
                    m("button.ml1", { title: "Save a world file", onclick: () => saveWorldToLocalFile(domain) }, "Save World"),
                    m("button.ml3", { title: "Make a new world", onclick: () => newWorld(domain) }, "New World"),
                ]
                : []
        ),
        activeForm === "about" ? viewAbout(domain) : [],
        activeForm === "files" ? viewDemoFiles(domain) : [],
        // TODO: Probably should wrap these with hidden divs so the component state is preserved
        activeForm === "console" ? viewConsole(domain) : [],
        activeForm === "ruleEditor" ? m(RuleEditorForm, <any>{domain: domain}) : [],
    )
}
