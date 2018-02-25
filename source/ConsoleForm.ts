import * as m from "mithril"

import { Color } from "./common"
import { TWorld } from "./TWorld"
import { VariablesView } from "./VariablesView"
import { RuleEditorForm } from "./RuleEditorForm"

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

function doCommand(domain: any, commandPhrase: string) {
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
        domain.consoleForm.addLineToTranscript("> " + commandPhrase, Color.clBlue);
        // bug - or bad riddle answer.
        domain.consoleForm.addLineToTranscript("That accomplishes nothing.", Color.clBlack)
        return
    }   
    domain.sessionCommandList.doCommandPhrase(domain.consoleForm, domain.ruleEditorForm, commandPhraseModified)
}

function scrollIntoView() {
    const buttons = document.getElementById("undoRedoButtons")
    if (buttons) buttons.scrollIntoView()
}

function viewChoices(domain: any) {
    const commands = availableCommands(domain.world)
    Promise.resolve().then(scrollIntoView)
    return m("div", 
        m("hr"),
        m("div", "You can choose from:"),
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

type formName = "console" | "files" | "ruleEditor"
let activeForm: formName = "console"

function color(color: Color) {
    switch (color) {
        case Color.clBlue: return ".blue"
        case Color.clGreen: return ".green"
        case Color.clBlack: return ".black"
        case Color.clRed: return ".red"
        default: return ""
    }
}

function viewFiles(domain: any) {
    return domain.availableWorldFiles.map((name: string) => 
        m("div.mt1", 
            { onclick: () => {
            domain.transcript.length = 0
            domain.loadTestWorld(name)
            activeForm = "console"
            }
        }, name)
    )
}

function viewConsole(domain: any) {
    return [
        domain.transcript.map((item: any) => m("div.mw6" + color(item.color), item.text)),
        viewChoices(domain),
        m(VariablesView, <any>{domain}),
    ]
}

export function viewConsoleForm(domain: any) {
    return m(".ConsoleForm.ml3",
        m("h3", "StoryHarp 2.0 CYOA Player and Editor"),
        m("div.mb3",
            "Playing: " + domain.loadedFileName,
            activeForm === "files" ? [] : m("button.ml2", { onclick: () => activeForm = "files" }, "Files"),
            activeForm === "console" ? [] : m("button.ml2", { onclick: () => activeForm = "console" }, "Console"),
            activeForm === "ruleEditor" ? [] : m("button.ml2", { onclick: () => activeForm = "ruleEditor" }, "Rule Editor"),
        ),
        activeForm === "console" ? viewConsole(domain) : [],
        activeForm === "files" ? viewFiles(domain) : [],
        activeForm === "ruleEditor" ? m(RuleEditorForm, <any>{domain: domain}) : [],
    )
}
