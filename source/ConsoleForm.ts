import * as m from "mithril"

import { Color } from "./common"
import { TWorld } from "./TWorld"

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
    console.log("doCommand", commandPhrase)
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
    console.log("test", commandPhraseModified, availableCommands(domain.world, true))
    if ((availableCommands(domain.world, true).indexOf(commandPhraseModified) === -1)) {
        if (commandPhrase.startsWith("$")) {
            // elimitate leading $
            commandPhrase = commandPhrase.substring(1)
        }
        domain.consoleForm.addLineToTranscript("> " + commandPhrase, Color.clRed);
        // bug - or bad riddle answer.
        domain.consoleForm.addLineToTranscript("That accomplishes nothing.", Color.clBlue)
        return
    }   
    domain.commandList.doCommandPhrase(domain.consoleForm, domain.ruleEditorForm, commandPhraseModified)
}

function viewChoices(domain: any) {
    const commands = availableCommands(domain.world)
    return m("div", 
        m("hr"),
        m("div", "You can choose from:"),
        commands.sort().map(command => m("div.ma2", { onclick: () => doCommand(domain, command) }, command))
    )
}

export function viewConsoleForm(domain: any) {
    return m(".ConsoleForm",
        m("h3", "StoryHarp 2.0 CYOA Player and Editor"),
        domain.transcript.map((text: string) => m("div.mw6", text)),
        viewChoices(domain)
    )
}
