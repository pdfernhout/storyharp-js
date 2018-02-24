import * as m from "mithril"

import { TWorld } from "./TWorld"

function availableCommands(world: TWorld): string[] {
    const result: string[] = []

    for (let rule of world.rules) {
        if (rule.available) {
            if (rule.command.phrase === "") {
                continue
            }
            const command = rule.command.phrase
            /*
            if ((UNRESOLVED.pos("$", rule.command.phrase) === 1)) {
                // only list first riddle answer
                command = firstRiddleAnswer
            }
            */
            if (result.indexOf(command) === -1) {
                result.push(command)
            }
        }
    }
    return result
}

function doCommand(domain: any, command: string) {
    console.log("doCommand", command)
    domain.commandList.doCommandPhrase(domain.consoleForm, domain.ruleEditorForm, command)
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
        m("h1", "StoryHarp-js"),
        domain.transcript.map((text: string) => m("div.mw6", text)),
        viewChoices(domain)
    )
}
