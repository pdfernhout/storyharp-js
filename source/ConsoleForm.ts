import * as m from "mithril"

import { Color } from "./common"
import { TWorld } from "./TWorld"
import { VariablesView } from "./VariablesView"
import { TSDomain, TranscriptLine } from "./TSDomain"
import { loadWorldFromLocalFile } from "./FileForm";

// TODO: Shutdown sound when press escape key

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

export function doCommand(domain: TSDomain, commandPhrase: string) {
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
            // eliminate leading $
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

function viewChoices(domain: TSDomain, scrollCallback: () => void) {
    const commands = availableCommands(domain.world)
    // Promise.resolve().then(scrollIntoView)
    return m("div", 
        m("hr"),
        [ // Put this in an array to keep keys separate from non-keyed items
            commands.sort().map(command => m("button.ma2.dark-blue.hover-blue", {
                key: command,
                onclick: () => {
                    doCommand(domain, command)
                    scrollCallback()
                },
            }, command)),
        ],
    )
}

function color(color: Color) {
    switch (color) {
        case Color.clBlue: return ".blue"
        case Color.clGreen: return ".green"
        case Color.clBlack: return ".black"
        case Color.clRed: return ".red"
        default: return ""
    }
}

export enum SegmentType {
    speakText,
    sayOptionsMacroInForce,
    showPicture,
    speakSound,
    speakMusic
}

interface Segment {
    type: SegmentType
    text: string
}

export function parseTextWithMacros(aString: string): Segment[] { 
    const result: Segment[] = []   
    let remaining = aString
    const wholeLength = aString.length
    while (remaining.length > 0) {
        const startPosition = remaining.indexOf("{")
        if (startPosition !== -1) {
            const toSay = remaining.substring(0, startPosition - 1)
            if (toSay.trim() !== "") {
                result.push({type: SegmentType.speakText, text: toSay})
            }
            remaining = remaining.substring(startPosition + 1)
        } else {
            if (remaining.trim() !== "") {
                result.push({type: SegmentType.speakText, text: remaining})
            }
            return result
        }
        const endPosition = remaining.indexOf("}")
        if (endPosition === -1) {
            // error - unmatched braces
            console.log("Error == unmatched braces")
            result.push({type: SegmentType.speakText, text: remaining})
            return result
        }
        const macro = remaining.substring(0, endPosition).trim()
        remaining = remaining.substring(endPosition + 1)
        if (macro.startsWith("options")) {
            // cfk added
            // TODO: use or remove: sayOptionsMacroInForce = true
            result.push({type: SegmentType.sayOptionsMacroInForce, text: ""})
        } else if (macro.startsWith("picture ")) {
            result.push({type: SegmentType.showPicture, text: macro.substring("picture ".length)})
        } else if (macro.startsWith("music ")) {
            result.push({type: SegmentType.speakMusic, text: macro.substring("music ".length)})
        } else if (macro.startsWith("sound ")) {
            // TODO: legacy from when did not prefix sounds? Maybe should require that? 
            result.push({type: SegmentType.speakSound, text: macro.substring("sound ".length)})
        } else {
            // TODO: legacy from when did not prefix sounds? Maybe should require that? 
            result.push({type: SegmentType.speakSound, text: macro})
        }
    }
    return result
}

function viewTranscriptItem(item: TranscriptLine) {
    // {picture http://www.kurtz-fernhout.com/StoryHarp2.gif}
    // {picture https://upload.wikimedia.org/wikipedia/commons/thumb/0/06/Kitten_in_Rizal_Park%2C_Manila.jpg/345px-Kitten_in_Rizal_Park%2C_Manila.jpg}
    const segments = parseTextWithMacros(item.text)
    return m("div.mw6" + color(item.color),
        {
            key: item.uuid,
        },
        segments.map(segment => {
            switch (segment.type) {
                case SegmentType.speakText:
                    if (item.color === Color.clBlue) return m("div.ma2", segment.text)
                    return m("div.ma1.ml3", segment.text)
                case SegmentType.sayOptionsMacroInForce:
                    return []
                case SegmentType.showPicture:
                    return m("div", m("img.ml3", {
                        src: segment.text,
                        /* to center the image
                        style: {
                            display: "block",
                            "margin-left": "auto",
                            "margin-right": "auto",
                        }
                        */
                    }))
                case SegmentType.speakSound:
                    // console.log("Unfinished sound handling", segment)
                    return []
                case SegmentType.speakMusic:
                    // console.log("Unfinished music handling", segment)
                    return []
                default:
                    throw new Error("unexpected segment type: " + JSON.stringify(segment))
            }
        })
    )
}

function resetConsole(domain: TSDomain) {
    if (!confirm("Are you sure you want to restart playing the world?")) return
    domain.newSession()
}

export class ConsoleForm {
    domain: TSDomain
    transcriptDiv: HTMLDivElement

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    scrollEndOfTranscriptIntoView() {
        if (this.transcriptDiv) {
            const transcriptDiv = this.transcriptDiv
            setTimeout(() => {
                transcriptDiv.scrollTop = transcriptDiv.scrollHeight
            }, 0)
        }
    }

    view() {
        const domain = this.domain

        return m("div.ConsoleForm.flex.flex-column", { style: "height: calc(100% - 5rem)" },
            m("div.ml2.mb2.flex-none",
                // m("button", { title: "Open a world file", onclick: () => loadWorldFromLocalFile(domain) }, "Load"),
                m("button.ml2.mr4", {
                    title: "Reset current world",
                    onclick: () => resetConsole(domain)
                }, "Restart session"),
                m("button.ml3.w4", {
                    disabled: !domain.sessionCommandList.isUndoEnabled(),
                    onclick: () => {
                        domain.sessionCommandList.undoLast()
                        this.scrollEndOfTranscriptIntoView()
                    },
                    title: "Undo " + domain.sessionCommandList.undoDescription()
                }, "Undo"),
                m("button.ml1.w4", { 
                    disabled: !domain.sessionCommandList.isRedoEnabled(),
                    onclick: () => {
                        domain.sessionCommandList.redoLast()
                        this.scrollEndOfTranscriptIntoView()
                    },
                    title: "Redo " + domain.sessionCommandList.redoDescription()
                }, "Redo"), 
            ),
            m("div.flex-auto.overflow-auto",
                {
                    oncreate: (vnode: any) => {
                    this.transcriptDiv = <HTMLDivElement>(vnode.dom)
                    },
                },
                domain.transcript.map(viewTranscriptItem),
            ),
            m("div.flex-none",
                viewChoices(domain, this.scrollEndOfTranscriptIntoView.bind(this)),
            ),
            m(VariablesView, <any>{domain}),
        )
    }
}
