import * as m from "mithril"

import { Color, ScrollIntoViewDirection } from "./common"
import { TWorld } from "./TWorld"
import { VariablesView } from "./VariablesView"
import { TSDomain, TranscriptLine, fixupPath, isMediaOK } from "./TSDomain"
// import { loadWorldFromLocalFile } from "./FileForm"
import { toast } from "./ToastView"
import { modalConfirm, modalPrompt } from "./ModalInputView"

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
    if (window.speechSynthesis && window.speechSynthesis.speaking) window.speechSynthesis.cancel()
    if (commandPhrase === firstRiddleAnswer) {
        // for riddles - need to be reassembled into command string first
        modalPrompt("Please enter the answer to a riddle. [case-sensitive]", "").then(answer => { 
            if (!answer) return
            commandPhrase = "answer " + answer
            doCommandContinued(domain, commandPhrase)
        })
    } else {
        doCommandContinued(domain, commandPhrase)
    }
}

function doCommandContinued(domain: TSDomain, commandPhrase: string) {
    let commandPhraseModified
    if ((availableCommands(domain.world, true).indexOf(commandPhrase) === -1)) {
        commandPhraseModified = "$" + commandPhrase
    } else {
        commandPhraseModified = commandPhrase
    }
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

    if (domain.updateEditorAfterCommandDone) {
        domain.editRule(domain.ruleEditorForm.lastCommand, ScrollIntoViewDirection.kFromTop, true)
    }
}

function viewChoices(domain: TSDomain, scrollCallback: () => void) {
    const commands = availableCommands(domain.world)
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
    while (remaining.length > 0) {
        const startPosition = remaining.indexOf("{")
        if (startPosition !== -1) {
            const toSay = remaining.substring(0, startPosition)
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
        } else if (macro.startsWith("picture")) {
            result.push({type: SegmentType.showPicture, text: macro.substring("picture".length).trim()})
        } else if (macro.startsWith("music")) {
            result.push({type: SegmentType.speakMusic, text: macro.substring("music".length).trim()})
        } else if (macro.startsWith("sound")) {
            result.push({type: SegmentType.speakSound, text: macro.substring("sound".length).trim()})
        } else {
            // TODO: legacy from when did not prefix sounds? Maybe should require that? 
            result.push({type: SegmentType.speakSound, text: macro.trim()})
        }
    }
    return result
}

function viewTranscriptItem(domain: TSDomain, item: TranscriptLine) {
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
                    if (domain.speechSystem.optionPicture && isMediaOK(segment.text)) {
                        return m("div", m("img.ml3", {
                            src: fixupPath(domain, segment.text),
                            /* to center the image
                            style: {
                                display: "block",
                                "margin-left": "auto",
                                "margin-right": "auto",
                            }
                            */
                        }))
                    } else {
                        return []
                    }
                case SegmentType.speakSound:
                    return []
                case SegmentType.speakMusic:
                    return []
                default:
                    throw new Error("unexpected segment type: " + JSON.stringify(segment))
            }
        })
    )
}

function resetConsole(domain: TSDomain) {
    modalConfirm("Are you sure you want to restart playing the world?").then(value => {
        if (!value) return
        domain.speechSystem.haltSpeechAndSoundAndMusic()
        domain.newSession()
        startSession(domain)
    })
}

function startSession(domain: TSDomain) {
    if (domain.world.rules.length > 0) {
        // TODO: This used to call doCommand in the speechSystem -- but made change -- consider other ramifications?
        domain.consoleForm.doCommand(domain, domain.world.rules[0].command.phrase)
    } else {
        toast("This world has no rules yet and so can't be started.")
    }
}

export class ConsoleForm {
    domain: TSDomain
    transcriptDiv!: HTMLDivElement

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

        return m("div.ConsoleForm.h-100.w-100.overflow-auto",
            {
                oncreate: (vnode: any) => {
                    this.transcriptDiv = <HTMLDivElement>(vnode.dom)
                },
            },
            domain.menuOpen && m("div.ml2.mb2",
                // m("button", { title: "Open a world file", onclick: () => loadWorldFromLocalFile(domain) }, "Load"),
                m("button.ml1.mt1", {
                    title: "Reset current world",
                    onclick: () => resetConsole(domain)
                }, "Restart session"),
                m("div.dib.ml3",
                    m("button.mt1", {
                        disabled: !domain.sessionCommandList.isUndoEnabled() || domain.sessionChangeCount <= 1,
                        onclick: () => {
                            domain.sessionCommandList.undoLast()
                            this.scrollEndOfTranscriptIntoView()
                            domain.speechSystem.haltSpeechAndSoundAndMusic()
                        },
                        title: "Undo " + domain.sessionCommandList.undoDescription()
                    }, "Undo"),
                    m("button.ml1.mt1", { 
                        disabled: !domain.sessionCommandList.isRedoEnabled(),
                        onclick: () => {
                            domain.sessionCommandList.redoLast()
                            this.scrollEndOfTranscriptIntoView()
                        },
                        title: "Redo " + domain.sessionCommandList.redoDescription()
                    }, "Redo"),
                ),
            ),
            (!domain.sessionChangeCount && domain.transcript.length <= 1)
                ? m("div",
                    m("button.ml4.mt5.w4.blue", {
                        onclick: () => startSession(domain)
                    }, m("span.f-larger", "Start ▶")))
                : [
                    m("div",
                        domain.transcript.map(each => viewTranscriptItem(domain, each)),
                    ),
                    (!domain.sessionChangeCount && domain.transcript.length <= 1) ? [] : m("div",
                        viewChoices(domain, this.scrollEndOfTranscriptIntoView.bind(this)),
                    ),
                ],
            m(VariablesView, <any>{domain}),
        )
    }
}
