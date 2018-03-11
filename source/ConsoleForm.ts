import * as m from "mithril"

import { Color, expander, makeFileNameWithWldExtension, makeFileNameWithoutWldExtension } from "./common"
import { TWorld, ExportRulesOption } from "./TWorld"
import { VariablesView } from "./VariablesView"
import { RuleEditorForm } from "./RuleEditorForm"
import { FileUtils } from "./FileUtils"
import { authoringHelp } from "./authoringHelp"
import { TSDomain, DemoEntry, TranscriptLine } from "./TSDomain"
import { storyHarpVersion } from "./version"
import { TSJavaScriptWriter } from "./TSJavaScriptWriter"

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
        m("img", { style: "float: left", src: "resources/harp.png", alt: "Picture of a harp" }),
        m("h3", "StoryHarp v" + storyHarpVersion),
        m("p", `
            A tool for authoring and playing Interactive Fiction adventure stories where the player picks from a list of choices.
        `),
        m("p", "Website: ", m("a", { href: "http://storyharp.com" }, "StoryHarp.com")),
        m("p", { style: "clear: left" }),
        m("div", { onclick: () => showAuthoringHelp = !showAuthoringHelp }, expander(showAuthoringHelp, "(Click for:) ") + "Authoring Help"),
        showAuthoringHelp ? authoringHelp.split("\n\n").map(text => m("p", text)) : [],
        m("hr"),
        // m("p", "StoryHarp 1.0 was originally a stand-alone desktop program in Delphi. Version 2.0 was in Java but was not released. Version 3.0 is web-based."),
        // m("p"),
        m("p", "StoryHarp 3.0 Copyright 1998-2018 Paul D. Fernhout and Cynthia F. Kurtz"),
        m("p", "StoryHarp is a trademark of Paul D. Fernhout and Cynthia F. Kurtz")
    )
}

enum SegmentType {
    speakText,
    sayOptionsMacroInForce,
    showPicture,
    speakSound
}

interface Segment {
    type: SegmentType
    text: string
}

function parseTextWithMacros(aString: string): Segment[] { 
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
        const macro = remaining.substring(0, endPosition - 1).trim()
        remaining = remaining.substring(endPosition + 1)
        if (macro.startsWith("options")) {
            // cfk added
            // TODO: use or remove: sayOptionsMacroInForce = true
            result.push({type: SegmentType.sayOptionsMacroInForce, text: ""})
        } else if (macro.startsWith("picture ")) {
            result.push({type: SegmentType.showPicture, text: macro.substring("picture ".length)})
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
        segments.map(segment => {
            switch (segment.type) {
                case SegmentType.speakText:
                    return segment.text
                case SegmentType.sayOptionsMacroInForce:
                    return []
                case SegmentType.showPicture:
                    return m("div", m("img", {src: segment.text}))
                case SegmentType.speakSound:
                    // console.log("Unfinished sound handling", segment)
                    return []
                default:
                    throw new Error("unexpected segment type: " + JSON.stringify(segment))
            }
        })
    )
}

function viewConsole(domain: TSDomain) {
    return m("div.overflow-auto", { style: "height: calc(100% - 5rem)" },
        m("div",
            domain.transcript.map(viewTranscriptItem),
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

async function generateHTML(domain: TSDomain) {
    const writer = new TSJavaScriptWriter()
    const programText = writer.writeJavaScriptProgram(domain.world)
    if (!programText) {
        alert("Some rules and contexts must be defined first")
        return
    }
    const template = await m.request(domain.dataPath + "template.html", {deserialize: (text) => text})
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
        m.redraw()
    })
}

export function viewConsoleForm(domain: TSDomain) {

    function buttonWithHighlight(selection: FormName) {
        return "button.ml2.w4.bb-0.br3.br--top" + (activeForm === selection ? ".bg-white" : "")
    }

    function setActiveForm(event: any, formName: FormName) {
        activeForm = formName
        event.target.blur()
        if (formName === "console") {
            domain.world.updateAvailable()
        }
    }

    return m(".ConsoleForm.ml3.h-100.overflow-hidden",
        m("div.mt1.mb1",
            m("span.f5.b", "StoryHarp:"),
            // m("span.ml1", "World:"),
            m("span.i.ml1", "" + makeFileNameWithoutWldExtension(domain.worldFileName))
        ),
        m("div.mb2",
            m(buttonWithHighlight("console"), { onclick: (event: any) => setActiveForm(event, "console") }, "Player"),
            m(buttonWithHighlight("ruleEditor"), { onclick: (event: any) => setActiveForm(event, "ruleEditor") }, "Editor"),
            m(buttonWithHighlight("files"), { onclick: (event: any) => setActiveForm(event, "files") }, "Examples"),
            m(buttonWithHighlight("about"), { onclick: (event: any) => setActiveForm(event, "about") }, "About"),
            (activeForm !== "about" && activeForm !== "files")
                ? m("button.ml4", { title: "Open a world file", onclick: () => loadWorldFromLocalFile(domain) }, "Load")
                : [],
            activeForm === "console" 
                ? m("button.ml2.mr4", { title: "Reset current world", onclick: () => resetConsole(domain) }, "Restart")
                : [],
            activeForm === "ruleEditor" 
                ? [
                    m("button.ml1", { title: "Save a world file", onclick: () => saveWorldToLocalFile(domain) }, "Save"),
                    m("button.ml3", { title: "Generate a standalone HTML file for this world", onclick: () => generateHTML(domain) }, "Generate"),
                    m("button.ml3", { title: "Make a new world", onclick: () => newWorld(domain) }, "New"),
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
