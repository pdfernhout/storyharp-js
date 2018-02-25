
// Exported from: ../../storyharp-js/converted_source/uscontextwizard.lfm
import * as m from "mithril"

import { viewConsoleForm } from "../source/ConsoleForm"

import { TWorld } from "../source/TWorld"
import { TSCommandList } from "../source/TSCommandList";

const world = new TWorld()
const commandList = new TSCommandList(world)
const transcript: {text: string, color: number}[] = []

const fakeDomain = {
    world,
    commandList,
    transcript,
    consoleForm: {
        speechSystem: {
            lastSaidTextWithMacros: "TEST",
            stripMacros: (text: string) => text,
            sayTextWithMacros: (text: string) => null,
            listenForAvailableCommands: () => null,
            checkForSayOptionsMacro: () => null,
            speakText: (text: string) => null,
        },
        addLineToTranscript: (text: string, color: number) => fakeDomain.transcript.push({text, color}),
        scrollTranscriptEndIntoView: () => null,
        updateVariables: () => null,
        VariablesListBox: {
            Invalidate: () => null
        }
    },
    ruleEditorForm: {},
    availableWorldFiles: [
        "Astronomy Test",
        "GarTrek",
        "Grue Pit",
        "House and Yard",
        "insurance",
        "interview",
        "Intro",
        "java1",
        "java2",
        "Max the Computer",
        "prompter",
        "testing",
        "Tutorial Basic",
        "Tutorial Intermediate",
        "Tutorial Advanced",
    ],
    loadedFileName: "",
    loadTestWorld: null
}

const MyComponent = { view: () => viewConsoleForm(fakeDomain) }

async function loadTestWorld(worldFileName: string) {

    const worldContent = await m.request("../data/" + worldFileName + ".wld", {deserialize: (text) => text})

    fakeDomain.world.reportModeCallback = function(text: string) { /* fakeDomain.transcript.push(text) */ }

    world.resetVariablesAndRules()
    const loaded = fakeDomain.world.loadWorldFromFileContents(worldContent)
    if (!loaded) throw new Error("Failed to load")

    // transcript.push("Playing: " + worldFileName)
    fakeDomain.loadedFileName = worldFileName

    fakeDomain.world.newSession()

    m.mount(document.body, MyComponent)
}


fakeDomain.loadTestWorld = <any>loadTestWorld

// loadTestWorld("GarTrek")
loadTestWorld("House and Yard")
