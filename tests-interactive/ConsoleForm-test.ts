
// Exported from: ../../storyharp-js/converted_source/uscontextwizard.lfm
import * as m from "mithril"

import { viewConsoleForm } from "../source/ConsoleForm"

import { TWorld } from "../source/TWorld"
import { TSCommandList } from "../source/TSCommandList";

const world = new TWorld()
const commandList = new TSCommandList(world)
const transcript: string[] = []

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
        addLineToTranscript: (text: string) => transcript.push(text),
        scrollTranscriptEndIntoView: () => null,
        updateVariables: () => null,
        VariablesListBox: {
            Invalidate: () => null
        }
    },
    ruleEditorForm: {}
}

const MyComponent = { view: () => viewConsoleForm(fakeDomain) }

async function loadTestWorld() {
    //const worldFileName = "../data/GarTrek.wld"
    const worldFileName = "../data/House and Yard.wld"

    const worldContent = await m.request(worldFileName, {deserialize: (text) => text})

    fakeDomain.world.reportModeCallback = function() {}

    const loaded = fakeDomain.world.loadWorldFromFileContents(worldContent)
    if (!loaded) throw new Error("Failed to load")

    transcript.push("Playing: " + worldFileName.substring(worldFileName.lastIndexOf("/") + 1))

    fakeDomain.world.newSession()

    m.mount(document.body, MyComponent)
}

loadTestWorld()
