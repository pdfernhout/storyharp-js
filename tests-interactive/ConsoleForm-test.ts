
// Exported from: ../../storyharp-js/converted_source/uscontextwizard.lfm
import * as m from "mithril"

import { viewConsoleForm } from "../source/ConsoleForm"

import { TWorld } from "../source/TWorld"
import { TSCommandList } from "../source/TSCommandList";

const world = new TWorld()
const commandList = new TSCommandList(world)
const transcript = [
    "Hello, StoryHarp!",
    "Welcome to an Audioventure"
]

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
    const GarTrekWorldContents = await m.request("../data/GarTrek.wld", {deserialize: (text) => text})

    fakeDomain.world.reportModeCallback = function() {}

    const loaded = fakeDomain.world.loadWorldFromFileContents(GarTrekWorldContents)
    if (!loaded) throw new Error("Failed to load")

    fakeDomain.world.newSession()

    m.mount(document.body, MyComponent)
}

loadTestWorld()
