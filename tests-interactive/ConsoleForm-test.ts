
// Exported from: ../../storyharp-js/converted_source/uscontextwizard.lfm
import * as m from "mithril"

import { viewConsoleForm } from "../source/ConsoleForm"

import { TWorld } from "../source/TWorld"
import { TSCommandList } from "../source/TSCommandList"
import { Color } from "../source/common"

const world = new TWorld()
const sessionCommandList = new TSCommandList(world)
const worldCommandList = new TSCommandList(world)
const transcript: {text: string, color: number}[] = []

const fakeDomain = {
    world,
    sessionCommandList,
    worldCommandList,
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
        },
        ShowOnlyTrueVariablesButton: {}
    },
    ruleEditorForm: {
        selectEditorField: (fieldIndex: number) => null,
        updateForRuleChange: () => null,
        scrollGridSelectionsIntoView: () => null,
        RuleGrid: {
            Invalidate: () => null
        },
        updateRuleNumberLabel: () => null,
        editRule: (rule: any) => null,
    },
    changeLogForm: {
        addToLog: (text: string) => null
    },
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
    loadTestWorld: null,
    editedRule: null,
    lastSingleRuleIndex: 0
}

const MyComponent = { view: () => viewConsoleForm(fakeDomain) }

async function loadTestWorld(worldFileName: string) {

    const worldContent = await m.request("../data/" + worldFileName + ".wld", {deserialize: (text) => text})

    fakeDomain.world.reportModeCallback = function(text: string) { /* fakeDomain.transcript.push(text) */ }

    world.resetVariablesAndRules()
    const loaded = fakeDomain.world.loadWorldFromFileContents(worldContent)
    if (!loaded) throw new Error("Failed to load")

    transcript.push({text: "Playing: " + worldFileName, color: Color.clGreen})
    fakeDomain.loadedFileName = worldFileName

    fakeDomain.world.newSession()
    fakeDomain.sessionCommandList.clear()
    fakeDomain.worldCommandList.clear()
    fakeDomain.editedRule = null
    fakeDomain.lastSingleRuleIndex = 0

    m.mount(document.body, MyComponent)
}


fakeDomain.loadTestWorld = <any>loadTestWorld

// loadTestWorld("GarTrek")
loadTestWorld("House and Yard")
