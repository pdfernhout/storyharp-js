
// Exported from: ../../storyharp-js/converted_source/uscontextwizard.lfm
import * as m from "mithril"

import { viewConsoleForm } from "../source/ConsoleForm"

import { TWorld } from "../source/TWorld"
import { TSCommandList } from "../source/TSCommandList"
import { Color } from "../source/common"
import { TSRuleField } from "../source/TSRule"

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
    lastSingleRuleIndex: 0,
    currentEditorView:"table",
    currentEditorWizard: "context",
    // set by the browser as callback
    setOrganizeByField: (newValue: TSRuleField) => null,
}

const MyComponent = { view: () => viewConsoleForm(fakeDomain) }

async function loadTestWorld(worldFileName: string) {
    const domain = fakeDomain

    const worldContent = await m.request("../data/" + worldFileName + ".wld", {deserialize: (text) => text})

    domain.world.reportModeCallback = function(text: string) { /* domain.transcript.push(text) */ }

    world.resetVariablesAndRules()
    const loaded = domain.world.loadWorldFromFileContents(worldContent)
    if (!loaded) throw new Error("Failed to load")

    domain.loadedFileName = worldFileName

    domain.world.newSession()
    domain.sessionCommandList.clear()
    domain.worldCommandList.clear()
    domain.editedRule = null
    domain.lastSingleRuleIndex = 0
    transcript.length = 0
    transcript.push({text: "Starting: " + worldFileName, color: Color.clGreen})
    /*
    if (domain.world.rules.length) {
        domain.transcript.push({text: "> " + domain.world.rules[0].command.phrase, color: Color.clBlue})
        domain.transcript.push({text: domain.world.rules[0].reply, color: Color.clBlack})
    }
    */
    m.mount(document.body, MyComponent)
}


fakeDomain.loadTestWorld = <any>loadTestWorld

// loadTestWorld("GarTrek")
loadTestWorld("House and Yard")
