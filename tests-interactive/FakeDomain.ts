import * as m from "mithril"
import { TWorld } from "../source/TWorld"
import { TSCommandList } from "../source/TSCommandList"
import { TSRuleField } from "../source/TSRule"
import { TSDomain, DemoConfig } from "../source/TSDomain"
import { TPoint } from "../source/TPoint"
import { Color } from "../source/common";

const world = new TWorld()
const sessionCommandList = new TSCommandList(world)
const worldCommandList = new TSCommandList(world)
const transcript: {text: string, color: number}[] = []

async function loadTestWorld(worldFileName: string) {
    const domain = FakeDomain

    if (!domain.demoConfig.demoWorldFiles.length) {
        domain.demoConfig = <DemoConfig>await m.request("../data/demoConfig.json")
    }

    const worldContent = await m.request("../data/" + worldFileName + ".wld", {deserialize: (text) => text})

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
}

export const FakeDomain: TSDomain = {
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
        addLineToTranscript: (text: string, color: number) => FakeDomain.transcript.push({text, color}),
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
        goodPosition: () => new TPoint(0, 0)
    },
    changeLogForm: {
        addToLog: (text: string) => null
    },
    demoConfig: {
        demoWorldFiles: []
    },
    loadedFileName: "",
    loadTestWorld,
    editedRule: null,
    lastSingleRuleIndex: 0,
    currentEditorView:"table",
    currentEditorWizard: "context",
    // set by the browser as callback
    setOrganizeByField: (newValue: TSRuleField) => null,
}
