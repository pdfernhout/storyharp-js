import * as m from "mithril"
import { TWorld } from "../source/TWorld"
import { TSCommandList } from "../source/TSCommandList"
import { TSRuleField } from "../source/TSRule"
import { TSDomain, DemoConfig } from "../source/TSDomain"
import { TPoint } from "../source/TPoint"
import { Color } from "../source/common"

const world = new TWorld()

// A cicularity here means the domain needs to be initialized later
const sessionCommandList = new TSCommandList(<any>null)
const worldCommandList = new TSCommandList(<any>null)

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
        addLineToTranscript: (text: string, color: number) => FakeDomain.transcript.push({text, color}),
        scrollTranscriptEndIntoView: () => null,
    },
    ruleEditorForm: {
        selectEditorField: (fieldIndex: number) => null,
        scrollGridSelectionsIntoView: () => null,
        lastChoice: null,
        lastCommand: null
    },
    changeLogForm: {
        addToLog: (text: string) => null
    },
    speechSystem: {
        lastSaidTextWithMacros: "TEST",
        stripMacros: (text: string) => text,
        sayTextWithMacros: (text: string) => null,
        listenForAvailableCommands: () => null,
        checkForSayOptionsMacro: () => null,
        speakText: (text: string) => null,
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
    showCommandPrefixInMap: false,
    // set by the browser as callback
    setOrganizeByField: (newValue: TSRuleField) => null,
}

// Fixup the references to the domain due to circularity
sessionCommandList.domain = FakeDomain
worldCommandList.domain = FakeDomain
