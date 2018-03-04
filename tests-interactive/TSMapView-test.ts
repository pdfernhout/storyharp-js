
// Exported from: ../../storyharp-js/converted_source/uscontextwizard.lfm
import * as m from "mithril"

import { TWorld } from "../source/TWorld"
import { TSCommandList } from "../source/TSCommandList"
import { Color } from "../source/common"
import { TSRuleField } from "../source/TSRule"
import { TPoint } from "../source/TPoint"
import { TSMapView } from "../source/TSMapView"
import { TRect } from "../source/TRect"

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
        goodPosition: () => new TPoint(0, 0)
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

const mapDrawer = new TSMapView()

const MyComponent = { 
    view: () => m("div", 
        m("canvas.ba", {
            onupdate(vnode: m.VnodeDOM) {
                const canvas = <HTMLCanvasElement>vnode.dom

                // Clear the canvas
                canvas.width = canvas.width

                const context = canvas.getContext("2d")
                if (!context) return
                context.beginPath()
                context.moveTo(0, 0)
                context.lineTo(100, 100)
                context.closePath()
                context.strokeStyle = "#00FFFF"
                context.stroke()

                mapDrawer.drawArrowhead(context, new TPoint(100, 40), new TPoint(120, 40))

                context.strokeStyle = "#00FF00"
                const r1 = new TRect(10, 110, 30, 130)
                const r2 = new TRect(70, 120, 90, 140)
                const r3 = new TRect(170, 20, 190, 40)
                mapDrawer.drawRect(context, r1)
                mapDrawer.drawRect(context, r2)
                mapDrawer.drawRect(context, r3)

                mapDrawer.drawArrowFromRectEdgeToRectEdge(context, r1, r2)
                mapDrawer.drawArrowFromRectEdgeToRectEdge(context, r2, r3)
                mapDrawer.drawArrowFromRectEdgeToRectEdge(context, r3, r1)

                const p1 = new TPoint(r3.Left, r3.Top)
                mapDrawer.drawCommandOrContext(context, "Hello, world!", r3, p1, false, false, false)
            },
            onclick: () => mapDrawer.scroll.X -= 20,
        }),
        m("div", "There should be a canvas above with a line from top left most of the way towards the bottom middle."),
        m("canvas.ba.ma4", {
            width: 500,
            height: 500,
            onupdate(vnode: m.VnodeDOM) {
                const canvas = <HTMLCanvasElement>vnode.dom

                // Clear the canvas
                canvas.width = canvas.width

                const context = canvas.getContext("2d")
                if (!context) return
                const displayOptions = []
                displayOptions[TSRuleField.kRuleContext] = true
                displayOptions[TSRuleField.kRuleCommand] = true
                mapDrawer.displayOn(context, displayOptions, null, null, world, null)
            },
            onclick: () => mapDrawer.scroll.X += 20,
        }),
    )
}

fakeDomain.loadTestWorld = <any>loadTestWorld

// loadTestWorld("GarTrek")
loadTestWorld("House and Yard")
