import * as fs from "fs"

import * as o from "ospec"

import { KfCommand, KfCommandChangeType } from "../source/KfCommand"
import { TSCommandList } from "../source/TSCommandList"
import { TWorld } from "../source/TWorld"

const GarTrekWorldContents = fs.readFileSync("../data/GarTrek.wld").toString()
const GarTrekSessionContents = fs.readFileSync("../data/just at sphinx 2.ses").toString()

o.spec("TSCommandList", () => {

    o("new", () => {
        const commandList = new TSCommandList(<any>null)
        o(commandList).notEquals(null)
    })

    o("do and redo", () => {
        const commandList = new TSCommandList(<any>null)
        const command = new KfCommand()

        let commandChangeType = null
        
        command.notifyProcedure = (command, state) => {
            commandChangeType = state
        }
        
        o(command).notEquals(null)
        o(commandChangeType).equals(null)
        o(command.done).equals(false)

        commandList.doCommand(command)
        o(commandChangeType).equals(KfCommandChangeType.commandDone)
        o(command.done).equals(true)
        
        commandList.undoLast()
        o(commandChangeType).equals(KfCommandChangeType.commandUndone)
        o(command.done).equals(false)
        
        commandList.redoLast()
        o(commandChangeType).equals(KfCommandChangeType.commandDone)
        o(command.done).equals(true)
        
        commandList.undoLast()
        o(commandChangeType).equals(KfCommandChangeType.commandUndone)
        o(command.done).equals(false)
    })

    o("say command in GarTrek", () => {
        const world = new TWorld()
        world.reportModeCallback = function() {}
        world.loadWorldFromFileContents(GarTrekWorldContents)
        world.loadSessionFromFile("GarTrek.wld", GarTrekSessionContents)

        // Mocking
        const transcript: string[] = []
        const said: string[] = []
        const consoleForm = {
            speechSystem: {
                lastSaidTextWithMacros: "TEST",
                stripMacros: (text: string) => text,
                sayTextWithMacros: (text: string) => said.push(text),
                listenForAvailableCommands: () => null,
                checkForSayOptionsMacro: () => null,
                speakText: (text: string) => said.push(text),
            },
            addLineToTranscript: (text: string) => transcript.push(text),
            scrollTranscriptEndIntoView: () => null,
            updateVariables: () => null,
            VariablesListBox: {
                Invalidate: () => null
            }
        }
        const ruleEditorForm = {}

        const commandList = new TSCommandList(world)

        if (!world.focus) throw "focus is null"
        o(world.focus.phrase).equals("<sphinx>")

        commandList.doCommandPhrase(consoleForm, ruleEditorForm, "$answer herring")

        o(world.focus.phrase).equals("nether regions")

        o(said.length).equals(1)
        o(said[0].includes("The Sphinx whisks you to the nether regions")).equals(true)

        commandList.undoLast()

        o(world.focus.phrase).equals("<sphinx>")

        o(said.length).equals(2)
        o(said[1]).equals(`(You decide not to say "answer herring")`)

        commandList.redoLast()

        o(world.focus.phrase).equals("nether regions")

        o(said.length).equals(3)
        o(said[2]).equals(`(You decide to say "answer herring" anyway)`)
    })

})