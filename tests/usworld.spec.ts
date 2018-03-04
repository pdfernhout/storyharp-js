import * as fs from "fs"

import * as o from "ospec"

import { TWorld } from "../source/TWorld"

const GarTrekWorldContents = fs.readFileSync("../data/GarTrek.wld").toString()
const GarTrekSessionContents = fs.readFileSync("../data/just at sphinx 2.ses").toString()
const Java1WorldContents = fs.readFileSync("../data/java1.wld").toString().replace(/\r/g, "")

o.spec("usworld", () => {

    o.spec("Tworld", () => {

        o("new", () => {
            const world = new TWorld()
            o(world).notEquals(null)
        })

        o("load world with missing contents", () => {
            const world = new TWorld()
            try {
                const loaded = world.loadWorldFromFileContents("")
                o(loaded).equals(true)
            } catch (e) {
                o(e.message).equals("Unexpected EOF loading file")
            }
        })

        o("load world with bad contents", () => {
            const world = new TWorld()
            try {
                const loaded = world.loadWorldFromFileContents("Not a robot")
                o(loaded).equals(true)
            } catch (e) {
                o(e.message).equals("File header for world file is not correct")
            }
        })

        o("load world for GarTrek", () => {
            const world = new TWorld()
            const loaded = world.loadWorldFromFileContents(GarTrekWorldContents)
            o(loaded).equals(true)
        })

        o("save world for java1", () => {
            const world = new TWorld()
            const loaded = world.loadWorldFromFileContents(Java1WorldContents)
            o(loaded).equals(true)
            const contents = world.saveWorldToFileContents(false)
            o(contents).equals(Java1WorldContents.replace("; world file version 1.0", "; StoryHarp world file version 1.3"))
        })

        o("load session for GarTrek", () => {
            const world = new TWorld()
            const loadedWorld = world.loadWorldFromFileContents(GarTrekWorldContents)
            o(loadedWorld).equals(true)
            o(world.focus).equals(null)
            const loadedSession = world.loadSessionFromFile("GarTrek.wld", GarTrekSessionContents)
            o(loadedSession).equals(true)
            if (!world.focus) throw "focus is null"
            o(world.focus.phrase).equals("<sphinx>")
        })

        o("saying phrase in GarTrek", () => {
            // can't easily do this without commands
            const world = new TWorld()
            world.loadWorldFromFileContents(GarTrekWorldContents)
            world.loadSessionFromFile("GarTrek.wld", GarTrekSessionContents)
            if (!world.focus) throw "focus is null"
            o(world.focus.phrase).equals("<sphinx>")
            // Manually activate a rule -- normally a command would do this
            const rule = world.rules.find(rule => rule.command.phrase === "$answer herring")
            if (!rule) throw "rule is null"
            const result = rule.recordReplyMoveChanges([], "")
            if (!result.contextToFocusTo) throw "result.contextToFocusTo is null"
            o(result.contextToFocusTo.phrase).equals("nether regions")
            o(result.totalReply.startsWith("The Sphinx says: ")).equals(true)
        })  
    })
})
