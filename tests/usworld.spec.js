const o = require("ospec")
const fs = require("fs")

const TWorld = require("../dist-test/TWorld").TWorld

const GarTrekWorldContents = fs.readFileSync("data/GarTrek.wld").toString()
const GarTrekSessionContents = fs.readFileSync("data/just at sphinx 2.ses").toString()

o.spec("usworld", () => {

    o.spec("Tworld", () => {

        o("new", () => {
            const world = new TWorld()
            o(world).notEquals(null)
        })

        o("load world with missing contents", () => {
            const world = new TWorld()
            world.reportModeCallback = function() {}
            try {
                const loaded = world.loadWorldFromFileContents("")
                o(loaded).equals(true)
            } catch (e) {
                o(e.message).equals("Unexpected EOF loading file")
            }
        })

        o("load world with bad contents", () => {
            const world = new TWorld()
            world.reportModeCallback = function() {}
            try {
                const loaded = world.loadWorldFromFileContents("Not a robot")
                o(loaded).equals(true)
            } catch (e) {
                o(e.message).equals("File header for world file is not correct")
            }
        })

        o("load world for GarTrek", () => {
            const world = new TWorld()
            world.reportModeCallback = function() {}
            const loaded = world.loadWorldFromFileContents(GarTrekWorldContents)
            o(loaded).equals(true)
        })

        o("load session for GarTrek", () => {
            const world = new TWorld()
            world.reportModeCallback = function() {}
            const loadedWorld = world.loadWorldFromFileContents(GarTrekWorldContents)
            o(loadedWorld).equals(true)
            o(world.focus).equals(null)
            const loadedSession = world.loadSessionFromFile("GarTrek.wld", GarTrekSessionContents)
            o(loadedSession).equals(true)
            o(world.focus.phrase).equals("<sphinx>")
        })

        o("saying phrase in GarTrek", () => {
            // can't easily do this without commands
            const world = new TWorld()
            world.reportModeCallback = function() {}
            world.loadWorldFromFileContents(GarTrekWorldContents)
            world.loadSessionFromFile("GarTrek.wld", GarTrekSessionContents)
            o(world.focus.phrase).equals("<sphinx>")
            // Manually activate a rule -- normally a command would do this
            const rule = world.rules.find(rule => rule.command.phrase === "$answer herring")
            const result = rule.recordReplyMoveChanges([], "")
            o(result.contextToFocusTo.phrase).equals("nether regions")
            o(result.totalReply.startsWith("The Sphinx says: ")).equals(true)
        })  
    })
})
