const o = require("ospec")
const fs = require("fs")

const usworld = require("../dist-test/usworld")

const GarTrekWorldContents = fs.readFileSync("data/GarTrek.wld").toString()
const GarTrekSessionContents = fs.readFileSync("data/just at sphinx 2.ses").toString()

o.spec("usworld", () => {

    o.spec("Tworld", () => {

        o("new", () => {
            const world = new usworld.TWorld()
            o(world).notEquals(null)
        })

        o("load world with missing contents", () => {
            const world = new usworld.TWorld()
            world.reportModeCallback = function() {}
            try {
                const loaded = world.loadWorldFromFileContents("")
                o(loaded).equals(true)
            } catch (e) {
                o(e.message).equals("Unexpected EOF loading file")
            }
        })

        o("load world with bad contents", () => {
            const world = new usworld.TWorld()
            world.reportModeCallback = function() {}
            try {
                const loaded = world.loadWorldFromFileContents("Not a robot")
                o(loaded).equals(true)
            } catch (e) {
                o(e.message).equals("File header for world file is not correct")
            }
        })

        o("load world for GarTrek", () => {
            const world = new usworld.TWorld()
            world.reportModeCallback = function() {}
            const loaded = world.loadWorldFromFileContents(GarTrekWorldContents)
            o(loaded).equals(true)
        })

        o("load session for GarTrek", () => {
            const world = new usworld.TWorld()
            world.reportModeCallback = function() {}
            const loadedWorld = world.loadWorldFromFileContents(GarTrekWorldContents)
            o(loadedWorld).equals(true)
            o(world.focus).equals(null)
            const loadedSession = world.loadSessionFromFile("GarTrek.wld", GarTrekSessionContents)
            o(loadedSession).equals(true)
            o(world.focus.phrase).equals("<sphinx>")
        })  
    })
})
