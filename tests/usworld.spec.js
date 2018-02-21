const o = require("ospec")
const fs = require("fs")

const usworld = require("../dist-test/usworld")

o.spec("usworld", () => {

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
        const contents = fs.readFileSync("data/GarTrek.wld").toString()
        const loaded = world.loadWorldFromFileContents(contents)
        o(loaded).equals(true)
    })
})
