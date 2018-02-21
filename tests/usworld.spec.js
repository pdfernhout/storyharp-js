const o = require("ospec")

const usworld = require("../dist-test/usworld")

o.spec("usworld", () => {

    o("new", () => {
        const world = new usworld.TWorld()
        o(world).notEquals(null)
    })
})
