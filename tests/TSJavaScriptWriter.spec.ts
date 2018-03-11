import * as fs from "fs"
import * as o from "ospec"
import { TWorld } from "../source/TWorld"
import { TSJavaScriptWriter } from "../source/TSJavaScriptWriter"

const Tiny1WorldContents = fs.readFileSync("../data/tiny1.wld").toString().replace(/\r/g, "")

o.spec("TSJavaScriptWriter", () => {

    o("generate code for tiny1", () => {
        const world = new TWorld()
        const loaded = world.loadWorldFromFileContents(Tiny1WorldContents)
        o(loaded).equals(true)
        const writer = new TSJavaScriptWriter()
        const program = writer.writeJavaScriptProgram(world)
        console.log(program)
        o(program.length > 0).equals(true)
    })

})
