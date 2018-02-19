import "../node_modules/mithril/mithril.js"
/* global m */

import { KfCommand } from "../dist/KfCommand.js"
import { KfCommandList } from "../dist/KfCommandList.js"

m.render(document.body, "Hello world")

console.log("KfCommandList", KfCommandList)

console.log("KfCommand", KfCommand)
