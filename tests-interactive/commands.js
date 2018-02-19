import "../node_modules/mithril/mithril.js"
/* global m */

import { KfCommand } from "../dist/KfCommand.js"
import { KfCommandList } from "../dist/KfCommandList.js"

function viewMain() {
    return m("div", "HELLO WORLD 3", 
        m("button.ml2", { onclick: () => alert("hello") }, "Click me")
    )
}

m.mount(document.body, { view: viewMain })
