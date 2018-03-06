import * as m from "mithril"
import { TSApplication } from "../source/TSDomain"
import { viewConsoleForm } from "../source/ConsoleForm"

const MyComponent = { view: () => viewConsoleForm(domain) }

const domain = new TSApplication()
domain.dataPath = "../data/"

domain.loadTestWorld("House and Yard").then(() => {
    m.mount(document.body, MyComponent)
})

// Add resize listener so canvas will get updated in RuleMapView
window.addEventListener("resize", () => m.redraw())
