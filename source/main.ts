
// Exported from: ../../storyharp-js/converted_source/uscontextwizard.lfm
import * as m from "mithril"
import { TSApplication } from "./TSDomain"
import { viewConsoleForm } from "./ConsoleForm"

const application = new TSApplication()

const MyComponent = { view: () => viewConsoleForm(application) }

// loadTestWorld("GarTrek")
application.loadTestWorld("House and Yard").then(() => {
    m.mount(document.body, MyComponent)
})

// Add resize listener so canvas will get updated in RuleMapView
window.addEventListener("resize", () => m.redraw())
