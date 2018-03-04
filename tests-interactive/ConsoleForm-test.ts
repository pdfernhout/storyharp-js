
// Exported from: ../../storyharp-js/converted_source/uscontextwizard.lfm
import * as m from "mithril"
import { FakeDomain } from "./FakeDomain"
import { viewConsoleForm } from "../source/ConsoleForm"

const MyComponent = { view: () => viewConsoleForm(FakeDomain) }

// loadTestWorld("GarTrek")
FakeDomain.loadTestWorld("House and Yard").then(() => {
    m.mount(document.body, MyComponent)
})

// Add resize listener so canvas will get updated in RuleMapView
window.addEventListener("resize", () => m.redraw())
