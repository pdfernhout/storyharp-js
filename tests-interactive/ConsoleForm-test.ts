import * as m from "mithril"
import { TSApplication } from "../source/TSDomain"
import { MainForm } from "../source/MainForm"

const domain = new TSApplication()
domain.dataPath = "../data/"

const BodyComponent = { view: () => m(MainForm, <any>{domain: domain}) }

domain.loadWorldFromServerData("House and Yard").then(() => {
    m.mount(document.body, BodyComponent)
})

// Add resize listener so canvas will get updated in RuleMapView
window.addEventListener("resize", () => m.redraw())
