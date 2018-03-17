
// Exported from: ../../storyharp-js/converted_source/uscontextwizard.lfm
import * as m from "mithril"
import { TSApplication } from "./TSDomain"
import { MainForm } from "./MainForm"
import { addExtraStylesBeyondTachyons } from "./extraStyles";

addExtraStylesBeyondTachyons()

const application = new TSApplication()

const BodyComponent = { view: () => m(MainForm, <any>{domain: application}) }

// const worldName = "GarTrek"
const worldName = "House and Yard"
application.loadWorldFromServerData(worldName).then(() => {
    application.updateForNewOrLoadedWorld(worldName, true)
    m.mount(document.body, BodyComponent)
})

// Add resize listener so canvas will get updated in RuleMapView
window.addEventListener("resize", () => m.redraw())

window.onbeforeunload = function() { 
    if (application.isWorldFileChanged()) {
        // Browsers may not use this exact string, but something has to be returned
        return "You have made changes to the world file that are not yet saved. If you navigate away from this page you will lose your unsaved changes."
    }
}
