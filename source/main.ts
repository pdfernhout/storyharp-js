
// Exported from: ../../storyharp-js/converted_source/uscontextwizard.lfm
import * as m from "mithril"
import { TSApplication } from "./TSDomain"
import { viewConsoleForm } from "./ConsoleForm"
import { addExtraStylesBeyondTachyons } from "./extraStyles";

addExtraStylesBeyondTachyons()

const application = new TSApplication()

const MyComponent = { view: () => viewConsoleForm(application) }

// loadTestWorld("GarTrek")
application.loadWorldFromServerData("House and Yard").then(() => {
    m.mount(document.body, MyComponent)
})

// Add resize listener so canvas will get updated in RuleMapView
window.addEventListener("resize", () => m.redraw())

window.onbeforeunload = function() { 
    if (application.isWorldFileChanged()) {
        // Browsers may not use this exact string, but something has to be returned
        return "You have made changes to the world file that are not yet saved. If you navigate away from this page you will lose your unsaved changes."
    }
}
