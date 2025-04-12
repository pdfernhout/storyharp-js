
// Exported from: ../../storyharp-js/converted_source/uscontextwizard.lfm
import * as m from "mithril"
import { TSApplication } from "./TSDomain"
import { MainForm } from "./MainForm"
import { addExtraStylesBeyondTachyons } from "./extraStyles";

addExtraStylesBeyondTachyons()

const application = new TSApplication()

const BodyComponent = { view: () => m(MainForm, <any>{domain: application}) }

// const worldName = "GarTrek"
// const worldName = "House and Yard"

async function loadWorld() {
    const hash = window.location.hash
    if (hash && hash.length > 1 && hash.startsWith("#world=")) {
        const worldCompressed = hash.substring("#world=".length)
        const worldString = application.decompresURLDataForWorld(worldCompressed)
        let world = {
            worldFileName: "Untitled",
            worldContent: "",
        }
        if (worldString === null) {
            console.error("Could not decompress world data from URL")
            alert("Could not decompress world data from URL. Using empty world.")
            window.location.hash = ""
        } else {
            try {
                world = JSON.parse(worldString)
                application.world.resetVariablesAndRules()
                const loaded = application.world.loadWorldFromFileContents(world.worldContent)
                if (!loaded) {
                    console.error("Could not load world data from URL")
                    application.world.resetVariablesAndRules()
                    alert("Could not load world data from URL. Using empty world.")
                    window.location.hash = ""
                }
                application.updateForNewOrLoadedWorld(world?.worldFileName || "Untitled", true)
            } catch (e) {
                console.error("Could not parse world data from URL")
                alert("Could not parse world data from URL. Using empty world.")
                application.updateForNewOrLoadedWorld(world?.worldFileName || "Untitled", true)
                window.location.hash = ""
            }
        }
        await application.loadDemoConfig()
        m.mount(document.body, BodyComponent)
    } else {
        await application.loadDemoConfig()
        // Load the world from the server
        const worldName = "House and Yard with media"
        await application.loadWorldFromServerData(worldName)
        m.mount(document.body, BodyComponent)
    }
}

loadWorld()

// Add resize listener so canvas will get updated in RuleMapView
window.addEventListener("resize", () => m.redraw())

window.onbeforeunload = function() { 
    if (application.isWorldFileChanged()) {
        // Browsers may not use this exact string, but something has to be returned
        return "You have made changes to the world file that are not yet saved. If you navigate away from this page you will lose your unsaved changes."
    } else {
        return undefined
    }
}
