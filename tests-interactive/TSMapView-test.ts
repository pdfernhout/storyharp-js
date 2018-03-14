import * as m from "mithril"
import { TSMapView, newMapViewState } from "../source/TSMapView"
import { TPoint } from "../source/TPoint"
import { TRect } from "../source/TRect"
import { TSRuleField } from "../source/TSRule"
import { TSApplication } from "../source/TSDomain"

const mapDrawer = new TSMapView(newMapViewState())

const MyComponent = { 
    view: () => m("div", 
        m("canvas.ba", {
            onupdate(vnode: m.VnodeDOM) {
                const canvas = <HTMLCanvasElement>vnode.dom

                // Clear the canvas
                canvas.width = canvas.width

                const context = canvas.getContext("2d")
                if (!context) return
                context.beginPath()
                context.moveTo(0, 0)
                context.lineTo(100, 100)
                context.closePath()
                context.strokeStyle = "#00FFFF"
                context.stroke()

                mapDrawer.drawArrowhead(context, new TPoint(100, 40), new TPoint(120, 40))

                context.strokeStyle = "#00FF00"
                const r1 = new TRect(10, 110, 30, 130)
                const r2 = new TRect(70, 120, 90, 140)
                const r3 = new TRect(170, 20, 190, 40)
                mapDrawer.drawRect(context, r1)
                mapDrawer.drawRect(context, r2)
                mapDrawer.drawRect(context, r3)

                mapDrawer.drawArrowFromRectEdgeToRectEdge(context, r1, r2)
                mapDrawer.drawArrowFromRectEdgeToRectEdge(context, r2, r3)
                mapDrawer.drawArrowFromRectEdgeToRectEdge(context, r3, r1)

                mapDrawer.drawCommandOrContext(context, "Hello, world!", r3, false, false, false)
            },
            onclick: () => mapDrawer.scroll.X -= 20,
        }),
        m("div", "There should be a canvas above with a line from top left most of the way towards the bottom middle."),
        m("canvas.ba.ma4", {
            width: 500,
            height: 500,
            onupdate(vnode: m.VnodeDOM) {
                const canvas = <HTMLCanvasElement>vnode.dom

                // Clear the canvas
                canvas.width = canvas.width

                const context = canvas.getContext("2d")
                if (!context) return
                const displayOptions = []
                displayOptions[TSRuleField.kRuleContext] = true
                displayOptions[TSRuleField.kRuleCommand] = true
                mapDrawer.displayOn(context, displayOptions, null, null, domain.world, null, domain.showCommandPrefixInMap)
            },
            onclick: () => mapDrawer.scroll.X += 20,
        }),
    )
}

const domain = new TSApplication()
domain.dataPath = "../data/"

domain.loadWorldFromServerData("House and Yard").then(() => {
    m.mount(document.body, MyComponent)
    // Force a first update
    m.redraw()
})
