import * as m from "mithril"
import { TQuickFillComboBox } from "../source/TQuickFillComboBox"

const MyComponent = { 
    view: () => m("div", 
        "This should be a TQuickFillComboBox:",
        m(TQuickFillComboBox, <any>{
            items: ["a", "b", "c", "sassafras", "a very long choice that is impactical",  "a very long choice that is really impactical and goes on and on and on and on and on and on and on and on", "d"]
        })
    )
}

m.mount(document.body, MyComponent)
