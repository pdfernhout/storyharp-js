import * as m from "mithril"
import { TQuickFillComboBox } from "../source/TQuickFillComboBox"

const editableList = [
    "first",
    "second",
    "third",
    "fourth",
    "fifth",
    "sixth",
    "sassafras",
    "sarsaparilla",
    "more",
    "a very long choice that is impractical",
    "a very long choice that is really impractical and goes on and on and on and on and on and on and on and on",
]

let editableValue = "hello"
let otherValue = ""

const MyComponent = { 
    view: () => m("div.ma3", 
        m("div", "Combobox test:"),
        m(TQuickFillComboBox, <any>{
            value: editableValue,
            items: editableList,
            onchange: (event: any) => {
                console.log("onchange from combobox", event)
                editableValue = event.target.value
            },
        }),
        m("div.mt7", "The combobox value is: \"" + editableValue + "\"", ),
        m("div", "Which can also be changed here:"),
        m("input", {
            value: editableValue,
            onchange: (event: any) => { editableValue = event.target.value },
        }),
        m("div.mt3", "Unrelated input for focus shift testing:"),
        m("input", {
            value: otherValue,
            onchange: (event: any) => { otherValue = event.target.value },
        })
    )
}

m.mount(document.body, MyComponent)
