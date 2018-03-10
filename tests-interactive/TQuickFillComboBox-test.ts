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
]

let editableValue: "hello"

const MyComponent = { 
    view: () => m("div.ma3", 
        "This should be a TQuickFillComboBox:",
        m(TQuickFillComboBox, <any>{
            items: ["a", "b", "c", "sassafras", "a very long choice that is impactical",  "a very long choice that is really impactical and goes on and on and on and on and on and on and on and on", "d"]
        }),

        m("div.ma3", "This is a more advanced test:"),
        m(TQuickFillComboBox, <any>{
            value: editableValue,
            items: editableList,
            onchange: (event: any) => {
                console.log("onchange", event)
                editableValue = event.target.value
            },
        }),
        m("div.ma2", "The combobox value is: ", editableValue),
        "Which can also be changed here:",
        m("input", {
            value: editableValue,
            onchange: (event: any) => { editableValue = event.target.value },
        })
    )
}

m.mount(document.body, MyComponent)
