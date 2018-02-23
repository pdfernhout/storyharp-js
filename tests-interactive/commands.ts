import * as m from "mithril"

import { KfCommand } from "../source/KfCommand"
import { KfCommandList } from "../source/KfCommandList"

const commandList = new KfCommandList()
commandList.undoLimit = 10

const items: Date[] = []

class CommandAddItem extends KfCommand {
    date: Date

    constructor() {
        super()
        this.date = new Date()
    }

    description() {
        return "Add Date Item"
    }

    doCommand() {
        items.push(this.date)
        super.doCommand()
    }

    undoCommand() {
        items.pop()
        super.undoCommand()
    }

}

function viewMain() {
    return m("div.ma1",
        m("div.mb2", "KfCommandList test"),
        m("div.mb2", 
            m("button", { onclick: () => {
                const newValueString = prompt("New undo limit?", "" + commandList.undoLimit)
                if (!newValueString) return
                const newValue = parseInt(newValueString)
                commandList.setNewUndoLimit(newValue)
            }}, "Undo limit: " + commandList.undoLimit), 
            m("button.ml2", {
                onclick: () => commandList.clear()
            }, "Clear Undo List")   
        ),
        m("button.ml2", { onclick: () => {
            const command = new CommandAddItem()
            commandList.doCommand(command)
        } }, "Add Item"),
        m("button.ml2.w5", {
            disabled: !commandList.isUndoEnabled(),
            onclick: () => commandList.undoLast()
        }, "Undo " + commandList.undoDescription()),
        m("button.ml2.w5", { 
            disabled: !commandList.isRedoEnabled(),
            onclick: () => commandList.redoLast()
        }, "Redo " + commandList.redoDescription()),   
        m("div.ba.ma2", m("div", "ITEMS:"), items.map((item) => m("div", "Item: " + item)))
    )
}

m.mount(document.body, { view: viewMain })
