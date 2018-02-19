import "../node_modules/mithril/mithril.js"
/* global m */

import { KfCommand } from "../dist/KfCommand.js"
import { KfCommandList } from "../dist/KfCommandList.js"

const commandList = new KfCommandList()

const items = []

class CommandAddItem extends KfCommand {
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
    return m("div", "KfCommandList test", 
        m("div.ba", items.map((item) => m("div", "Item: " + item))),
        m("button.ml2", { onclick: () => {
            const command = new CommandAddItem()
            commandList.doCommand(command)
        } }, "Add Item"),
        m("button.ml2", {
            disabled: !commandList.isUndoEnabled(),
            onclick: () => commandList.undoLast()
        }, "Undo " + commandList.undoDescription()),
        m("button.ml2", { 
            disabled: !commandList.isRedoEnabled(),
            onclick: () => commandList.redoLast()
        }, "Redo " + commandList.redoDescription())
    )
}

m.mount(document.body, { view: viewMain })
