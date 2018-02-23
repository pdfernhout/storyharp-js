import * as o from "ospec"

import { KfCommand } from "../source/KfCommand.js"

o.spec("KfCommand", () => {

    o("new", () => {
        const command = new KfCommand()
        o(command).notEquals(null)
    })

    o("do and redo", () => {
        const command = new KfCommand()
        o(command).notEquals(null)
        o(command.done).equals(false)

        command.doCommand()
        o(command.done).equals(true)

        command.undoCommand()
        o(command.done).equals(false)

        command.redoCommand()
        o(command.done).equals(true)

        command.undoCommand()
        o(command.done).equals(false)
    })

})

