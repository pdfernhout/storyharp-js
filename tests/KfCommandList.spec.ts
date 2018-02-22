var o = require("ospec")

import { KfCommand, KfCommandChangeType } from "../source/KfCommand"
import { KfCommandList } from "../source/KfCommandList"

o.spec("KfCommandList", () => {

    o("new", () => {
        const commandList = new KfCommandList()
        o(commandList).notEquals(null)
    })

    o("do and redo", () => {
        const commandList = new KfCommandList()
        const command = new KfCommand()

        let commandChangeType = null
        
        command.notifyProcedure = (command, state) => {
            commandChangeType = state
        }
        
        o(command).notEquals(null)
        o(commandChangeType).equals(null)
        o(command.done).equals(false)

        commandList.doCommand(command)
        o(commandChangeType).equals(KfCommandChangeType.commandDone)
        o(command.done).equals(true)
        
        commandList.undoLast()
        o(commandChangeType).equals(KfCommandChangeType.commandUndone)
        o(command.done).equals(false)
        
        commandList.redoLast()
        o(commandChangeType).equals(KfCommandChangeType.commandDone)
        o(command.done).equals(true)
        
        commandList.undoLast()
        o(commandChangeType).equals(KfCommandChangeType.commandUndone)
        o(command.done).equals(false)
    })

})
