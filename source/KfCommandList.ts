import { KfCommand, TCommandEvent, TrackPhase } from "./KfCommand"

import { arrayRemove } from "./common"
import { TPoint } from "./TPoint"

type longint = number
type integer = number

const kMinMouseMoveDistance = 2;

export class KfCommandList {
    notifyProcedure: TCommandEvent
	commandList: KfCommand[]
	lastDoneCommandIndex: longint
    undoLimit: longint
    mouseCommand: KfCommand | null = null
    anchorPoint: TPoint
    previousPoint: TPoint
    rightButtonDown: boolean;

    constructor () {
        this.commandList = []
        this.lastDoneCommandIndex = -1
        this.undoLimit = 100
    }

    clear(): void {
        this.commandList = []
        this.lastDoneCommandIndex = -1
    }

    command(index: longint): KfCommand {
        return this.commandList[index]
    }

    setNewUndoLimit(newLimit: longint): void {
        this.undoLimit = newLimit
        this.freeCommandsAboveLimit(this.undoLimit)
    }

    // free any command more than the number passed in
    freeCommandsAboveLimit(theLimit: longint): void {
        while ((this.commandList.length > theLimit) && (this.commandList.length > 0)) {
            this.commandList.pop()
            this.lastDoneCommandIndex--
            if (this.lastDoneCommandIndex < -1) this.lastDoneCommandIndex = -1
        }
    }

    doCommand(newCommand: KfCommand): void {
        // remove any extra commands after the current
        // do this first to free memory for command
        this.clearRedoableCommands()
        // see if too many commands are stored and if so, scroll them
        this.freeCommandsAboveLimit(this.undoLimit - 1)
        // now do this command
        if (!newCommand.notifyProcedure && this.notifyProcedure) {
            newCommand.notifyProcedure = this.notifyProcedure.bind(this)
        }
        newCommand.doCommand() // may fail in which case won't add
        this.lastDoneCommandIndex++
        this.commandList.push(newCommand)
        newCommand.doNotify()
    }

    /* added nextMouseCommand in these three functions to deal with unhandled exceptions occurring
    during mouse commands.  This way, the command will not be further processed.
    This may occasionally leak - the mouse command should be the one responsible for freeing
    itself and returning null if a problem occurs */

    // returns whether the command finished tracking without freeing itself
    mouseDown(newCommand: KfCommand, point: TPoint): boolean {
        let nextMouseCommand: KfCommand
        let result = false
        // check if need to clear mouse command
        if (this.mouseCommand !== null) {
            this.mouseUp(point)
        }
        this.mouseCommand = null
        // save mouse command and start it
        if (newCommand !== null) {
            this.anchorPoint = point
            this.previousPoint = point
            nextMouseCommand = newCommand
            if (!newCommand.notifyProcedure && this.notifyProcedure) {
                newCommand.notifyProcedure = this.notifyProcedure.bind(this)
            }
            this.mouseCommand = nextMouseCommand.trackMouse(TrackPhase.trackPress, this.anchorPoint, this.previousPoint, point, false, this.rightButtonDown)
            result = (this.mouseCommand !== null)
            }
        return result
    }

    mouseMove(point: TPoint): void {
        let mouseDidMove: boolean
        let nextMouseCommand: KfCommand | null
        let pointMovedTo: TPoint = new TPoint(0, 0)

        nextMouseCommand = this.mouseCommand
        this.mouseCommand = null
        if (nextMouseCommand !== null) {
            mouseDidMove = 
                (Math.abs(this.previousPoint.X - point.X) > kMinMouseMoveDistance) ||
                (Math.abs(this.previousPoint.Y - point.Y) > kMinMouseMoveDistance)
            if (mouseDidMove) {
                pointMovedTo = point
            } else {
                pointMovedTo = this.previousPoint
            }
            this.mouseCommand = nextMouseCommand.trackMouse(TrackPhase.trackMove, this.anchorPoint, this.previousPoint, pointMovedTo, mouseDidMove, this.rightButtonDown)
        }
        this.previousPoint = pointMovedTo
    }

    mouseUp(point: TPoint): void {
        let mouseDidMove: boolean
        let nextMouseCommand: KfCommand | null
        let pointMovedTo: TPoint

        nextMouseCommand = this.mouseCommand
        this.mouseCommand = null
        if (nextMouseCommand !== null) {
            mouseDidMove = 
                (Math.abs(point.X - this.anchorPoint.X) > kMinMouseMoveDistance) ||
                (Math.abs(point.Y - this.anchorPoint.Y) > kMinMouseMoveDistance)
            if (mouseDidMove) {
                pointMovedTo = point
            } else {
                pointMovedTo = this.anchorPoint
            }
            nextMouseCommand = nextMouseCommand.trackMouse(TrackPhase.trackRelease, this.anchorPoint, this.previousPoint, point, mouseDidMove, this.rightButtonDown)
            if (nextMouseCommand !== null) {
                this.doCommand(nextMouseCommand)
            }
        }
    }

    isUndoEnabled(): boolean {
        return this.lastDoneCommandIndex >= 0
    }

    isRedoEnabled(): boolean {
        return this.lastDoneCommandIndex < (this.commandList.length - 1)
    }

    undoDescription(): string {
        if (this.lastDoneCommandIndex >= 0) {
            return this.command(this.lastDoneCommandIndex).description()
        } else {
            return ""
        }
    }

    redoDescription(): string {
        if (this.lastDoneCommandIndex < (this.commandList.length - 1)) {
            return this.command(this.lastDoneCommandIndex + 1).description()
        } else {
            return ""
        }
    }

    undoLast(): void {
        let aCommand: KfCommand
        if (this.lastDoneCommandIndex >= 0) {
            aCommand = this.command(this.lastDoneCommandIndex)
            aCommand.undoCommand()
            this.lastDoneCommandIndex--
            aCommand.doNotify()
        }
    }

    redoLast(): void {
        let aCommand: KfCommand
        if (this.lastDoneCommandIndex < (this.commandList.length - 1)) {
            aCommand = this.command(this.lastDoneCommandIndex + 1)
            aCommand.redoCommand()
            this.lastDoneCommandIndex++
            aCommand.doNotify()
        }
    }

    removeCommand(aCommand: KfCommand) {
        //  assume this command has been undone previously 
        if (aCommand.done) {
            throw new Error("KfCommandList.removeCommand: command not undone")
        }
        arrayRemove(this.commandList, aCommand)
    }

    lastCommand(): KfCommand | null {
        if (this.lastDoneCommandIndex >= 0) {
            return this.command(this.lastDoneCommandIndex)
        }
        return null
    }

    clearRedoableCommands(): void {
        if (this.isRedoEnabled)
            for (let i: longint = this.commandList.length - 1; i > this.lastDoneCommandIndex; i--) {
                this.commandList.pop()
            }
        }

    // call if change last command
    lastCommandChanged(): void {
        this.clearRedoableCommands()
    }

}
