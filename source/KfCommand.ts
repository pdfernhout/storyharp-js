// An undoable and redoable command

import { TPoint } from "./common"

export enum TrackPhase { trackPress, trackMove, trackRelease }

export enum KfCommandChangeType { commandDone, commandUndone }

export type TCommandEvent = (command: KfCommand, state: KfCommandChangeType) => void

export class KfCommand {
    notifyProcedure: TCommandEvent
    canUndo: boolean
    done: boolean
    commandChangesFile: boolean

    constructor () {
        this.canUndo = true
        this.done = false

        // default commandChangesFile to true, since most commands change file,
        // if command does not change file, set to false after call to inherited create
        this.commandChangesFile = true
    }

    doCommand(): void {
        this.done = true
        // subclass should override and call inherited
    }

    undoCommand(): void {
        this.done = false
        // sublass should override and call inherited
    }

    redoCommand(): void {
        this.doCommand()
        // sublass may override and call inherited doCommand
        // could call inherited redo, but then watch out that do will be done too!
    }

    description(): string {
        return "*command description*"
    }

    trackMouse(
        aTrackPhase: TrackPhase,
        anchorPoint: TPoint, 
        previousPoint: TPoint, 
        nextPoint: TPoint,
        mouseDidMove: boolean,
        rightButtonDown: boolean
    ): KfCommand {
        // subclasses should override if needed
        return this
    }

    // notify cannot be done within the do, undo, redo of command because
    // command list will not have finished updating itself
    doNotify(): void {
        // commandChangesFile and
        if (this.notifyProcedure) {
            if (this.done) {
                this.notifyProcedure(this, KfCommandChangeType.commandDone)
            } else {
                this.notifyProcedure(this, KfCommandChangeType.commandUndone)
            }
        }
    }
}
