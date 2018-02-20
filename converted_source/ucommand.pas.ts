// unit Ucommand

from conversion_common import *
import delphi_compatability

const Ucommand = Ucommand || {}

export enum TrackPhase { trackPress, trackMove, trackRelease }

export enum KfCommandChangeType { commandDone, commandUndone }

// const
const kMinMouseMoveDistance = 2



export class KfCommand {
    notifyProcedure: TCommandEvent = new TCommandEvent()
    canUndo: boolean = false
    done: boolean = false
    commandChangesFile: boolean = false
    
    //KfCommand
    create(): void {
        TObject.prototype.create.call(this)
        this.canUndo = true
        this.done = false
        // default commandChangesPlantFile to true, since most commands change file,
        //    if command does not change file, set to false after call to inherited create 
        this.commandChangesFile = true
    }
    
    destroy(): void {
        //sublass could override
        TObject.prototype.destroy.call(this)
    }
    
    doCommand(): void {
        this.done = true
        //subclass should override and call inherited
    }
    
    undoCommand(): void {
        this.done = false
        //sublass should override and call inherited
    }
    
    redoCommand(): void {
        this.doCommand()
        //sublass may override and call inherited doCommand
        //could call inherited redo, but then watch out that do will be done too!
    }
    
    description(): string {
        let result = ""
        result = "*command description*"
        return result
    }
    
    TrackMouse(aTrackPhase: TrackPhase, anchorPoint: TPoint, previousPoint: TPoint, nextPoint: TPoint, mouseDidMove: boolean, rightButtonDown: boolean): KfCommand {
        let result = new KfCommand()
        //sublasses should override if needed
        result = this
        return result
    }
    
    // notify cannot be done within the do, undo, redo of command because
    // command list will not have finished updating itself
    doNotify(): void {
        if (delphi_compatability.Assigned(this.notifyProcedure)) {
            if (this.done) {
                // commandChangesFile and
                this.notifyProcedure(this, KfCommandChangeType.commandDone)
            } else {
                this.notifyProcedure(this, KfCommandChangeType.commandUndone)
            }
        }
    }
    
}

export class KfCommandList {
    notifyProcedure: TCommandEvent = new TCommandEvent()
    commandList: TList = new TList()
    lastDoneCommandIndex: long = 0
    undoLimit: long = 0
    mouseCommand: KfCommand = new KfCommand()
    anchorPoint: TPoint = new TPoint()
    previousPoint: TPoint = new TPoint()
    rightButtonDown: boolean = false
    
    //KfCommandList
    create(): void {
        TObject.prototype.create.call(this)
        this.commandList = delphi_compatability.TList().Create()
        this.lastDoneCommandIndex = -1
        this.undoLimit = 100
    }
    
    clear(): void {
        let i: int
        
        if (this.commandList.Count > 0) {
            for (i = 0; i <= this.commandList.Count - 1; i++) {
                UNRESOLVED.TObject(this.commandList[i]).free
            }
        }
        this.commandList.Clear()
        this.lastDoneCommandIndex = -1
    }
    
    destroy(): void {
        this.clear()
        this.commandList.free
        //if mouseCommand <> nil then error condition - ignoring for now - not released
        //could only happend if quitting somehow in middle of action
        TObject.prototype.destroy.call(this)
    }
    
    command(index: long): KfCommand {
        let result = new KfCommand()
        result = KfCommand(this.commandList.Items[index])
        return result
    }
    
    setNewUndoLimit(newLimit: long): void {
        this.undoLimit = newLimit
        this.freeCommandsAboveLimit(this.undoLimit)
    }
    
    //free any command more than the number passed in
    freeCommandsAboveLimit(theLimit: long): void {
        let theCommand: KfCommand
        
        while ((this.commandList.Count > theLimit) && (this.commandList.Count > 0)) {
            theCommand = this.command(0)
            this.commandList.Delete(0)
            theCommand.free
            this.lastDoneCommandIndex -= 1
            if (this.lastDoneCommandIndex < -1) {
                this.lastDoneCommandIndex = -1
            }
        }
    }
    
    doCommand(newCommand: KfCommand): void {
        //remove any extra commands after the current
        //do this first to free memory for command
        this.clearRedoableCommands()
        //see if too many commands are stored and if so, scroll them
        this.freeCommandsAboveLimit(this.undoLimit - 1)
        if (!delphi_compatability.Assigned(newCommand.notifyProcedure)) {
            //now do this command
            newCommand.notifyProcedure = this.notifyProcedure
        }
        //may fail in which case won't add
        newCommand.doCommand()
        this.lastDoneCommandIndex += 1
        this.commandList.Add(newCommand)
        newCommand.doNotify()
    }
    
    //added nextMouseCommand in these three functions to deal with unhandled exceptions occurring
    //during mouse commands.  This way, the command will not be further processed.
    //This may occasionally leak - the mouse command should be the one responsible for freeing
    //itself and returning nil if a problem occurs
    //returns whether the command finished tracking without freeing itself
    mouseDown(newCommand: KfCommand, point: Tpoint): boolean {
        let result = false
        let nextMouseCommand: KfCommand
        
        result = false
        if (this.mouseCommand !== null) {
            //check if need to clear mouse command
            this.mouseUp(Point)
        }
        this.mouseCommand = null
        if (newCommand !== null) {
            //save mouse command and start it
            this.anchorPoint = Point
            this.previousPoint = Point
            nextMouseCommand = newCommand
            if (!delphi_compatability.Assigned(newCommand.notifyProcedure)) {
                newCommand.notifyProcedure = this.notifyProcedure
            }
            this.mouseCommand = nextMouseCommand.TrackMouse(TrackPhase.trackPress, this.anchorPoint, this.previousPoint, Point, false, this.rightButtonDown)
            result = (this.mouseCommand !== null)
        }
        return result
    }
    
    mouseMove(point: TPoint): void {
        let mouseDidMove: boolean
        let nextMouseCommand: KfCommand
        let pointMovedTo: TPoint
        
        nextMouseCommand = this.mouseCommand
        this.mouseCommand = null
        if (nextMouseCommand !== null) {
            mouseDidMove = (abs(this.previousPoint.X - Point.x) > kMinMouseMoveDistance) || (abs(this.previousPoint.Y - Point.y) > kMinMouseMoveDistance)
            if (mouseDidMove) {
                pointMovedTo = Point
            } else {
                pointMovedTo = this.previousPoint
            }
            this.mouseCommand = nextMouseCommand.TrackMouse(TrackPhase.trackMove, this.anchorPoint, this.previousPoint, pointMovedTo, mouseDidMove, this.rightButtonDown)
        }
        this.previousPoint = pointMovedTo
    }
    
    mouseUp(point: TPoint): void {
        let mouseDidMove: boolean
        let nextMouseCommand: KfCommand
        let pointMovedTo: TPoint
        
        nextMouseCommand = this.mouseCommand
        this.mouseCommand = null
        if (nextMouseCommand !== null) {
            mouseDidMove = (abs(Point.x - this.anchorPoint.X) > kMinMouseMoveDistance) || (abs(Point.y - this.anchorPoint.Y) > kMinMouseMoveDistance)
            if (mouseDidMove) {
                pointMovedTo = Point
            } else {
                pointMovedTo = this.anchorPoint
            }
            nextMouseCommand = nextMouseCommand.TrackMouse(TrackPhase.trackRelease, this.anchorPoint, this.previousPoint, Point, mouseDidMove, this.rightButtonDown)
            if (nextMouseCommand !== null) {
                this.doCommand(nextMouseCommand)
            }
        }
    }
    
    isUndoEnabled(): boolean {
        let result = false
        result = this.lastDoneCommandIndex >= 0
        return result
    }
    
    isRedoEnabled(): boolean {
        let result = false
        result = this.lastDoneCommandIndex < (this.commandList.Count - 1)
        return result
    }
    
    undoDescription(): string {
        let result = ""
        if (this.lastDoneCommandIndex >= 0) {
            result = this.command(this.lastDoneCommandIndex).description()
        } else {
            result = ""
        }
        return result
    }
    
    redoDescription(): string {
        let result = ""
        if (this.lastDoneCommandIndex < (this.commandList.Count - 1)) {
            result = this.command(this.lastDoneCommandIndex + 1).description()
        } else {
            result = ""
        }
        return result
    }
    
    undoLast(): void {
        let aCommand: KfCommand
        
        if (this.lastDoneCommandIndex >= 0) {
            aCommand = this.command(this.lastDoneCommandIndex)
            aCommand.undoCommand()
            this.lastDoneCommandIndex -= 1
            aCommand.doNotify()
        }
    }
    
    redoLast(): void {
        let aCommand: KfCommand
        
        if (this.lastDoneCommandIndex < (this.commandList.Count - 1)) {
            aCommand = this.command(this.lastDoneCommandIndex + 1)
            aCommand.redoCommand()
            this.lastDoneCommandIndex += 1
            aCommand.doNotify()
        }
    }
    
    removeCommand(aCommand: KfCommand): void {
        if (aCommand.done) {
            // assume this command has been undone previously 
            throw new GeneralException.create("KfCommandList.removeCommand: command not undone")
        }
        this.commandList.Remove(aCommand)
    }
    
    lastCommand(): KfCommand {
        let result = new KfCommand()
        result = null
        if (this.lastDoneCommandIndex >= 0) {
            result = this.command(this.lastDoneCommandIndex)
        }
        return result
    }
    
    clearRedoableCommands(): void {
        let i: long
        let theCommand: KfCommand
        
        if (this.isRedoEnabled()) {
            for (i = this.commandList.Count - 1; i >= this.lastDoneCommandIndex + 1; i--) {
                theCommand = this.command(i)
                this.commandList.Delete(i)
                theCommand.free
            }
        }
    }
    
    //call if change last command
    lastCommandChanged(): void {
        this.clearRedoableCommands()
    }
    
}

