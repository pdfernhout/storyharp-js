import { KfCommand, TCommandEvent, KfCommandChangeType, TrackPhase } from "./KfCommand"
import { TWorld } from "./TWorld"
import { TSDragRecord } from "./TSDragRecord"
import { TPoint } from "./TPoint"

export class TSMapDragCommand extends KfCommand {
    world: TWorld
    dragRecords: TSDragRecord[] = []
    notifyProcedure: TCommandEvent
    
    constructor(world: TWorld) {
        super()
        this.world = world
        this.world.addDragRecordsToList(this.dragRecords)
    }
    
    doCommand(): void {
        for (let i = 0; i < this.dragRecords.length; i++) {
            this.dragRecords[i].doDrag()
        }
        if (this.notifyProcedure) {
            this.notifyProcedure(this, KfCommandChangeType.commandDone)
        }
        super.doCommand()
    }
    
    undoCommand(): void {
        this.world.deselectAllExcept(null)
        for (let i = 0; i < this.dragRecords.length; i++) {
            this.dragRecords[i].draggedNode.selected = true
            this.dragRecords[i].undoDrag()
        }
        if (this.notifyProcedure) {
            this.notifyProcedure(this, KfCommandChangeType.commandUndone)
        }
        super.undoCommand()
    }
    
    redoCommand(): void {
        this.world.deselectAllExcept(null)
        for (let i = 0; i < this.dragRecords.length; i++) {
            this.dragRecords[i].draggedNode.selected = true
            this.dragRecords[i].doDrag()
        }
        if (this.notifyProcedure) {
            this.notifyProcedure(this, KfCommandChangeType.commandDone)
        }
        super.doCommand()
    }
    
    description(): string {
        let result = ""
        if (this.dragRecords.length > 1) {
            result = "Drag nodes"
        } else if (this.dragRecords.length === 1) {
            result = "Drag " + this.dragRecords[0].draggedNode.displayName()
        } else {
            result = "Drag"
        }
        return result
    }
    
    trackMouse(aTrackPhase: TrackPhase, anchorPoint: TPoint, previousPoint: TPoint, nextPoint: TPoint, mouseDidMove: boolean, rightButtonDown: boolean): KfCommand | null {
        let result: KfCommand | null

        result = this
        switch (aTrackPhase) {
            case TrackPhase.trackPress:
                if (this.dragRecords.length === 0) {
                    result = null
                }
                break
            case TrackPhase.trackMove:
                if (mouseDidMove) {
                    const delta = new TPoint(nextPoint.X - previousPoint.X, nextPoint.Y - previousPoint.Y)
                    for (let i = 0; i < this.dragRecords.length; i++) {
                        this.dragRecords[i].offset(delta)
                    }
                    if (this.notifyProcedure) {
                        this.notifyProcedure(this, KfCommandChangeType.commandDone)
                    }
                }
                break
            case TrackPhase.trackRelease:
                if (!mouseDidMove) {
                    if ((this.dragRecords[0].draggedNode.position.X !== this.dragRecords[0].originalLocation.X) || (this.dragRecords[0].draggedNode.position.Y !== this.dragRecords[0].originalLocation.Y)) {
                        for (let i = 0; i < this.dragRecords.length; i++) {
                            this.dragRecords[i].undoDrag()
                        }
                        if (this.notifyProcedure) {
                            this.notifyProcedure(this, KfCommandChangeType.commandDone)
                        }
                    }
                    result = null
                } else {
                    const delta = new TPoint(nextPoint.X - previousPoint.X, nextPoint.Y - previousPoint.Y)
                    if ((delta.X !== 0) || (delta.Y !== 0)) {
                        for (let i = 0; i < this.dragRecords.length; i++) {
                            this.dragRecords[i].offset(delta)
                        }
                        if (this.notifyProcedure) {
                            this.notifyProcedure(this, KfCommandChangeType.commandDone)
                        }
                    }
                }
                break
        }
        return result
    }
    
}
