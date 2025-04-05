import { KfCommand, TCommandEvent, KfCommandChangeType, TrackPhase } from "./KfCommand"
import { TSDragRecord } from "./TSDragRecord"
import { TPoint } from "./TPoint"
import { TSDomain } from "./TSDomain"

export class TSMapDragCommand extends KfCommand {
    domain: TSDomain
    scale: number
    dragRecords: TSDragRecord[] = []
    
    constructor(domain: TSDomain, scale: number) {
        super()
        this.domain = domain
        this.scale = scale
        this.domain.world.addDragRecordsToList(this.dragRecords)
    }
    
    doCommand(): void {
        for (let i = 0; i < this.dragRecords.length; i++) {
            this.dragRecords[i].doDrag()
        }
        super.doCommand()
    }
    
    undoCommand(): void {
        this.domain.world.deselectAllExcept(null)
        for (let i = 0; i < this.dragRecords.length; i++) {
            this.dragRecords[i].draggedNode.selected = true
            this.dragRecords[i].undoDrag()
        }
        super.undoCommand()
    }
    
    redoCommand(): void {
        this.domain.world.deselectAllExcept(null)
        for (let i = 0; i < this.dragRecords.length; i++) {
            this.dragRecords[i].draggedNode.selected = true
            this.dragRecords[i].doDrag()
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
                        this.dragRecords[i].offset(delta.scale(this.scale))
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
                            this.dragRecords[i].offset(delta.scale(this.scale))
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
