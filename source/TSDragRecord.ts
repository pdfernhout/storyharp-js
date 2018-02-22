import { TPoint } from "./TPoint.js"
import { TSDraggableObject } from "./TSDraggableObject.js"

export class TSDragRecord {
    draggedNode: TSDraggableObject;
    originalLocation: TPoint;
    newLocation: TPoint;
    
    constructor(node: TSDraggableObject) {
        this.draggedNode = node
        this.originalLocation = this.draggedNode.position.copy()
        this.newLocation = this.originalLocation.copy()
    }
    
    doDrag(): void {
        this.draggedNode.position = this.newLocation.copy()
    }
    
    undoDrag(): void {
        this.draggedNode.position = this.originalLocation.copy()
    }
    
    offset(delta: TPoint): void {
        this.newLocation = new TPoint(this.newLocation.X + delta.X, this.newLocation.Y + delta.Y)
        this.draggedNode.position = this.newLocation.copy()
    }   
}
