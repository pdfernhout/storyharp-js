import { TPoint } from "./TPoint"
import { TRect } from "./TRect"
import { StrToInt } from "./common"

export class TSDraggableObject {
    position: TPoint = new TPoint()
    extent: TPoint = new TPoint()
    selected: boolean = false
    
    displayName(): string {
        return "Error - override needed"
    }
    
    setPosition(value: string): void {
        const [firstNumber, secondNumber] = value.split(",")
        
        try {
            this.position.X = StrToInt(firstNumber)
            this.position.Y = StrToInt(secondNumber)
        } catch (e) {
            console.log("setPosition exception", e)
        }
    }
    
    bounds(): TRect {
        const topLeft = new TPoint(this.position.X - this.extent.X / 2, this.position.Y - this.extent.Y / 2)
        return new TRect(topLeft.X, topLeft.Y, topLeft.X + this.extent.X, topLeft.Y + this.extent.Y)
    }
    
    center(): TPoint {
        return this.position.copy()
    }
    
}
