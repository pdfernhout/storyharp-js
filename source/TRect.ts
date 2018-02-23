import { int } from "./common"

export class TRect {
    Left: int
    Top: int
    Right: int
    Bottom: int

    constructor(Left = 0, Top = 0, Right = Left, Bottom = Top) {
        this.Left = Math.round(Left)
        this.Top = Math.round(Top)
        this.Right = Math.round(Right)
        this.Bottom = Math.round(Bottom)
    }

    /*
    width(): int {
        return this.Right - this.Left
    }

    height(): int {
        return this.Bottom - this.Top()
    }
    */

    intersects(otherRect: TRect): boolean {
        // Wondering if this should use <= ?
        return this.Left < otherRect.Right 
            && this.Right > otherRect.Left 
            && this.Top < otherRect.Bottom
            && this.Bottom > otherRect.Top
    }
}
