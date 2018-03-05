import { int } from "./common"
import { TPoint } from "./TPoint"

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

    get width(): int {
        return this.Right - this.Left
    }

    get height(): int {
        return this.Bottom - this.Top
    }

    center(): TPoint {
        return new TPoint(
            Math.round(this.Left + this.width / 2),
            Math.round(this.Top + this.height / 2)
        )
    }

    intersects(otherRect: TRect): boolean {
        // Wondering if this should use <= ?
        return this.Left < otherRect.Right 
            && this.Right > otherRect.Left 
            && this.Top < otherRect.Bottom
            && this.Bottom > otherRect.Top
    }

    contains(point: TPoint): boolean {
        // Wondering if this should use <= ?
        return this.Left < point.X 
            && this.Right > point.X 
            && this.Top < point.Y
            && this.Bottom > point.Y 
    }

    copy(): TRect {
        return new TRect(this.Left, this.Top, this.Right, this.Bottom)
    }

    scale(scale: number) {
        return new TRect(this.Left * scale, this.Top * scale, this.Right * scale, this.Bottom * scale)
    }
}
