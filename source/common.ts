export type int = number


export class TPoint {
    X: int
    Y: int

    constructor(x = 0, y = 0) {
        this.X = Math.round(x)
        this.Y = Math.round(y)
    }

    copy(): TPoint {
        return new TPoint(this.X, this.Y)
    }
}

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

export function arrayRemove(theArray: Array<any>, item: any) {
    const index = theArray.indexOf(item)
    if (index !== -1) {
        theArray.splice(index, 1)
    }
}

export function StrToInt(value: string): int {
    return parseInt(value)
}

export function compareTextIgnoreCase(a: string, b: string): boolean {
    return a.toUpperCase() === b.toUpperCase()
}

/*
export function ShowMessage(message: string) {
    alert(message)
}
*/
