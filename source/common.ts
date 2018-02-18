export class TPoint {
    x: number
    y: number
}

export function arrayRemove(theArray: Array<any>, item: any) {
    const index = theArray.indexOf(item)
    if (index !== -1) {
        theArray.splice(index, 1)
    }
}
