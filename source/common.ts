export type int = number

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

export enum Color {
    clRed,
    clBlue
}

/*
export function ShowMessage(message: string) {
    alert(message)
}
*/
