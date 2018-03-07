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
    clGreen,
    clBlue,
    clRed,
    clBlack
}

export function caption(text: string) {
    return text.replace("&", "")
}

export function expander(expanded: boolean, closedLabel: string = "", openLabel: string = ""): string {
    return expanded
        ? "▲" + openLabel
        : "▼" + closedLabel
}

/*
export function ShowMessage(message: string) {
    alert(message)
}
*/

export enum ScrollIntoViewDirection { kFromBottom, kFromTop }

export function makeFileNameWithWldExtension(fileName: string) {
    if (!fileName.endsWith(".wld")) {
        return fileName + ".wld"
    } else {
        return fileName
    }
}

export function makeFileNameWithoutWldExtension(fileName: string) {
    if (fileName.endsWith(".wld")) {
       return fileName.substring(0, fileName.length - 4)
    } else {
        return fileName
    }
}
