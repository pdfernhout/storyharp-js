import * as m from "mithril"

// TODO: Fix this log
const log: string[] = []

export function addToLog(text: string): void {
    log.push(text)
}

export class LogView {
    view() {
        return m("div.LogView.h-100.overflow-auto",
            log.map(text=> m("pre.pre-wrap", text))
        )
    }
}
