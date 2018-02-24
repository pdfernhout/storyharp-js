import * as m from "mithril"

export function viewConsoleForm(domain: any) {
    return m(".ConsoleForm", "Console Form", JSON.stringify(domain))
}
