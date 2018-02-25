import * as m from "mithril"

export class RuleEditorForm {
    domain: any

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    view() {
        return m("div",
            "Unfinished",
        )
    }
}
