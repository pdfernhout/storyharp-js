import * as m from "mithril"

export class LinkWizardView {
    domain: any

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    view() {
        return m("div", "Unfinished LinkWizardView")
    }
}
