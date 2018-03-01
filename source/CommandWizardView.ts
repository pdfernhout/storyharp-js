import * as m from "mithril"

export class CommandWizardView {
    domain: any

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    view() {
        return m(".CommandWizardView", "Unfinished CommandWizardView")
    }
}
