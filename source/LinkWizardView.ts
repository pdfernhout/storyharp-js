import * as m from "mithril"
import { TSDomain } from "./TSDomain"

export class LinkWizardView {
    domain: TSDomain

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    view() {
        return m(".LinkWizardView", "Unfinished LinkWizardView")
    }
}
