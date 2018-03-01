import * as m from "mithril"

export class RuleMapView {
    domain: any

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    view() {
        return m(".RuleMapView", "Unfinished RuleMapForm")
    }
}
