import * as m from "mithril"
import { TSDomain } from "./TSDomain"

export class FileForm {
    domain: TSDomain

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }
    
    view() {
        return m("div", "unfinished")
    }
}
