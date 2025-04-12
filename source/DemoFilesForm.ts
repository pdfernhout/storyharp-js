import * as m from "mithril"
import { TSDomain, DemoEntry } from "./TSDomain"
import { confirmUnsavedChangesLoss } from "./FileForm"

export class DemoFilesForm {
    domain: TSDomain

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }
    
    view() {
        const domain = this.domain

        if (!domain.demoConfig) {
            return m("div.h-100.w-100.overflow-y-scroll.overflow-x-auto",
                m("div.pa2", "Something went wrong. No demo world files available.")
            )
        }

        return m("div.h-100.w-100.overflow-y-scroll.overflow-x-auto",
            m("div", "Choose a demo world file to load:"),
            m("br"),
            m("table.ml2",
                m("colgroup", 
                    m("col", {style: "width: 30%"}), 
                    m("col", {style: "width: 70%"})),
                m("tr",
                    m("th", "Name"),
                    m("th.ml2", "Description")
                ),
                domain.demoConfig.demoWorldFiles.map((entry: DemoEntry) => 
                    m("tr", 
                        { onclick: () => {
                            confirmUnsavedChangesLoss(domain).then(value => {
                                if (!value) return
                                domain.loadWorldFromServerData(entry.name).then((loaded) => {
                                    if (loaded) domain.activeForm = "console"
                                    location.hash = ""
                                })
                            })
                        }
                    },
                        m("td.tr", entry.name),
                        m("td.pl3.i.pt3.f-smaller", entry.description)
                    )
                ),
            )
        )
    }
}
