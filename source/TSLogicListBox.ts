import * as m from "mithril"
import { TSVariableState } from "./TSVariable"
import { TSDesiredStateVariableWrapper } from "./TSDesiredStateVariableWrapper"
import { Glyph } from "./VariablesView"

export class TSLogicListBox {
    view(vnode: any) {
        const items = vnode.attrs.items
        const world = vnode.attrs.world
        
        return m("div.LogicListBox",
            m("ul",
                {
                    style:  {
                        "list-style-type": "none",
                    },
                },
                items.map((item: TSDesiredStateVariableWrapper, i: number) => {
                    return m("li.ba.bg-light-yellow.fl.mr1.mb1.br1",
                        m("span", {
                            onclick: (event: any) => {
                                // TODO: make this a command
                                item.desiredState = item.desiredState ? TSVariableState.kAbsent : TSVariableState.kPresent
                            }    
                        }, item.desiredState ? Glyph.present : Glyph.absent),
                        item.variable.phrase,
                        m("span.ml2", {
                            // make this int command
                            onclick: () => items.splice(i, 1),
                        }, "x")
                    )
                }),
                m("li", 
                    m("input", {
                        onchange: (event: any) => {
                            if (event.target.value) {
                                // TODO: Make this a command
                                items.push(new TSDesiredStateVariableWrapper(world.findOrCreateVariable(event.target.value, false), TSVariableState.kPresent))
                            }
                        }
                    })
                )
            )
        )
    }
}
