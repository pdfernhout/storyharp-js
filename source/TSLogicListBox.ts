import * as m from "mithril"
import { TSVariableState } from "./TSVariable"
import { TSDesiredStateVariableWrapper } from "./TSDesiredStateVariableWrapper"
import { Glyph } from "./VariablesView"

export class TSLogicListBox {
    inputElement: HTMLInputElement

    view(vnode: any) {
        const { items, world, ...attrs } = vnode.attrs
        
        return m("div.dib.LogicListBox.ba.bg-white",
            attrs,
            m("ul.bg-white.pl1.pr1",
                {
                    style:  {
                        "list-style-type": "none",
                        "-webkit-margin-before": "0em",
                        "-webkit-margin-after": "0em",
                        "-webkit-padding-start": "0px",
                    },
                    onclick: (event: any) => {
                        this.inputElement.focus()
                    }
                },
                items.map((item: TSDesiredStateVariableWrapper, i: number) => {
                    return m("li.ba.bg-light-gray.fl.ml1.mt1.mb1.br1",
                        {
                            style: {
                                "word-wrap": "break-word",
                            }
                        },
                        m("span.pl1.b", {
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
                m("li.ml1.mt1.mb1", 
                    m("input.ml1.mt1.mb1", {
                        style: {
                            "border": "0",
                            "white-space": "nowrap",
                        },
                        oncreate: (vnode: any) => {
                            this.inputElement = <HTMLInputElement>(vnode.dom)
                        },
                        onchange: (event: any) => {
                            if (event.target.value) {
                                // TODO: Make this a command
                                let desiredState = TSVariableState.kPresent
                                let variableName = event.target.value.trim()
                                if (variableName.startsWith("~") {
                                    variableName = variableName.substring(1).trim()
                                    desiredState = TSVariableState.kAbsent
                                } else if (variableName.startsWith("+")) {
                                    variableName = variableName.substring(1).trim()
                                }
                                items.push(new TSDesiredStateVariableWrapper(world.findOrCreateVariable(variableName, false), desiredState))
                                event.target.value = ""
                            }
                        }
                    })
                )
            )
        )
    }
}
