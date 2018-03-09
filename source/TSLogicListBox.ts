import * as m from "mithril"
import { TSVariableState } from "./TSVariable"
import { TSDesiredStateVariableWrapper } from "./TSDesiredStateVariableWrapper"
import { Glyph } from "./VariablesView"
import { TQuickFillComboBox } from "./TQuickFillComboBox";

export class TSLogicListBox {
    inputElement: TQuickFillComboBox

    view(vnode: any) {
        const { selections, items, world, onchange: onchangeCallback, ...attrs } = vnode.attrs
        
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
                        // TODO: problem as events bubble up from combobox: this.inputElement.focus()
                    }
                },
                selections.map((wrapper: TSDesiredStateVariableWrapper, i: number) => {
                    return m("li.ba.bg-light-gray.fl.ml1.mt1.mb1.br1",
                        {
                            id: i,
                            style: {
                                "word-wrap": "break-word",
                            }
                        },
                        m("span.pl1.b", {
                            onclick: (event: any) => {
                                wrapper.desiredState = wrapper.desiredState ? TSVariableState.kAbsent : TSVariableState.kPresent
                                if (onchangeCallback) onchangeCallback(selections)
                            }    
                        }, wrapper.desiredState ? Glyph.present : Glyph.absent),
                        wrapper.variable.phrase,
                        m("span.ml2", {
                            onclick: () => {
                                selections.splice(i, 1)
                                if (onchangeCallback) onchangeCallback(selections)
                            },
                        }, "x")
                    )
                }),
                m("li.ml1.mt1.mb1", 
                    m(TQuickFillComboBox, <any>{
                        id: -1,
                        items: items,
                        value: "",
                        ignoreLeadingCharacter: "~+",
                        clearAfterAccept: true,
                        style: {
                            "margin-left": "0.25rem",
                            "border": "0",
                            "white-space": "nowrap",
                        },
                        oncreate: (vnode: any) => {
                            this.inputElement = <TQuickFillComboBox>(vnode.state)
                        },
                        onchange: (event: any) => {
                            if (event.target.value) {
                                let desiredState = TSVariableState.kPresent
                                let variableName = event.target.value.trim()
                                if (variableName.startsWith("~")) {
                                    variableName = variableName.substring(1).trim()
                                    desiredState = TSVariableState.kAbsent
                                } else if (variableName.startsWith("+")) {
                                    variableName = variableName.substring(1).trim()
                                }
                                selections.push(new TSDesiredStateVariableWrapper(world.findOrCreateVariable(variableName, false), desiredState))
                                event.target.value = ""
                                if (onchangeCallback) onchangeCallback(selections)
                            }
                        }
                    })
                )
            )
        )
    }
}
