import * as m from "mithril"
import { expander } from "./common"
import { TWorld } from "./TWorld";
import { TSCommandList } from "./TSCommandList";
import { TSVariableState } from "./TSVariable";

interface ButtonState {
    present: boolean;
    context: boolean;
    move: boolean;
    requirements: boolean;
    changes: boolean;
    command: boolean;
}

export enum Glyph {
    absent = "◻",
    present =  "⊞",

    context = "⌂",
    move = "⛹",
    requirements = "🔒",
    changes = "⚿",
    command = "！",
    reply = "💬",

    spacer = " ",
}

export class VariablesView {
    domain: any

    expanded = false

    buttonState: ButtonState = {
        present: true,
        context: false,
        move: false,
        requirements: false,
        changes: false,
        command: false,
    }

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    makeToggleButton(buttonName: keyof ButtonState) {
        const state = this.buttonState[buttonName]
        const glyph = Glyph[buttonName]
        return m("button.ml1" + (state ? ".bg-light-blue" : ""),
            { onclick: () => this.buttonState[buttonName] = !this.buttonState[buttonName] },
            glyph
        )
    }

    viewInterior() {
        const world: TWorld = this.domain.world
        const sessionCommandList: TSCommandList = this.domain.sessionCommandList
        const selectedPhrase: string = world.focus ? world.focus.phrase : ""
        const allVariables = world.variables.slice().sort((a, b) => a.phrase.localeCompare(b.phrase))
        const contextVariables =  allVariables.filter(
            variable => variable.contextUseages > 0 || variable.moveUseages > 0
        )
        const shownVariables = allVariables.filter(
            variable => 
                (!this.buttonState.present || variable.getState() === TSVariableState.kPresent) &&
                (!this.buttonState.context || variable.contextUseages > 0) &&
                (!this.buttonState.move || variable.moveUseages > 0) &&
                (!this.buttonState.requirements || variable.requirementsUseages > 0) &&
                (!this.buttonState.changes || variable.changesUseages > 0) &&
                (!this.buttonState.command || variable.commandUseages > 0)
        )

        return m("div.ml1", { style: { "min-width": "25rem" } },
            "Context:",
            m("select.ml1.ma2",
                {
                    onchange: (event: any) => {
                        const newFocusPhrase = event.target.value
                        const newFocus = world.findVariable(newFocusPhrase)
                        if (!newFocus) return

                        if ((newFocus === world.focus) && (newFocus.getState() === TSVariableState.kPresent)) {
                            return
                        }
                        sessionCommandList.moveFocus(this.domain.consoleForm, newFocus)

                    }
                },
                contextVariables
                    .map(variable => variable.phrase)
                    .map(phrase =>
                        m("option", {
                            value: phrase,
                            selected: (phrase === selectedPhrase ? "selected" : undefined),
                        }, phrase)
                    )
            ),
            m("div",
                m("span.ml1", "Filter:"),
                this.makeToggleButton("present"),
                this.makeToggleButton("context"),
                this.makeToggleButton("move"),
                this.makeToggleButton("requirements"),
                this.makeToggleButton("changes"),
                this.makeToggleButton("command"),
            ),
            m("div.overflow-auto.ma2",
                {
                    style: {
                        height: (window.innerHeight - 90) + "px",
                        // "max-width": "12rem",
                    },
                },
                shownVariables.map(variable => 
                    m("div.nowrap",
                        m("span.blue.w1", {
                            onclick: () => sessionCommandList.toggleVariable(this.domain.consoleForm, variable)
                        }, variable.getState() === TSVariableState.kPresent ? Glyph.present : Glyph.absent),
                        m("span.ml1.mw5.truncate.dib", { title: variable.phrase }, variable.phrase),
                        m("div.nowrap.ml1.fr",
                            m("span.blue.w1", variable.contextUseages > 0 ? Glyph.context : Glyph.spacer),
                            m("span.blue.w1", variable.moveUseages > 0 ? Glyph.move : Glyph.spacer),
                            m("span.blue.w1", variable.requirementsUseages > 0 ? Glyph.requirements : Glyph.spacer),
                            m("span.blue.w1", variable.changesUseages > 0 ? Glyph.changes : Glyph.spacer),
                            m("span.blue.w1", variable.commandUseages > 0 ? Glyph.command: Glyph.spacer),
                        )
                    )
                )
            )
        )
    }

    view(vnode: m.Vnode) {
        return m(".VariablesView.fixed.ba.right-0.top-0.bg-washed-blue", 
            m("div", { onclick: () => this.expanded = !this.expanded }, m("span.b", "Variables " + expander(this.expanded))),
            !this.expanded
                ? []
                : this.viewInterior()
        )
    }
}
