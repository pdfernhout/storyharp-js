import * as m from "mithril"
import { TWorld } from "./TWorld"
import { TSRule } from "./TSRule"
import { TSDomain } from "./TSDomain"

export class RuleTableView {
    domain: TSDomain

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    ruleClicked(event: MouseEvent, rule: TSRule): void {
        const world: TWorld = this.domain.world
        const index = world.rules.indexOf(rule)
        const editedRule: TSRule | null = this.domain.editedRule
        const lastSingleRuleIndex: number = this.domain.lastSingleRuleIndex

        const isShiftClick = event.shiftKey
        const isControlClick = event.ctrlKey
        
        // TODO: POSSIBLE BUG: What if deselect the currently edited rule using control click?
        // TODO: POSSIBLE BUG: Seems like might be an issue if first click is a shift click?
        if (isShiftClick) {
            if ((lastSingleRuleIndex >= 0) && (lastSingleRuleIndex < world.rules.length) && (lastSingleRuleIndex !== index)) {
                world.deselectAllExcept(rule)
                if (lastSingleRuleIndex < index) {
                    for (let i = lastSingleRuleIndex; i <= index; i++) {
                        world.rules[i].selected = true
                    }
                } else if (lastSingleRuleIndex > index) {
                    for (let i = lastSingleRuleIndex; i >= index; i--) {
                        world.rules[i].selected = true
                    }
                }
            }
        } else if (isControlClick) {
            rule.selected = !rule.selected
        } else {
            // just plain click
            if (!rule.selected) {
                world.deselectAllExcept(rule)
                rule.selected = true
                this.domain.lastSingleRuleIndex = index
            } else {
                // do nothing except maybe drag...
            }
        }
        if (rule.selected && (editedRule !== rule) && !isControlClick && !isShiftClick) {
            this.domain.editedRule = rule
        }
    }

    view() {
        const world: TWorld = this.domain.world
        const editedRule: TSRule | null = this.domain.editedRule
        let row = 0

        function ellipsis(text: string): string { return text.length > 58 ? text.substring(0, 58) + "..." : text }
        function color(rule: TSRule, row: number): string { return rule === editedRule ? ".bg-light-blue" : (row % 2 == 0) ? ".bg-washed-green" : "" }
        function styleForSelected(rule: TSRule): string { return rule.selected ? (rule === editedRule ? ".ba.bw2" : ".ba.bw2") : "" }

        return m(".RuleTableView.h-100.overflow-auto",
            m("table.collapse",
                m("tr",
                    m("th.w-10", "context"),
                    m("th.w-20", "requirements"),
                    m("th.w-20", "command"),
                    m("th.w-20", "reply"),
                    m("th.w-10", "move"),
                    m("th.w-20", "changes"),
                ),
                world.rules.map(rule => 
                    m("tr" + color(rule, row++) + styleForSelected(rule),
                        {
                            onclick: (event: any) => this.ruleClicked(event, rule)
                        },
                        m("td.w-10", rule.context.phrase),
                        m("td.w-20", rule.requirements.map(wrapper => m("div.nowrap", wrapper.displayString()))),
                        m("td.w-20", rule.command.phrase),
                        m("td.w-20", ellipsis(rule.reply)),
                        m("td.w-10", rule.move.phrase),
                        m("td.w-20", rule.changes.map(wrapper => m("div.nowrap", wrapper.displayString()))),
                    )
                )   
            )
        )
    }
}
