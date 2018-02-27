import * as m from "mithril"
import { TWorld } from "./TWorld";
import { TSRule } from "./TSRule";

export class RuleTableView {
    domain: any

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    ruleClicked(event: MouseEvent, rule: TSRule): void {
        const world: TWorld = this.domain.world
        const index = world.rules.indexOf(rule)
        const editedRule: TSRule = this.domain.editedRule
        const lastSingleRuleIndex: number = this.domain.lastSingleRuleIndex

        const isShiftClick = event.shiftKey
        const isControlClick = event.ctrlKey
        
        // TODO: POSSIBLE BUG: What if deselect the currently edited rule using control click?
        // TODO: POSSIBLE BUG: Seems like might be an issue if first click is a shift click?
        if (isShiftClick) {
            if ((lastSingleRuleIndex >= 0) && (lastSingleRuleIndex <= world.rules.length - 1) && (lastSingleRuleIndex !== index)) {
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
            // TODO: Remove this -- odd how it is not easy to access one component from a sibling component
            // this.editRule(rule)
            this.domain.editedRule = rule
        }
    }

    view() {
        const world: TWorld = this.domain.world
        const editedRule: TSRule = this.domain.editedRule
        let row = 0

        function color(row: number): string { return (row % 2 == 0) ? ".bg-washed-green" : "." }
        function ellipsis(text: string): string { return text.length > 58 ? text.substring(0, 58) + "..." : text }
        function selected(rule: TSRule): string { return rule.selected ? (rule === editedRule ? ".ba.bw2" : ".ba.bw1") : "" }

        return m("div",
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
                    m("tr" + color(row++) + selected(rule),
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