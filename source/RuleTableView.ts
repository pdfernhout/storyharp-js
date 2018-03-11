import * as m from "mithril"
import { TWorld } from "./TWorld"
import { TSRule } from "./TSRule"
import { TSDomain } from "./TSDomain"
import { ScrollIntoViewDirection } from "./common"

export interface PendingTableScroll {
    rule: TSRule | null,
    direction: ScrollIntoViewDirection,
}

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
            this.domain.editRule(rule)
        }
    }

    findSelectedRuleToBeScrolledIntoView(direction: ScrollIntoViewDirection): void { 
        let firstSelectedRuleIndex = -1
        if (direction === ScrollIntoViewDirection.kFromBottom) {
            for (let i = this.domain.world.rules.length - 1; i >= 0; i--) {
                const rule: TSRule = this.domain.world.rules[i]
                if (!rule.selected) {
                    continue
                }
                firstSelectedRuleIndex = i
                break
            }
        } else {
            for (let i = 0; i < this.domain.world.rules.length; i++) {
                const rule: TSRule = this.domain.world.rules[i]
                if (!rule.selected) {
                    continue
                }
                firstSelectedRuleIndex = i
                break
            }
        }
        if (firstSelectedRuleIndex === -1) {
            return
        }

        this.domain.pendingTableScroll = {
            rule: this.domain.world.rules[firstSelectedRuleIndex],
            direction,
        }

        /* TODO: use or remove
        // to account for header
        firstSelectedRuleIndex += 1
        if ((this.RuleGrid.TopRow <= firstSelectedRuleIndex) && (this.RuleGrid.TopRow + this.RuleGrid.VisibleRowCount > firstSelectedRuleIndex)) {
            return
        }
        if (direction === ScrollIntoViewDirection.kFromBottom) {
            this.RuleGrid.TopRow = Math.max(1, firstSelectedRuleIndex - this.RuleGrid.VisibleRowCount + 1)
        } else {
            this.RuleGrid.TopRow = Math.max(1, firstSelectedRuleIndex)
        }
        */
    }

    scrollToRuleIfNeeded(vnode: any, rule: TSRule) {
        if (this.domain.pendingTableScroll && this.domain.pendingTableScroll.rule === rule) {
            const alignToTop = this.domain.pendingTableScroll.direction === ScrollIntoViewDirection.kFromTop
            ;(<HTMLElement>(vnode.dom)).scrollIntoView(alignToTop)
            this.domain.pendingTableScroll = null
        }
    }

    view() {
        const world: TWorld = this.domain.world
        const editedRule: TSRule | null = this.domain.editedRule
        let row = 0

        function ellipsis(text: string): string { return text.length > 58 ? text.substring(0, 58) + "..." : text }
        function color(rule: TSRule, row: number): string { return rule === editedRule ? ".bg-light-blue" : (row % 2 == 0) ? ".bg-washed-green" : "" }
        function styleForSelected(rule: TSRule): string { return rule.selected ? (rule === editedRule ? ".ba.bw2" : ".ba.bw2") : "" }

        if (this.domain.pendingTableScroll && !this.domain.pendingTableScroll.rule) {
            // Find the rule that needs to be scrolled to
            this.findSelectedRuleToBeScrolledIntoView(this.domain.pendingTableScroll.direction)
        }

        return m(".RuleTableView.h-100.overflow-auto",
            m("table.collapse",
                m("tr",
                    { key: "header" },
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
                            key: rule,
                            onclick: (event: any) => this.ruleClicked(event, rule),
                            oncreate: (vnode: m.Vnode) => this.scrollToRuleIfNeeded(vnode, rule),
                            onupdate: (vnode: m.Vnode) => this.scrollToRuleIfNeeded(vnode, rule),
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
