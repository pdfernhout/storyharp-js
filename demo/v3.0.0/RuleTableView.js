define(["require", "exports", "mithril", "./common", "./VariablesView"], function (require, exports, m, common_1, VariablesView_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class RuleTableView {
        constructor(vnode) {
            this.domain = vnode.attrs.domain;
        }
        ruleClicked(event, rule) {
            const world = this.domain.world;
            const index = world.rules.indexOf(rule);
            const editedRule = this.domain.editedRule;
            const lastSingleRuleIndex = this.domain.lastSingleRuleIndex;
            const isShiftClick = event.shiftKey;
            const isControlClick = event.ctrlKey;
            if (isShiftClick) {
                if ((lastSingleRuleIndex >= 0) && (lastSingleRuleIndex < world.rules.length) && (lastSingleRuleIndex !== index)) {
                    world.deselectAllExcept(rule);
                    if (lastSingleRuleIndex < index) {
                        for (let i = lastSingleRuleIndex; i <= index; i++) {
                            world.rules[i].selected = true;
                        }
                    }
                    else if (lastSingleRuleIndex > index) {
                        for (let i = lastSingleRuleIndex; i >= index; i--) {
                            world.rules[i].selected = true;
                        }
                    }
                }
            }
            else if (isControlClick) {
                rule.selected = !rule.selected;
            }
            else {
                if (!rule.selected) {
                    world.deselectAllExcept(rule);
                    rule.selected = true;
                    this.domain.lastSingleRuleIndex = index;
                }
                else {
                }
            }
            if (rule.selected && (editedRule !== rule) && !isControlClick && !isShiftClick) {
                this.domain.editRule(rule);
            }
        }
        findSelectedRuleToBeScrolledIntoView(direction) {
            let firstSelectedRuleIndex = -1;
            if (direction === common_1.ScrollIntoViewDirection.kFromBottom) {
                for (let i = this.domain.world.rules.length - 1; i >= 0; i--) {
                    const rule = this.domain.world.rules[i];
                    if (!rule.selected) {
                        continue;
                    }
                    firstSelectedRuleIndex = i;
                    break;
                }
            }
            else {
                for (let i = 0; i < this.domain.world.rules.length; i++) {
                    const rule = this.domain.world.rules[i];
                    if (!rule.selected) {
                        continue;
                    }
                    firstSelectedRuleIndex = i;
                    break;
                }
            }
            if (firstSelectedRuleIndex === -1) {
                return;
            }
            this.domain.pendingTableScroll = {
                rule: this.domain.world.rules[firstSelectedRuleIndex],
                direction,
            };
        }
        scrollToRuleIfNeeded(vnode, rule) {
            if (this.domain.pendingTableScroll && this.domain.pendingTableScroll.rule === rule) {
                const alignToTop = this.domain.pendingTableScroll.direction === common_1.ScrollIntoViewDirection.kFromTop;
                (vnode.dom).scrollIntoView(alignToTop);
                this.domain.pendingTableScroll = null;
            }
        }
        view() {
            const world = this.domain.world;
            const editedRule = this.domain.editedRule;
            let row = 0;
            function ellipsis(text) { return text.length > 58 ? text.substring(0, 58) + "..." : text; }
            function color(rule, row) { return rule === editedRule ? ".bg-light-blue" : (row % 2 == 0) ? ".bg-washed-green" : ""; }
            function styleForSelected(rule) { return rule.selected ? (rule === editedRule ? ".ba.bw2" : ".ba.bw2") : ""; }
            if (this.domain.pendingTableScroll && !this.domain.pendingTableScroll.rule) {
                this.findSelectedRuleToBeScrolledIntoView(this.domain.pendingTableScroll.direction);
            }
            return m(".RuleTableView.h-100.w-100.overflow-auto", m("table.collapse", { style: "width: calc(100% - 2px)" }, m("tr", { key: "header" }, m("th.w-3.tc", "#"), m("th.w-15.pl1.bl.b--moon-gray", "Context", m("span.normal.ml1", VariablesView_1.Glyph.context)), m("th.w-15.pl1.bl.b--moon-gray", "Requirements", m("span.normal.ml1", VariablesView_1.Glyph.requirements)), m("th.w-17.pl1.bl.b--moon-gray", "Command", m("span.normal.ml1", VariablesView_1.Glyph.command)), m("th.w-25.pl1.bl.b--moon-gray", "Reply", m("span.normal.ml1", VariablesView_1.Glyph.reply)), m("th.w-10.pl1.bl.b--moon-gray", "Move", m("span.normal.ml1", VariablesView_1.Glyph.move)), m("th.w-15.pl1.bl.b--moon-gray", "Changes", m("span.normal.ml1", VariablesView_1.Glyph.changes))), world.rules.map((rule, index) => m("tr" + color(rule, row++) + styleForSelected(rule), {
                key: rule.uuid,
                onclick: (event) => this.ruleClicked(event, rule),
                oncreate: (vnode) => this.scrollToRuleIfNeeded(vnode, rule),
                onupdate: (vnode) => this.scrollToRuleIfNeeded(vnode, rule),
            }, m("td.w-3.tc", index + 1), m("td.w-15.pl1.bl.b--moon-gray", rule.context.phrase), m("td.w-15.pl1.bl.b--moon-gray", rule.requirements.map(wrapper => m("div.nowrap", wrapper.displayString()))), m("td.w-17.pl1.bl.b--moon-gray.i", rule.command.phrase), m("td.w-25.pl1.bl.b--moon-gray", ellipsis(rule.reply)), m("td.w-10.pl1.bl.b--moon-gray", rule.move.phrase), m("td.w-15.pl1.bl.b--moon-gray", rule.changes.map(wrapper => m("div.nowrap", wrapper.displayString())))))));
        }
    }
    exports.RuleTableView = RuleTableView;
});
