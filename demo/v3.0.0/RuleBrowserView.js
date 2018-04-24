define(["require", "exports", "mithril", "./TSRule", "./VariablesView"], function (require, exports, m, TSRule_1, VariablesView_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    const textToDisplayForBlankName = "...empty...";
    class RuleBrowserView {
        constructor(vnode) {
            this.selectedVariable = null;
            this.lastBrowserSingleRuleIndex = 0;
            this.ruleSubset = [];
            this.domain = vnode.attrs.domain;
            this.setOrganizeByField(this.domain.browseBy);
        }
        get browseBy() {
            return this.domain.browseBy;
        }
        viewFirstListBox() {
            let label = TSRule_1.TSRule.headerForField(this.browseBy).toLowerCase();
            if (!label.endsWith("s")) {
                label += "s";
            }
            const scrollIntoViewIfNeeded = (vnode, variable) => {
                if (this.domain.pendingBrowserScroll && this.domain.editedRule && this.selectedVariable === variable) {
                    (vnode.dom).scrollIntoView(true);
                }
            };
            return m("div.h-100.overflow-hidden.flex.flex-column", m("div.flex-none", {
                onclick: () => this.firstListBoxImageClick(),
            }, this.glyphForBrowseBy(), " All ", label), m("div.ba.pa1.flex-auto.overflow-auto", this.domain.world.variables
                .filter((variable) => variable.hasUseagesForField(this.browseBy))
                .sort((a, b) => a.phrase.localeCompare(b.phrase))
                .map((variable) => m("div.mt1" + (variable === this.selectedVariable ? ".bg-light-blue" : ""), {
                key: variable.uuid,
                onclick: () => this.selectedVariable = variable,
                oncreate: (vnode) => scrollIntoViewIfNeeded(vnode, variable),
                onupdate: (vnode) => scrollIntoViewIfNeeded(vnode, variable),
            }, variable.phrase || textToDisplayForBlankName))));
        }
        styleForSelected(rule) {
            return rule.selected
                ? (rule === this.domain.editedRule
                    ? ".ba.bw1.bg-light-blue"
                    : ".ba.bw1")
                : (rule === this.domain.editedRule
                    ? ".ba.bw1.b--white.bg-light-blue"
                    : ".ba.bw1.b--white");
        }
        viewSecondListBox() {
            let displayFieldType;
            let glyph;
            if (this.browseBy === TSRule_1.TSRuleField.kRuleCommand) {
                displayFieldType = TSRule_1.TSRuleField.kRuleContext;
                glyph = VariablesView_1.Glyph.context;
            }
            else {
                displayFieldType = TSRule_1.TSRuleField.kRuleCommand;
                glyph = VariablesView_1.Glyph.command;
            }
            let selectedItemString = TSRule_1.TSRule.headerForField(this.browseBy).toLowerCase();
            if (selectedItemString.endsWith("s")) {
                selectedItemString = selectedItemString.substring(0, selectedItemString.length - 1);
            }
            const caption = TSRule_1.TSRule.headerForField(displayFieldType) + "s with selected " + selectedItemString;
            let rules = [];
            if (this.selectedVariable) {
                for (let i = 0; i < this.domain.world.rules.length; i++) {
                    const rule = this.domain.world.rules[i];
                    if (rule.usesVariableFor(this.selectedVariable, this.browseBy)) {
                        rules.push(rule);
                    }
                }
            }
            this.ruleSubset = rules;
            const scrollIntoViewIfNeeded = (vnode, rule) => {
                if (this.domain.pendingBrowserScroll && this.domain.editedRule && this.domain.editedRule === rule) {
                    (vnode.dom).scrollIntoView(true);
                    this.domain.pendingBrowserScroll = false;
                }
            };
            return m("div.h-100.w-100.overflow-hidden.flex.flex-column", m("div.flex-none", caption), m("div.ba.pa1.h-100.w-100.flex-auto.overflow-auto", rules.map((rule, index) => m("div.us-none" + this.styleForSelected(rule), {
                key: rule.uuid,
                onclick: (event) => this.ruleClicked(event, rule, index),
                oncreate: (vnode) => scrollIntoViewIfNeeded(vnode, rule),
                onupdate: (vnode) => scrollIntoViewIfNeeded(vnode, rule),
            }, rule.variableForField(displayFieldType).phrase || textToDisplayForBlankName))));
        }
        ruleClicked(event, rule, index) {
            if (event.shiftKey) {
                if ((this.lastBrowserSingleRuleIndex >= 0)
                    && (this.lastBrowserSingleRuleIndex < this.ruleSubset.length)
                    && (this.lastBrowserSingleRuleIndex !== index)) {
                    this.domain.world.deselectAllExcept(rule);
                    if (this.lastBrowserSingleRuleIndex < index) {
                        for (let i = this.lastBrowserSingleRuleIndex; i <= index; i++) {
                            const shiftRule = this.ruleSubset[i];
                            shiftRule.selected = true;
                        }
                    }
                    else if (this.lastBrowserSingleRuleIndex > index) {
                        for (let i = this.lastBrowserSingleRuleIndex; i >= index; i--) {
                            const shiftRule = this.ruleSubset[i];
                            shiftRule.selected = true;
                        }
                    }
                }
                else {
                }
            }
            else if (event.ctrlKey) {
                rule.selected = !rule.selected;
            }
            else {
                if (!rule.selected) {
                    this.domain.world.deselectAllExcept(rule);
                    rule.selected = true;
                    this.lastBrowserSingleRuleIndex = index;
                }
                else {
                }
            }
            if (rule.selected && (this.domain.editedRule !== rule) && !(event.ctrlKey) && !(event.shiftKey)) {
                this.domain.editRule(rule);
            }
        }
        firstListBoxImageClick() {
            switch (this.browseBy) {
                case TSRule_1.TSRuleField.kRuleContext:
                    this.setOrganizeByField(TSRule_1.TSRuleField.kRuleCommand);
                    break;
                case TSRule_1.TSRuleField.kRuleCommand:
                    this.setOrganizeByField(TSRule_1.TSRuleField.kRuleMove);
                    break;
                case TSRule_1.TSRuleField.kRuleMove:
                    this.setOrganizeByField(TSRule_1.TSRuleField.kRuleRequirements);
                    break;
                case TSRule_1.TSRuleField.kRuleRequirements:
                    this.setOrganizeByField(TSRule_1.TSRuleField.kRuleChanges);
                    break;
                case TSRule_1.TSRuleField.kRuleChanges:
                    this.setOrganizeByField(TSRule_1.TSRuleField.kRuleContext);
                    break;
                default:
                    throw new Error("Unexpected default: " + this.browseBy);
            }
        }
        setOrganizeByField(newValue) {
            if ((newValue < 0) || (newValue > TSRule_1.TSRuleField.kLastRuleField)) {
                throw new Error("unexpectd value for organizeByField: " + newValue);
            }
            this.domain.browseBy = newValue;
            if (this.domain.editedRule !== null) {
                if (this.domain.pendingBrowserScrollSelectedVariable) {
                    this.selectedVariable = this.domain.pendingBrowserScrollSelectedVariable;
                    this.domain.pendingBrowserScrollSelectedVariable = null;
                }
                else {
                    this.selectedVariable = this.domain.editedRule.variableForFieldWithSelections(this.browseBy, 0, 0);
                }
            }
            else {
                this.selectedVariable = null;
            }
            this.lastBrowserSingleRuleIndex = 0;
        }
        glyphForBrowseBy() {
            switch (this.browseBy) {
                case TSRule_1.TSRuleField.kRuleContext:
                    return VariablesView_1.Glyph.context;
                case TSRule_1.TSRuleField.kRuleCommand:
                    return VariablesView_1.Glyph.command;
                case TSRule_1.TSRuleField.kRuleMove:
                    return VariablesView_1.Glyph.move;
                case TSRule_1.TSRuleField.kRuleRequirements:
                    return VariablesView_1.Glyph.requirements;
                case TSRule_1.TSRuleField.kRuleChanges:
                    return VariablesView_1.Glyph.changes;
                default:
                    throw new Error("unexpected case");
            }
        }
        view() {
            if (this.domain.pendingBrowserScroll) {
                this.setOrganizeByField(this.domain.browseBy);
            }
            return m(".RuleBrowserView.div.flex.flex-row.h-100.w-100.overflow-hidden", m("div.w-30.h-100.overflow-hidden", this.viewFirstListBox()), m("div.w-70.h-100.overflow-hidden", this.viewSecondListBox()));
        }
    }
    exports.RuleBrowserView = RuleBrowserView;
});
