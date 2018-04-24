define(["require", "exports", "mithril", "./IndividualRuleView", "./RuleTableView", "./RuleMapView", "./RuleBrowserView", "./ContextWizardView", "./CommandWizardView", "./LinkWizardView", "./common", "./LogView", "./ToastView", "./ModalInputView"], function (require, exports, m, IndividualRuleView_1, RuleTableView_1, RuleMapView_1, RuleBrowserView_1, ContextWizardView_1, CommandWizardView_1, LinkWizardView_1, common_1, LogView_1, ToastView_1, ModalInputView_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class RuleEditorForm {
        constructor(vnode) {
            this.lastSearchString = "";
            this.domain = vnode.attrs.domain;
        }
        setCurrentWizard(event, wizardName) {
            this.domain.currentEditorWizard = wizardName;
            event.target.blur();
        }
        viewWizards() {
            const currentWizard = this.domain.currentEditorWizard;
            const domain = this.domain;
            return m("div.flex-auto.h-100.w-100.flex.flex-column.overflow-hidden", m("div.mb2.flex-none", "Wizard:", m(common_1.notebookTabButton(currentWizard === "context"), { onclick: (event) => this.setCurrentWizard(event, "context") }, "Context"), m(common_1.notebookTabButton(currentWizard === "command"), { onclick: (event) => this.setCurrentWizard(event, "command") }, "Command"), m(common_1.notebookTabButton(currentWizard === "link"), { onclick: (event) => this.setCurrentWizard(event, "link") }, "Link")), m("div.WizardHolder.w-100.h-100.flex-auto.overflow-hidden", currentWizard === "context" ? m(ContextWizardView_1.ContextWizardView, { domain: domain }) : [], currentWizard === "command" ? m(CommandWizardView_1.CommandWizardView, { domain: domain }) : [], currentWizard === "link" ? m(LinkWizardView_1.LinkWizardView, { domain: domain }) : []));
        }
        setCurrentView(event, editorName) {
            this.domain.currentEditorView = editorName;
            event.target.blur();
        }
        search() {
            ModalInputView_1.modalPrompt("Search string? [case insensitive]", this.lastSearchString).then(newSearchString => {
                if (!newSearchString)
                    return;
                this.lastSearchString = newSearchString;
                this.searchForAndSelectRule(newSearchString, true, true);
            });
        }
        searchForAndSelectRule(aText, ignoreCase, goDown) {
            const domain = this.domain;
            let match;
            let ruleIndex = 0;
            if (domain.editedRule) {
                ruleIndex = domain.world.rules.indexOf(domain.editedRule);
            }
            const matchText = ignoreCase ? aText.toLocaleLowerCase() : aText;
            let count = 1;
            let row = 0;
            while ((count <= domain.world.rules.length)) {
                if (goDown) {
                    row = (ruleIndex + count) % domain.world.rules.length;
                }
                else {
                    row = ((domain.world.rules.length * 2) + (ruleIndex - count)) % domain.world.rules.length;
                }
                const rule = domain.world.rules[row];
                let textForRule = [
                    rule.context.phrase,
                    rule.command.phrase,
                    rule.reply,
                    rule.move.phrase,
                    rule.changesString,
                    rule.requirementsString
                ].join("|");
                if (ignoreCase)
                    textForRule = textForRule.toLocaleLowerCase();
                const match = textForRule.includes(matchText);
                if (match) {
                    domain.world.deselectAllExcept(rule);
                    domain.editRule(rule, common_1.ScrollIntoViewDirection.kFromTop, true);
                    rule.selected = true;
                    return;
                }
                count += 1;
            }
            ToastView_1.toast("Search string \"" + aText + "\" not found.");
        }
        view() {
            const currentView = this.domain.currentEditorView;
            const domain = this.domain;
            return m(".RuleEditorForm.h-100.w-100.flex.flex-column.flex-nowrap.overflow-hidden", m("div.flex-none.mb2", m("span", "Rule Editor:"), m(common_1.notebookTabButton(currentView === "table"), { onclick: (event) => this.setCurrentView(event, "table") }, "Table"), m(common_1.notebookTabButton(currentView === "map"), { onclick: (event) => this.setCurrentView(event, "map") }, "Map"), m(common_1.notebookTabButton(currentView === "browser"), { onclick: (event) => this.setCurrentView(event, "browser") }, "Browser"), m(common_1.notebookTabButton(currentView === "wizards"), { onclick: (event) => this.setCurrentView(event, "wizards") }, "Wizards"), m(common_1.notebookTabButton(currentView === "log"), { onclick: (event) => this.setCurrentView(event, "log") }, "Log"), m("div.dib.ml4", m("button.w3.mt1", {
                disabled: !domain.worldCommandList.isUndoEnabled(),
                onclick: () => domain.worldCommandList.undoLast(),
                title: "Undo " + domain.worldCommandList.undoDescription()
            }, "Undo"), m("button.ml1.w3.mt1", {
                disabled: !domain.worldCommandList.isRedoEnabled(),
                onclick: () => domain.worldCommandList.redoLast(),
                title: "Redo " + domain.worldCommandList.redoDescription()
            }, "Redo"))), currentView === "wizards"
                ? this.viewWizards()
                : [
                    m("div.flex-auto.h-100.w-100.overflow-hidden", currentView === "table" ? m(RuleTableView_1.RuleTableView, { domain: domain }) : [], currentView === "map" ? m(RuleMapView_1.RuleMapView, { domain: domain }) : [], currentView === "browser" ? m(RuleBrowserView_1.RuleBrowserView, { domain: domain }) : [], currentView === "log" ? m(LogView_1.LogView, { domain: domain }) : []),
                    currentView !== "log"
                        ? m("div.flex-none", m(IndividualRuleView_1.IndividualRuleView, { domain: domain, ruleEditorForm: this }))
                        : []
                ]);
        }
    }
    exports.RuleEditorForm = RuleEditorForm;
});
