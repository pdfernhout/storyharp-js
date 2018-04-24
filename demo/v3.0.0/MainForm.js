define(["require", "exports", "mithril", "./common", "./AboutForm", "./DemoFilesForm", "./RuleEditorForm", "./ConsoleForm", "./FileForm", "./ToastView", "./ModalInputView"], function (require, exports, m, common_1, AboutForm_1, DemoFilesForm_1, RuleEditorForm_1, ConsoleForm_1, FileForm_1, ToastView_1, ModalInputView_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class MainForm {
        constructor(vnode) {
            this.domain = vnode.attrs.domain;
        }
        view() {
            const domain = this.domain;
            function setActiveForm(event, formName) {
                domain.activeForm = formName;
                event.target.blur();
                if (formName === "console") {
                    domain.world.updateAvailable();
                }
            }
            return m(".MainForm.pa2.h-100.w-100.overflow-hidden.flex.flex-column", m(ModalInputView_1.ModalInputView), m(ToastView_1.ToastView), m("div.mt1.mb1.flex-none", m("span.f5.b", "StoryHarp:"), m("span.i.ml1", "" + common_1.makeFileNameWithoutWldExtension(domain.worldFileName)), m("span.ml1.i", { title: "world change count" }, domain.isWorldFileChanged() ? `<${domain.worldChangeCount}>` : "")), m("div.mb2.flex-none", m(common_1.notebookTabButton(domain.activeForm === "file"), { onclick: (event) => setActiveForm(event, "file") }, "File"), m(common_1.notebookTabButton(domain.activeForm === "console"), { onclick: (event) => setActiveForm(event, "console") }, "Player"), m(common_1.notebookTabButton(domain.activeForm === "ruleEditor"), { onclick: (event) => setActiveForm(event, "ruleEditor") }, "Editor"), m(common_1.notebookTabButton(domain.activeForm === "demos"), { onclick: (event) => setActiveForm(event, "demos") }, "Demos"), m(common_1.notebookTabButton(domain.activeForm === "about"), { onclick: (event) => setActiveForm(event, "about") }, "About")), m("div.h-100.w-100.flex-auto.overflow-hidden", domain.activeForm === "about" ? m(AboutForm_1.AboutForm) : [], domain.activeForm === "demos" ? m(DemoFilesForm_1.DemoFilesForm, { domain: domain }) : [], domain.activeForm === "console" ? m(ConsoleForm_1.ConsoleForm, { domain: domain }) : [], domain.activeForm === "ruleEditor" ? m(RuleEditorForm_1.RuleEditorForm, { domain: domain }) : [], domain.activeForm === "file" ? m(FileForm_1.FileForm, { domain: domain }) : []));
        }
    }
    exports.MainForm = MainForm;
});
