import * as m from "mithril"

import { notebookTabButton, makeFileNameWithoutWldExtension } from "./common"
import { TSDomain, FormName } from "./TSDomain"
import { AboutForm } from "./AboutForm"
import { DemoFilesForm } from "./DemoFilesForm"
import { RuleEditorForm } from "./RuleEditorForm"
import { ConsoleForm } from "./ConsoleForm"
import { FileForm } from "./FileForm"
import { ToastView } from "./ToastView";
import { ModalInputView } from "./ModalInputView";
import { ConfigForm } from "./ConfigForm"

export class MainForm {
    domain: TSDomain

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    view() {
        const domain: TSDomain = this.domain

        function setActiveForm(event: any, formName: FormName) {
            domain.activeForm = formName
            event.target.blur()
            if (formName === "console") {
                domain.world.updateAvailable()
            }
        }
     
        return m(".MainForm.pa2-ns.h-100.w-100.flex.flex-column" + "." + domain.baseFontClass,
            m(ModalInputView),
            m(ToastView),
            m("div.mt1.mb1.flex-none.w-100.flex",
                m("span.b", {
                    onclick: () => {
                        domain.menuOpen = !domain.menuOpen
                    },
                    title: "Click to toggle menu"
                }, "☰StoryHarp:"),
                m("span.ml1.i.truncate.flex-auto", "" + makeFileNameWithoutWldExtension(domain.worldFileName)),
                m("span.ml1.i", { title: "world change count" }, domain.isWorldFileChanged() ? `<${domain.worldChangeCount}>` : "")
            ),
            domain.menuOpen && m("div.mb2.flex-none",
                m(notebookTabButton(domain.activeForm === "file"), { onclick: (event: any) => setActiveForm(event, "file") }, "File"),
                m(notebookTabButton(domain.activeForm === "console"), { onclick: (event: any) => setActiveForm(event, "console") }, "Player"),
                m(notebookTabButton(domain.activeForm === "ruleEditor"), { onclick: (event: any) => setActiveForm(event, "ruleEditor") }, "Editor"),
                m(notebookTabButton(domain.activeForm === "demos"), { onclick: (event: any) => setActiveForm(event, "demos") }, "Demos"),
                m(notebookTabButton(domain.activeForm === "about"), { onclick: (event: any) => setActiveForm(event, "about") }, "About"),
                m(notebookTabButton(domain.activeForm === "config"), { onclick: (event: any) => setActiveForm(event, "config") }, "Config"),
            ),
            m("div.flex-auto",
                domain.activeForm === "about" ? m(AboutForm) : [],
                domain.activeForm === "demos" ? m(DemoFilesForm, <any>{domain: domain}) : [],
                domain.activeForm === "console" ? m(ConsoleForm, <any>{domain: domain}) : [],
                domain.activeForm === "ruleEditor" ? m(RuleEditorForm, <any>{domain: domain}) : [],
                domain.activeForm === "file" ? m(FileForm, <any>{domain: domain}) : [],
                domain.activeForm === "config" ? m(ConfigForm, <any>{domain: domain}) : [],
            )
        )
    }
}
