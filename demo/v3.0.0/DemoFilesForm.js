define(["require", "exports", "mithril", "./FileForm"], function (require, exports, m, FileForm_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    class DemoFilesForm {
        constructor(vnode) {
            this.domain = vnode.attrs.domain;
        }
        view() {
            const domain = this.domain;
            return m("div.h-100.w-100.overflow-auto", m("div", "Choose a demo world file to load:"), m("br"), m("table.ml2", { style: "border-spacing: 0.5rem" }, m("tr", m("th", "Name"), m("th.ml2", "Description")), domain.demoConfig.demoWorldFiles.map((entry) => m("tr.mt1", { onclick: () => {
                    FileForm_1.confirmUnsavedChangesLoss(domain).then(value => {
                        if (!value)
                            return;
                        domain.loadWorldFromServerData(entry.name).then((loaded) => {
                            if (loaded)
                                domain.activeForm = "console";
                        });
                    });
                }
            }, m("td.nowrap.tr.f4", entry.name), m("td.ml2.i", entry.description)))));
        }
    }
    exports.DemoFilesForm = DemoFilesForm;
});
