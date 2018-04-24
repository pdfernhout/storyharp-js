define(["require", "exports", "mithril", "./TSDomain", "./MainForm", "./extraStyles"], function (require, exports, m, TSDomain_1, MainForm_1, extraStyles_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    extraStyles_1.addExtraStylesBeyondTachyons();
    const application = new TSDomain_1.TSApplication();
    const BodyComponent = { view: () => m(MainForm_1.MainForm, { domain: application }) };
    const worldName = "House and Yard with media";
    application.loadWorldFromServerData(worldName).then(() => {
        m.mount(document.body, BodyComponent);
    });
    window.addEventListener("resize", () => m.redraw());
    window.onbeforeunload = function () {
        if (application.isWorldFileChanged()) {
            return "You have made changes to the world file that are not yet saved. If you navigate away from this page you will lose your unsaved changes.";
        }
    };
});
