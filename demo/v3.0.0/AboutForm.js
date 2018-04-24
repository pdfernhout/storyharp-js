define(["require", "exports", "mithril", "./common", "./version", "./authoringHelp"], function (require, exports, m, common_1, version_1, authoringHelp_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    let showAuthoringHelp = false;
    class AboutForm {
        view() {
            return m("div.h-100.w-100.overflow-auto", m("img", { style: "float: left", src: "resources/harp.png", alt: "Picture of a harp" }), m("h3", "StoryHarp v" + version_1.storyHarpVersion), m("p", `
                A tool for authoring and playing Interactive Fiction adventure stories where the player picks from a list of choices.
            `), m("p", "Website: ", m("a", { href: "http://storyharp.com" }, "StoryHarp.com")), m("p.cl"), m("div", { onclick: () => showAuthoringHelp = !showAuthoringHelp }, common_1.expander(showAuthoringHelp, "(Click for:) ") + "Authoring Help"), showAuthoringHelp ? authoringHelp_1.authoringHelp.split("\n\n").map(text => m("p", text)) : [], m("hr"), m("p", "StoryHarp 3.0 Copyright 1998-2018 Paul D. Fernhout and Cynthia F. Kurtz"), m("p", "StoryHarp is a trademark of Paul D. Fernhout and Cynthia F. Kurtz"));
        }
    }
    exports.AboutForm = AboutForm;
});
