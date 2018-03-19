import * as m from "mithril"
import { expander } from "./common"
import { storyHarpVersion } from "./version"
import { authoringHelp } from "./authoringHelp"

// Could put showAuthoringHelp in domain
let showAuthoringHelp = false

export class AboutForm {

    view() {
        return m("div.h-100.w-100.overflow-auto",
            m("img", { style: "float: left", src: "resources/harp.png", alt: "Picture of a harp" }),
            m("h3", "StoryHarp v" + storyHarpVersion),
            m("p", `
                A tool for authoring and playing Interactive Fiction adventure stories where the player picks from a list of choices.
            `),
            m("p", "Website: ", m("a", { href: "http://storyharp.com" }, "StoryHarp.com")),
            m("p.cl"),
            m("div", { onclick: () => showAuthoringHelp = !showAuthoringHelp }, expander(showAuthoringHelp, "(Click for:) ") + "Authoring Help"),
            showAuthoringHelp ? authoringHelp.split("\n\n").map(text => m("p", text)) : [],
            m("hr"),
            // m("p", "StoryHarp 1.0 was originally a stand-alone desktop program in Delphi. Version 2.0 was in Java but was not released. Version 3.0 is web-based."),
            // m("p"),
            m("p", "StoryHarp 3.0 Copyright 1998-2018 Paul D. Fernhout and Cynthia F. Kurtz"),
            m("p", "StoryHarp is a trademark of Paul D. Fernhout and Cynthia F. Kurtz")
        )
    }
}
