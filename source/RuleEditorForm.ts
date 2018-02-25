import * as m from "mithril"

type ViewName = "table" | "map" | "browser"

class RuleTableView {
    domain: any

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    view() {
        return m("div", "Unfinished RuleTableForm")
    }
}

class RuleMapView {
    domain: any

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    view() {
        return m("div", "Unfinished RuleMapForm")
    }
}

class RuleBrowserView {
    domain: any

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    view() {
        return m("div", "Unfinished RuleBrowserForm")
    }
}

class IndividualRuleView {
    domain: any

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    view() {
        function SpeedButtonClick() { console.log("SpeedButtonClick") }

        return m("div.IndividualRuleView",
            m("div.PanelRest.TPanel",
                {
                },
                m(".Context",
                    m("div.RuleNumberLabel.TLabel",
                        {
                            title: "The index of the edited rule in the table",
                        },
                        "#1234",
                    ),
                    m("button.ContextSpeedButton.TSpeedButton",
                        {
                            onclick: SpeedButtonClick,
                            title: "Browse all rules with this context",
                        },
                        "Context",
                    ),
                    m("input.ContextEdit.TEdit",
                        {
                        },
                    ),
                ),
                m(".Command",
                    m("button.CommandSpeedButton.TSpeedButton",
                        {
                            onclick: SpeedButtonClick,
                            title: "Browse all rules with this command",
                        },
                        "Command",
                    ),
                    m("input.CommandEdit.TEdit",
                        {
                        },
                    ),
                ),
                m(".Reply",
                    m("Group.Group.g00000064",
                        m("img.replyPicture.TImage",
                            {
                                title: "Test saying the reply",
                            },
                        ),
                        m("div.Label5.TLabel",
                            {
                            },
                            "Reply",
                        ),
                    ),
                    m("textarea.ReplyMemo.TMemo",
                        {
                        },
                    ),
                ),
                m(".Move",
                    m("button.MoveSpeedButton.TSpeedButton",
                        {
                            onclick: SpeedButtonClick,
                            title: "Browse all rules with this move",
                        },
                        "Move",
                    ),
                    m("input.MoveEdit.TEdit",
                        {
                        },
                    ),
                ),
            ),
            m("div.SplitterRequirementsChanges.TSplitter",
                "--------------------------------------------------------------"
            ),
            m("div.PanelRequirementsChanges.TPanel",
                {
                },
                m(".Requirements",
                    m("button.RequirementsSpeedButton.TSpeedButton",
                        {
                            onclick: SpeedButtonClick,
                            title: "Browse all rules with the selected requirement",
                        },
                        "Requirements",
                    ),
                    m("TListBox.RequirementsListBox.TListBox",
                        {
                        },
                    ),
                    m("input.RequirementsEdit.TEdit",
                        {
                        },
                    ),
                ),
                m(".Changes",
                    m("button.ChangesSpeedButton.TSpeedButton",
                        {
                            onclick: SpeedButtonClick,
                            title: "Browse all rules with the selected change",
                        },
                        "Changes",
                    ),
                    m("input.ChangesEdit.TEdit",
                        {
                        },
                    ),
                    m("TListBox.ChangesListBox.TListBox",
                        {
                        },
                    ),

                ),
            ),
        )
    }
}

export class RuleEditorForm {
    domain: any
    currentView: ViewName= "table"

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    view(vnode: m.Vnode) {
        const currentView = this.currentView
        const domain = (<any>vnode.attrs).domain

        function buttonWithHighlight(selection: ViewName) {
            return "button.ml2.w4" + (currentView === selection ? ".bg-light-blue" : "")
        }

        return m(".RuleEditorForm.ml3",
            m("span.b", "Rule Editor"),
            m(buttonWithHighlight("table"), { onclick: () => this.currentView = "table" }, "Table"),
            m(buttonWithHighlight("map"),  { onclick: () => this.currentView = "map" }, "Map"),
            m(buttonWithHighlight("browser"),  { onclick: () => this.currentView = "browser" }, "Browser"),
            // TODO: Probably should wrap these with hidden divs so the component state is preserved
            m("div.mt2",
                currentView === "table" ? m(RuleTableView, <any>{domain: domain}) : [],
                currentView === "map" ? m(RuleMapView, <any>{domain: domain}) : [],
                currentView === "browser" ? m(RuleBrowserView, <any>{domain: domain}) : [],
            ),
            m(IndividualRuleView, <any>{domain: domain})
        )
    }
}
