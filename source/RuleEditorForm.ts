import * as m from "mithril"
import { TWorld } from "./TWorld"
import { TSRule } from "./TSRule"

type ViewName = "table" | "map" | "browser"

/* From TSRule:
    context: TSVariable
    requirements: TSDesiredStateVariableWrapper[] = []
    command: TSVariable
    reply: string = ""
    move: TSVariable
    changes: TSDesiredStateVariableWrapper[] = []
*/

class RuleTableView {
    domain: any

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    view() {
        const world: TWorld = this.domain.world
        let row = 0

        function color(row: number): string { return (row % 2 == 0) ? ".bg-washed-green" : "." }
        function ellipsis(text: string): string { return text.length > 58 ? text.substring(0, 58) + "..." : text }

        return m("div",
            m("table",
                m("tr", 
                    m("th.w-10", "context"),
                    m("th.w-20", "requirements"),
                    m("th.w-20", "command"),
                    m("th.w-20", "reply"),
                    m("th.w-10", "move"),
                    m("th.w-20", "changes"),
                ),
                world.rules.map(rule => 
                    m("tr" + color(row++),
                        {
                            onclick: () => this.domain.editedRule = rule
                        },
                        m("td.w-10", rule.context.phrase),
                        m("td.w-20", rule.requirements.map(wrapper => m("div.nowrap", wrapper.displayString()))),
                        m("td.w-20", rule.command.phrase),
                        m("td.w-20", ellipsis(rule.reply)),
                        m("td.w-10", rule.move.phrase),
                        m("td.w-20", rule.changes.map(wrapper => m("div.nowrap", wrapper.displayString()))),
                    )
                )   
            )
        )
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
    expanded = true

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    view() {
        function SpeedButtonClick() { console.log("SpeedButtonClick") }
        function MoveDownButtonClick() { console.log("MoveDownButtonClick") }
        function MoveUpButtonClick() { console.log("MoveUpButtonClick") }
        function NewRuleButtonClick() { console.log("NewRuleButtonClick") }
        function DeleteRuleButtonClick() { console.log("DeleteRuleButtonClick") }
        function DuplicateRuleButtonClick() { console.log("DuplicateRuleButtonClick") }
        function InsertMusicButtonClick() { console.log("InsertMusicButtonClick") }
        function InsertSoundClick() { console.log("insertSoundClick") }

        const world: TWorld = this.domain.world
        let rule: TSRule = this.domain.editedRule

        // TODO: Resolve how to handle a null rule better

        return m("div.IndividualRuleView.ba.bg-light-gray.w-100.ma1",
            m("div", {
                onclick: () => this.expanded = !this.expanded
                }, "Rule Viewer " + (this.expanded ? "▲" : "▼")
            ),
            !this.expanded ? [] : [
                rule ? [] : "Rule Viewer: Please select a rule",
                !rule ? [] : [
                    m("div.PanelRest.TPanel",
                        m("div.PanelButtonBar.TPanel",
                            {
                            },
                            m("button.NewRuleButton.TSpeedButton",
                                {
                                    onclick: NewRuleButtonClick,
                                    title: "Make a new rule",
                                },
                                "New",
                            ),
                            m("button.DuplicateRuleButton.TSpeedButton",
                                {
                                    onclick: DuplicateRuleButtonClick,
                                    title: "Duplicate the rule showing in the rule editor panel",
                                },
                                "Duplicate",
                            ),
                            m("button.DeleteRuleButton.TSpeedButton",
                                {
                                    onclick: DeleteRuleButtonClick,
                                    title: "Delete all selected rules",
                                },
                                "Delete",
                            ),
                            m("button.MoveUpButton.TSpeedButton",
                                {
                                    onclick: MoveUpButtonClick,
                                    title: "Raise all selected rules",
                                },
                                "Raise",
                            ),
                            m("button.MoveDownButton.TSpeedButton",
                                {
                                    onclick: MoveDownButtonClick,
                                    title: "Lower all selected rules",
                                },
                                "Lower",
                            ),
                            m("button.insertSound.TSpeedButton",
                                {
                                    onclick: InsertSoundClick,
                                    title: "Insert a sound into a reply",
                                },
                                "Sound",
                            ),
                            m("button.InsertMusicButton.TSpeedButton",
                                {
                                    onclick: InsertMusicButtonClick,
                                    title: "Insert music into a reply",
                                },
                                "Music",
                            ),
                        ),
                        m(".Context",
                            m("div.RuleNumberLabel.TLabel",
                                {
                                    title: "The index of the edited rule in the table",
                                },
                                "#" + (world.rules.indexOf(rule) + 1),
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
                                    value: rule.context.phrase
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
                                    value: rule.command.phrase
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
                                    value: rule.reply
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
                                    value: rule.move.phrase
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
                                    value: rule.decompileRequirements()
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
                            m("TListBox.ChangesListBox.TListBox",
                                {
                                },
                            ),
                            m("input.ChangesEdit.TEdit",
                                {
                                    value: rule.decompileChanges()
                                },
                            ),
                        ),
                    ),
                ]
            ]
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

        return m(".RuleEditorForm.ml3.flex.flex-column.flex-nowrap.overflow-hidden",
            { style: "height: calc(100% - 5rem)" },
            m("div.flex-none",
                m("span.b", "Rule Editor"),
                m(buttonWithHighlight("table"), { onclick: () => this.currentView = "table" }, "Table"),
                m(buttonWithHighlight("map"),  { onclick: () => this.currentView = "map" }, "Map"),
                m(buttonWithHighlight("browser"),  { onclick: () => this.currentView = "browser" }, "Browser"),
            ),
            // TODO: Probably should wrap these with hidden divs so the component state is preserved
            m("div.mt2.flex-auto.overflow-auto",
                currentView === "table" ? m(RuleTableView, <any>{domain: domain}) : [],
                currentView === "map" ? m(RuleMapView, <any>{domain: domain}) : [],
                currentView === "browser" ? m(RuleBrowserView, <any>{domain: domain}) : [],
            ),
            m("div.flex-none",
                m(IndividualRuleView, <any>{domain: domain})
            )
        )
    }
}
