import * as m from "mithril"
import { TWorld } from "./TWorld"
import { TSRule, TSRuleField } from "./TSRule"
import { TSCommandList } from "./TSCommandList"

type ViewName = "table" | "map" | "browser"

/* Decided not to use and to edit a rule directly
// Similar to TSRule:
interface TSRuleShadow {
    context: string
    requirements: string[]
    command: string
    reply: string
    move: string
    changes: string[]
}

ruleShadow: TSRuleShadow = {
    context: "",
    requirements: [],
    command: "",
    reply: "",
    move: "",
    changes: [],
}
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

    /*
    commitChangesToRule(): void {
        // this.RequirementsEdit.Hide()
        // this.ChangesEdit.Hide()
        if (this.rule === null) {
            return
        }
        
        if (this.ContextEdit.Text !== this.rule.context.phrase) {
            usdomain.domain.worldCommandList.ruleFieldChange(this.rule, usworld.kRuleContext, this.ContextEdit.Text)
        }
        if (this.CommandEdit.Text !== this.rule.command.phrase) {
            usdomain.domain.worldCommandList.ruleFieldChange(this.rule, usworld.kRuleCommand, this.CommandEdit.Text)
        }
        if (this.ReplyMemo.Text !== this.rule.reply) {
            usdomain.domain.worldCommandList.ruleFieldChange(this.rule, usworld.kRuleReply, this.ReplyMemo.Text)
        }
        if (this.MoveEdit.Text !== this.rule.move.phrase) {
            usdomain.domain.worldCommandList.ruleFieldChange(this.rule, usworld.kRuleMove, this.MoveEdit.Text)
        }
        if (this.logicalStatementForListBox(this.RequirementsListBox) !== this.rule.decompileRequirements()) {
            usdomain.domain.worldCommandList.ruleFieldChange(this.rule, usworld.kRuleRequirements, this.logicalStatementForListBox(this.RequirementsListBox))
        }
        if (this.logicalStatementForListBox(this.ChangesListBox) !== this.rule.decompileChanges()) {
            usdomain.domain.worldCommandList.ruleFieldChange(this.rule, usworld.kRuleChanges, this.logicalStatementForListBox(this.ChangesListBox))
        }
    }

    logicalStatementForListBox(listBox: TListBox): string {
        let result = ""
        let i: int
        
        result = ""
        for (i = 0; i <= listBox.Items.Count - 2; i++) {
            if (result !== "") {
                // use 2 because last is always blank for adding
                result = result + " & " + trim(listBox.Items[i])
            } else {
                result = trim(listBox.Items[i])
            }
        }
        return result
    }

    selectEditorField(field: int): void {
        if (this.rule === null) {
            return
        }
        switch (field) {
            case usworld.kRuleContext:
                this.ContextEdit.SetFocus()
                this.ContextEdit.SelStart = 0
                this.ContextEdit.SelLength = len(this.ContextEdit.Text)
                break
            case usworld.kRuleCommand:
                this.CommandEdit.SetFocus()
                this.CommandEdit.SelStart = 0
                this.CommandEdit.SelLength = len(this.CommandEdit.Text)
                break
            case usworld.kRuleReply:
                this.ReplyMemo.SetFocus()
                this.ReplyMemo.SelStart = 0
                this.ReplyMemo.SelLength = len(this.ReplyMemo.Text)
                break
            case usworld.kRuleMove:
                this.MoveEdit.SetFocus()
                this.MoveEdit.SelStart = 0
                this.MoveEdit.SelLength = len(this.MoveEdit.Text)
                break
            case usworld.kRuleRequirements:
                this.RequirementsListBox.SetFocus()
                if (this.RequirementsListBox.Items.Count > 0) {
                    this.RequirementsListBox.ItemIndex = 0
                }
                break
            case usworld.kRuleChanges:
                this.ChangesListBox.SetFocus()
                if (this.ChangesListBox.Items.Count > 0) {
                    this.ChangesListBox.ItemIndex = 0
                }
                break
    }
    */

    view() {
        const world: TWorld = this.domain.world
        const worldCommandList: TSCommandList = this.domain.worldCommandList
        const rule: TSRule = this.domain.editedRule

        // TODO: Fix all these
        const ruleEditorForm: any = this.domain.ruleEditorForm
        const changeLogForm: any = this.domain.changeLogForm
        const consoleForm: any = this.domain.consoleForm
        
        function SpeedButtonClick() { console.log("SpeedButtonClick") }

        function MoveDownButtonClick() { console.log("MoveDownButtonClick") }

        function MoveUpButtonClick() { console.log("MoveUpButtonClick") }

        function NewRuleButtonClick() { console.log("NewRuleButtonClick") }

        function DeleteRuleButtonClick() { console.log("DeleteRuleButtonClick") }

        function DuplicateRuleButtonClick() { console.log("DuplicateRuleButtonClick") }

        function InsertMusicButtonClick() { console.log("InsertMusicButtonClick") }

        function InsertSoundClick() { console.log("insertSoundClick") }

        function contextChange(event: { target: HTMLInputElement }) {
            worldCommandList.ruleFieldChange(ruleEditorForm, changeLogForm, consoleForm, rule, TSRuleField.kRuleContext, event.target.value)
        }

        function commandChange(event: { target: HTMLInputElement }) {
            worldCommandList.ruleFieldChange(ruleEditorForm, changeLogForm, consoleForm, rule, TSRuleField.kRuleCommand, event.target.value)
        }

        function replyChange(event: { target: HTMLTextAreaElement }) {
            worldCommandList.ruleFieldChange(ruleEditorForm, changeLogForm, consoleForm, rule, TSRuleField.kRuleReply, event.target.value)
        }

        function moveChange(event: { target: HTMLInputElement }) {
            worldCommandList.ruleFieldChange(ruleEditorForm, changeLogForm, consoleForm, rule, TSRuleField.kRuleMove, event.target.value)
        }

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
                                    value: rule.context.phrase,
                                    onchange: contextChange
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
                                    value: rule.command.phrase,
                                    onchange: commandChange
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
                                    value: rule.reply,
                                    onchange: replyChange
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
                                    value: rule.move.phrase,
                                    onchange: moveChange
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
            { style: "height: calc(100% - 7rem)" },
            m("div.flex-none",
                m("span.b", "Rule Editor"),
                m(buttonWithHighlight("table"), { onclick: () => this.currentView = "table" }, "Table"),
                m(buttonWithHighlight("map"),  { onclick: () => this.currentView = "map" }, "Map"),
                m(buttonWithHighlight("browser"),  { onclick: () => this.currentView = "browser" }, "Browser"),
            ),
            m("div.ma2",
                m("button.ml2.w4", {
                    disabled: !domain.worldCommandList.isUndoEnabled(),
                    onclick: () => domain.worldCommandList.undoLast(),
                    title: "Undo " + domain.worldCommandList.undoDescription()
                }, "Undo"),
                m("button.ml2.w4", { 
                    disabled: !domain.worldCommandList.isRedoEnabled(),
                    onclick: () => domain.worldCommandList.redoLast(),
                    title: "Redo " + domain.worldCommandList.redoDescription()
                }, "Redo"), 
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
