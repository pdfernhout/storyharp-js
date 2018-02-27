import * as m from "mithril"
import { TWorld } from "./TWorld"
import { TSRule, TSRuleField } from "./TSRule"
import { TSCommandList } from "./TSCommandList"
import { TSNewRulesCommand } from "./TSNewRulesCommand"
import { FileUtils } from "./FileUtils";

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

// TODO: POSSIBLE BUG: What happens to undo/redo for console when delete rules? Or change rule? Maybe just ignore?
// TODO: Should variables be deleted when they are no longer used by a rule?

class RuleTableView {
    domain: any

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    ruleClicked(event: MouseEvent, rule: TSRule): void {
        const world: TWorld = this.domain.world
        const index = world.rules.indexOf(rule)
        const editedRule: TSRule = this.domain.editedRule
        const lastSingleRuleIndex: number = this.domain.lastSingleRuleIndex

        const isShiftClick = event.shiftKey
        const isControlClick = event.ctrlKey
        
        // TODO: POSSIBLE BUG: What if deselect the currently edited rule using control click?
        // TODO: POSSIBLE BUG: Seems like might be an issue if first click is a shift click?
        if (isShiftClick) {
            if ((lastSingleRuleIndex >= 0) && (lastSingleRuleIndex <= world.rules.length - 1) && (lastSingleRuleIndex !== index)) {
                world.deselectAllExcept(rule)
                if (lastSingleRuleIndex < index) {
                    for (let i = lastSingleRuleIndex; i <= index; i++) {
                        world.rules[i].selected = true
                    }
                } else if (lastSingleRuleIndex > index) {
                    for (let i = lastSingleRuleIndex; i >= index; i--) {
                        world.rules[i].selected = true
                    }
                }
            }
        } else if (isControlClick) {
            rule.selected = !rule.selected
        } else {
            // just plain click
            if (!rule.selected) {
                world.deselectAllExcept(rule)
                rule.selected = true
                this.domain.lastSingleRuleIndex = index
            } else {
                // do nothing except maybe drag...
            }
        }
        if (rule.selected && (editedRule !== rule) && !isControlClick && !isShiftClick) {
            // TODO: Remove this -- odd how it is not easy to access one component from a sibling component
            // this.editRule(rule)
            this.domain.editedRule = rule
        }
    }

    view() {
        const world: TWorld = this.domain.world
        const editedRule: TSRule = this.domain.editedRule
        let row = 0

        function color(row: number): string { return (row % 2 == 0) ? ".bg-washed-green" : "." }
        function ellipsis(text: string): string { return text.length > 58 ? text.substring(0, 58) + "..." : text }
        function selected(rule: TSRule): string { return rule.selected ? (rule === editedRule ? ".ba.bw2" : ".ba.bw1") : "" }

        return m("div",
            m("table.collapse",
                m("tr",
                    m("th.w-10", "context"),
                    m("th.w-20", "requirements"),
                    m("th.w-20", "command"),
                    m("th.w-20", "reply"),
                    m("th.w-10", "move"),
                    m("th.w-20", "changes"),
                ),
                world.rules.map(rule => 
                    m("tr" + color(row++) + selected(rule),
                        {
                            onclick: (event: any) => this.ruleClicked(event, rule)
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

// TODO: Change capitalization on some method names
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

   RuleNewClick(): void {
        const world: TWorld = this.domain.world
        const rule: TSRule = this.domain.editedRule
        const worldCommandList: TSCommandList = this.domain.worldCommandList

        const newRulesCommand = new TSNewRulesCommand(world, this.domain.ruleEditorForm)
        const newRule = world.newRule()
        newRulesCommand.addRule(newRule)
        // TODO: Remove this or use it to set context
        //
        //  if ListPages.ActivePage = TabSheetTable then
        //    begin
        //
        //    end
        // 	else if ListPages.ActivePage = TabSheetBrowse then
        //    begin
        //    end
        //	else if ListPages.ActivePage = TabSheetMap then
        //    begin
        //  	if lastChoice is TSVariable then
        //    	newRule.setContext(TSVariable(lastChoice).phrase)
        //  	else if lastChoice is TSRule then
        //    	newRule.setContext(TSRule(lastChoice).context.phrase);
        //    end;
        //    
        const variable = world.firstSelectedVariable()
        if (variable !== null) {
            newRule.setContext(variable.phrase)
        } else if (rule !== null) {
            newRule.setContext(rule.context.phrase)
        }
        world.deselectAllExcept(newRule)
        newRule.selected = true
        worldCommandList.doCommand(newRulesCommand)
        this.editRule(newRule)
        /* TODO: Use this to set focus or remove it
        if (newRule.context.phrase.trim() !== "") {
            this.ActiveControl = this.CommandEdit
        } else {
            this.ActiveControl = this.ContextEdit
        }
        */
    }

    RuleDuplicateClick(): void {
        const world: TWorld = this.domain.world
        const rule: TSRule = this.domain.editedRule
        const worldCommandList: TSCommandList = this.domain.worldCommandList

        if (rule === null) {
            return
        }
        const newRulesCommand = new TSNewRulesCommand(world, this.domain.ruleEditorForm)
        newRulesCommand.creator = "duplicating"
        const newRule: TSRule = world.newRule()
        newRulesCommand.addRule(newRule)
        newRule.setContext(rule.context.phrase)
        newRule.setCommand(rule.command.phrase)
        newRule.setReply(rule.reply)
        newRule.setMove(rule.move.phrase)
        newRule.setRequirements(rule.requirementsString)
        newRule.setChanges(rule.changesString)
        world.deselectAllExcept(newRule)
        newRule.selected = true
        worldCommandList.doCommand(newRulesCommand)
        this.editRule(newRule)
    }

    RuleDeleteClick(): void {
        const rule: TSRule = this.domain.editedRule
        const worldCommandList: TSCommandList = this.domain.worldCommandList

        if ((rule !== null) && (rule.selected)) {
            this.editRule(null)
        }
        worldCommandList.deleteSelectedRules(this.domain.ruleEditorForm)
        /* TODO: Remove or implement -- used in map
        this.previousChoice = null
        this.lastChoice = null
        */
    }


    MoveDownClick() {
        const worldCommandList: TSCommandList = this.domain.worldCommandList
        const ruleEditorForm: any = this.domain.ruleEditorForm
        worldCommandList.lowerSelectedRules(ruleEditorForm)
    }

    MoveUpClick() {
        const worldCommandList: TSCommandList = this.domain.worldCommandList
        const ruleEditorForm: any = this.domain.ruleEditorForm
        worldCommandList.raiseSelectedRules(ruleEditorForm)
    }

    editRule(rule: TSRule | null): void {
        this.domain.editedRule = rule
    }

    view() {
        const world: TWorld = this.domain.world
        const worldCommandList: TSCommandList = this.domain.worldCommandList
        const rule: TSRule = this.domain.editedRule

        // TODO: Fix all these
        const ruleEditorForm: any = this.domain.ruleEditorForm
        const changeLogForm: any = this.domain.changeLogForm
        const consoleForm: any = this.domain.consoleForm
        
        function SpeedButtonClick() { console.log("SpeedButtonClick") }

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

        function requirementsChange(event: { target: HTMLInputElement }) {
            worldCommandList.ruleFieldChange(ruleEditorForm, changeLogForm, consoleForm, rule, TSRuleField.kRuleRequirements, event.target.value)
        }

        function changesChange(event: { target: HTMLInputElement }) {
            worldCommandList.ruleFieldChange(ruleEditorForm, changeLogForm, consoleForm, rule, TSRuleField.kRuleChanges, event.target.value)
        }

        return m("div.IndividualRuleView.ba.bg-light-gray.w-100.pa1",
            m("div", {
                onclick: () => this.expanded = !this.expanded
                }, "Rule Viewer " + (this.expanded ? "▲" : "▼")
            ),
            !this.expanded ? [] : [
                rule ? [] : m("div",
                    "Rule Viewer: Please select a rule or make a new one: ",
                    m("button.ml1",
                        {
                            onclick: () => this.RuleNewClick(),
                            title: "Make a new rule",
                        },
                        "New Rule",
                    ),
                ),
                !rule ? [] : [
                    m("div",
                        m("div.PanelButtonBar.TPanel.ma1",
                            {
                            },
                            m("span.dib.w2.RuleNumberLabel.TLabel",
                                {
                                    title: "The index of the edited rule in the table",
                                },
                                "#" + (world.rules.indexOf(rule) + 1),
                            ),
                            m("button.NewRuleButton.TSpeedButton.ml1",
                                {
                                    onclick: () => this.RuleNewClick(),
                                    title: "Make a new rule",
                                },
                                "New",
                            ),
                            m("button.DuplicateRuleButton.TSpeedButton.ml1",
                                {
                                    onclick: () => this.RuleDuplicateClick(),
                                    title: "Duplicate the rule showing in the rule editor panel",
                                },
                                "Duplicate",
                            ),
                            m("button.DeleteRuleButton.TSpeedButton.ml1",
                                {
                                    onclick: () => this.RuleDeleteClick(),
                                    title: "Delete all selected rules",
                                },
                                "Delete",
                            ),
                            m("button.MoveUpButton.TSpeedButton.ml1",
                                {
                                    onclick: () => this.MoveUpClick(),
                                    title: "Raise all selected rules",
                                },
                                "Raise",
                            ),
                            m("button.MoveDownButton.TSpeedButton.ml1",
                                {
                                    onclick: () => this.MoveDownClick(),
                                    title: "Lower all selected rules",
                                },
                                "Lower",
                            ),
                            m("button.insertSound.TSpeedButton.ml1",
                                {
                                    onclick: InsertSoundClick,
                                    title: "Insert a sound into a reply",
                                },
                                "Sound",
                            ),
                            m("button.InsertMusicButton.TSpeedButton.ml1",
                                {
                                    onclick: InsertMusicButtonClick,
                                    title: "Insert music into a reply",
                                },
                                "Music",
                            ),
                        ),
                        m(".Context.mt1",
                            m("button.ContextSpeedButton.TSpeedButton.w4.mr1",
                                {
                                    onclick: SpeedButtonClick,
                                    title: "Browse all rules with this context",
                                },
                                "Context",
                            ),
                            m("input.ContextEdit.TEdit",
                                {
                                    style: { width: "35rem", },
                                    value: rule.context.phrase,
                                    onchange: contextChange
                                },
                            ),
                        ),
                        m(".Requirements.mt1",
                            m("button.RequirementsSpeedButton.TSpeedButton.w4.mr1",
                                {
                                    onclick: SpeedButtonClick,
                                    title: "Browse all rules with the selected requirement",
                                },
                                "Requirements",
                            ),
                            /*
                            m("TListBox.RequirementsListBox.TListBox",
                                {
                                },
                            ),
                            */
                            m("input.RequirementsEdit.TEdit",
                                {
                                    style: { width: "35rem", },
                                    value: rule.decompileRequirements(),
                                    onchange: requirementsChange
                                },
                            ),
                        ),
                        m(".Command.mt1",
                            m("button.CommandSpeedButton.TSpeedButton.w4.mr1",
                                {
                                    onclick: SpeedButtonClick,
                                    title: "Browse all rules with this command",
                                },
                                "Command",
                            ),
                            m("input.CommandEdit.TEdit",
                                {
                                    style: { width: "35rem", },
                                    value: rule.command.phrase,
                                    onchange: commandChange
                                },
                            ),
                        ),
                        m(".Reply.flex.items-center.mt1",
                            /*
                            m("img.replyPicture.TImage",
                                {
                                    title: "Test saying the reply",
                                },
                            ),
                            */
                            m("button.Label5.TLabel.w4.mr1",
                                {
                                },
                                "Reply",
                            ),
                            m("textarea.ReplyMemo.TMemo",
                                {
                                    style: {
                                        width: "35rem",
                                        height: "5em",
                                    },
                                    value: rule.reply,
                                    onchange: replyChange
                                },
                            ),
                        ),
                        m(".Move.mt1",
                            m("button.MoveSpeedButton.TSpeedButton.w4.mr1",
                                {
                                    onclick: SpeedButtonClick,
                                    title: "Browse all rules with this move",
                                },
                                "Move",
                            ),
                            m("input.MoveEdit.TEdit",
                                {
                                    style: { width: "35rem", },
                                    value: rule.move.phrase,
                                    onchange: moveChange
                                },
                            ),
                        ),
                    ),
                    m(".Changes.mt1",
                        m("button.ChangesSpeedButton.TSpeedButton.w4.mr1",
                            {
                                onclick: SpeedButtonClick,
                                title: "Browse all rules with the selected change",
                            },
                            "Changes",
                        ),
                        /*
                        m("TListBox.ChangesListBox.TListBox",
                            {
                            },
                        ),
                        */
                        m("input.ChangesEdit.TEdit",
                            {
                                style: { width: "35rem", },
                                value: rule.decompileChanges(),
                                onchange: changesChange,
                            },
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

    load() {
        const world: TWorld = this.domain.world
        FileUtils.loadFromFile(false, (fileName: string, contents: string) => {
            console.log("chose", fileName)
            world.resetVariablesAndRules()
            const loaded = world.loadWorldFromFileContents(contents)
            console.log("load status", loaded)
            this.domain.loadedFileName = fileName
            this.domain.world.newSession()
            this.domain.sessionCommandList.clear()
            this.domain.worldCommandList.clear()
            this.domain.editedRule = null
            this.domain.lastSingleRuleIndex = 0
            m.redraw()
        })
    }

    save() {
        const world: TWorld = this.domain.world
        const fileName = this.domain.loadedFileName
        FileUtils.saveToFile(fileName, world.saveWorldToFileContents(false), ".wld", (fileName: string) => {
            console.log("written", fileName)
            this.domain.loadedFileName = fileName
            m.redraw()
        })
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
                m("button.ml4.w3", {
                    disabled: !domain.worldCommandList.isUndoEnabled(),
                    onclick: () => domain.worldCommandList.undoLast(),
                    title: "Undo " + domain.worldCommandList.undoDescription()
                }, "Undo"),
                m("button.ml1.w3", { 
                    disabled: !domain.worldCommandList.isRedoEnabled(),
                    onclick: () => domain.worldCommandList.redoLast(),
                    title: "Redo " + domain.worldCommandList.redoDescription()
                }, "Redo"), 
                m("button.ml4.w3", { onclick: () => this.save() }, "Save"),
                m("button.ml1.w3", { onclick: () => this.load() }, "Load"),
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
