import * as m from "mithril"
import { expander } from "./common"
import { TWorld } from "./TWorld"
import { TSRule, TSRuleField } from "./TSRule"
import { TSCommandList } from "./TSCommandList"
import { TSNewRulesCommand } from "./TSNewRulesCommand"
import { TSDomain } from "./TSDomain"
import { TQuickFillComboBox } from "./TQuickFillComboBox"

// TODO: Change capitalization on some method names
export class IndividualRuleView {
    domain: TSDomain
    expanded = true

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
    }

    /*

    fillListBox(listBox: TListBox, list: TList): void {
        let i: int
        let wrapper: TSDesiredStateVariableWrapper
        
        // OK for requirements because is parent class
        listBox.Items.Clear()
        if (this.rule !== null) {
            for (i = 0; i <= list.Count - 1; i++) {
                wrapper = list[i]
                listBox.Items.AddObject(wrapper.displayString(), wrapper)
            }
            listBox.Items.Add("")
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
        const rule: TSRule | null = this.domain.editedRule
        const worldCommandList: TSCommandList = this.domain.worldCommandList

        const newRulesCommand = new TSNewRulesCommand(this.domain)
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
        //    	newRule.setContext(TSRule(lastChoice).context.phrase)
        //    end
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
        const rule: TSRule | null = this.domain.editedRule
        const worldCommandList: TSCommandList = this.domain.worldCommandList

        if (rule === null) {
            return
        }
        const newRulesCommand = new TSNewRulesCommand(this.domain)
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
        const rule: TSRule | null = this.domain.editedRule
        const worldCommandList: TSCommandList = this.domain.worldCommandList

        if ((rule !== null) && (rule.selected)) {
            this.editRule(null)
        }
        worldCommandList.deleteSelectedRules()
        /* TODO: Remove or implement -- used in map
        this.previousChoice = null
        this.lastChoice = null
        */
    }


    MoveDownClick() {
        const worldCommandList: TSCommandList = this.domain.worldCommandList
        const ruleEditorForm: any = this.domain.ruleEditorForm
        worldCommandList.lowerSelectedRules()
    }

    MoveUpClick() {
        const worldCommandList: TSCommandList = this.domain.worldCommandList
        const ruleEditorForm: any = this.domain.ruleEditorForm
        worldCommandList.raiseSelectedRules()
    }

    editRule(rule: TSRule | null): void {
        this.domain.editedRule = rule
    }

    SpeedButtonClick(field: TSRuleField) {
        this.domain.currentEditorView = "browser"
        this.domain.setOrganizeByField(field)
    }

    view() {
        const world: TWorld = this.domain.world
        const worldCommandList: TSCommandList = this.domain.worldCommandList
        const rule: TSRule | null = this.domain.editedRule

        // TODO: Fix all these
        const ruleEditorForm: any = this.domain.ruleEditorForm
        const changeLogForm: any = this.domain.changeLogForm
        const consoleForm: any = this.domain.consoleForm

        function InsertMusicButtonClick() { console.log("InsertMusicButtonClick") }

        function InsertSoundClick() { console.log("insertSoundClick") }

        function contextChange(event: { target: HTMLInputElement }) {
            if (!rule) throw new Error("Rule must be defined first")
            worldCommandList.ruleFieldChange(rule, TSRuleField.kRuleContext, event.target.value)
        }

        function commandChange(event: { target: HTMLInputElement }) {
            if (!rule) throw new Error("Rule must be defined first")
            worldCommandList.ruleFieldChange(rule, TSRuleField.kRuleCommand, event.target.value)
        }

        function replyChange(event: { target: HTMLTextAreaElement }) {
            if (!rule) throw new Error("Rule must be defined first")
            worldCommandList.ruleFieldChange(rule, TSRuleField.kRuleReply, event.target.value)
        }

        function moveChange(event: { target: HTMLInputElement }) {
            if (!rule) throw new Error("Rule must be defined first")
            worldCommandList.ruleFieldChange(rule, TSRuleField.kRuleMove, event.target.value)
        }

        function requirementsChange(event: { target: HTMLInputElement }) {
            if (!rule) throw new Error("Rule must be defined first")
            worldCommandList.ruleFieldChange(rule, TSRuleField.kRuleRequirements, event.target.value)
        }

        function changesChange(event: { target: HTMLInputElement }) {
            if (!rule) throw new Error("Rule must be defined first")
            worldCommandList.ruleFieldChange(rule, TSRuleField.kRuleChanges, event.target.value)
        }

        return m(".IndividualRuleView.ba.bg-light-gray.w-100.pa1",
            m("div", {
                onclick: () => this.expanded = !this.expanded
                }, "Rule Viewer " + expander(this.expanded)
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
                                    onclick: () => this.SpeedButtonClick(TSRuleField.kRuleContext),
                                    title: "Browse all rules with this context",
                                },
                                "Context",
                            ),
                            m(TQuickFillComboBox,
                                <any>{
                                    extraStyling: ".ContextEdit",
                                    style: { width: "35rem", },
                                    value: rule.context.phrase,
                                    onchange: contextChange,
                                    items: this.domain.world.getContextNames(),
                                },
                            ),
                        ),
                        m(".Requirements.mt1",
                            m("button.RequirementsSpeedButton.TSpeedButton.w4.mr1",
                                {
                                    onclick: () => this.SpeedButtonClick(TSRuleField.kRuleRequirements),
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
                                    onclick: () => this.SpeedButtonClick(TSRuleField.kRuleCommand),
                                    title: "Browse all rules with this command",
                                },
                                "Command",
                            ),
                            m(TQuickFillComboBox,
                                <any>{
                                    extraStyling: ".CommandEdit",
                                    style: { width: "35rem", },
                                    value: rule.command.phrase,
                                    onchange: commandChange,
                                    items: this.domain.world.getCommandNames(),
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
                                    onclick: () => this.SpeedButtonClick(TSRuleField.kRuleMove),
                                    title: "Browse all rules with this move",
                                },
                                "Move",
                            ),
                            m(TQuickFillComboBox,
                                <any>{
                                    extraStyling: ".MoveEdit",
                                    style: { width: "35rem", },
                                    value: rule.move.phrase,
                                    onchange: moveChange,
                                    items: this.domain.world.getContextNames(),
                                },
                            ),
                        ),
                    ),
                    m(".Changes.mt1",
                        m("button.ChangesSpeedButton.TSpeedButton.w4.mr1",
                            {
                                onclick: () => this.SpeedButtonClick(TSRuleField.kRuleChanges),
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
