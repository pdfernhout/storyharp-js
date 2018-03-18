import * as m from "mithril"
import { expander } from "./common"
import { TWorld } from "./TWorld"
import { TSRule, TSRuleField } from "./TSRule"
import { TSCommandList } from "./TSCommandList"
import { TSNewRulesCommand } from "./TSNewRulesCommand"
import { TSDomain } from "./TSDomain"
import { TQuickFillComboBox } from "./TQuickFillComboBox"
import { TSDesiredStateVariableWrapper } from "./TSDesiredStateVariableWrapper";
import { Glyph } from "./VariablesView"
import { TSVariableState, TSVariable } from "./TSVariable";
import { TSLogicListBox } from "./TSLogicListBox";
import { RuleEditorForm } from "./RuleEditorForm";

// const
const kPlaySoundMacroStart = "sound "
const kPlayMusicMacroStart = "music "
const kShowPictureMacroStart = "picture "

function insertTextAtCursor(element: HTMLTextAreaElement | HTMLInputElement, textToInsert: string) {
    let newText
    if (element.selectionStart || element.selectionStart === 0) {
        var startPos = element.selectionStart
        var endPos = element.selectionEnd
        newText = element.value.substring(0, startPos) + textToInsert + element.value.substring(endPos, element.value.length)
        element.value = newText
        element.selectionStart = startPos + textToInsert.length
        element.selectionEnd = startPos + textToInsert.length
    } else {
        newText = element.value + textToInsert
        element.value = newText
    }
    return newText
}

// TODO: Change capitalization on some method names
export class IndividualRuleView {
    domain: TSDomain
    ruleEditorForm: RuleEditorForm
    expanded = true

    replyTextArea: HTMLTextAreaElement

    constructor(vnode: m.Vnode) {
        this.domain = (<any>vnode.attrs).domain
        this.ruleEditorForm = (<any>vnode.attrs).ruleEditorForm
    }

    /*

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
        this.domain.editRule(newRule)
        /* TODO: Use this to set focus or remove it
        if (newRule.context.phrase.trim() !== "") {
            this.ActiveControl = this.CommandEdit
        } else {
            this.ActiveControl = this.ContextEdit
        }
        */
        this.expanded = true
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
        this.domain.editRule(newRule)
    }

    RuleDeleteClick(): void {
        const rule: TSRule | null = this.domain.editedRule
        const worldCommandList: TSCommandList = this.domain.worldCommandList

        if ((rule !== null) && (rule.selected)) {
            this.domain.editRule(null)
        }
        worldCommandList.deleteSelectedRules()
        this.domain.ruleEditorForm.previousChoice = null
        this.domain.ruleEditorForm.lastChoice = null
    }


    MoveDownClick() {
        const worldCommandList: TSCommandList = this.domain.worldCommandList
        worldCommandList.lowerSelectedRules()
    }

    MoveUpClick() {
        const worldCommandList: TSCommandList = this.domain.worldCommandList
        worldCommandList.raiseSelectedRules()
    }

    SpeedButtonClick(field: TSRuleField, selectedVariable: TSVariable | null = null) {
        this.domain.currentEditorView = "browser"
        this.domain.setOrganizeByField(field, selectedVariable)
    }

    insertSoundClick(): void {
        const url = prompt("Sound file URL?")
        if (!url) return

        const textToInsert = " {" + kPlaySoundMacroStart + url + "} "

        const rule: TSRule | null = this.domain.editedRule
        if (rule) {
            const newText = insertTextAtCursor(this.replyTextArea, textToInsert)
            rule.setReply(newText)
        }
    }

    insertMusicClick(): void {
        const url = prompt("Music file URL?")
        if (!url) return

        const textToInsert = " {" + kPlayMusicMacroStart + url + "} "

        const rule: TSRule | null = this.domain.editedRule
        if (rule) {
            const newText = insertTextAtCursor(this.replyTextArea, textToInsert)
            rule.setReply(newText)
        }
    }

    insertImageClick(): void {
        const url = prompt("Image file URL?")
        if (!url) return

        const textToInsert = " {" + kShowPictureMacroStart + url + "} "

        const rule: TSRule | null = this.domain.editedRule
        if (rule) {
            const newText = insertTextAtCursor(this.replyTextArea, textToInsert)
            rule.setReply(newText)
        }
    }
    
    /* TODO: use or remove -- testing reply with sound

    MenuRuleTestReplyClick(Sender: TObject): void {
        let oldSpeak: boolean
        let oldPlayMusic: boolean
        let oldPlaySounds: boolean
        
        this.commitChangesToRule()
        usconsoleform.ConsoleForm.speechSystem.haltSpeechAndSound()
        if (this.rule === null) {
            return
        }
        // temporarily turn on these options to test the reply
        oldSpeak = usdomain.domain.options.playerSpeak
        oldPlaySounds = usdomain.domain.options.playerPlaySounds
        oldPlayMusic = usdomain.domain.options.playerPlayMusic
        usdomain.domain.options.playerSpeak = true
        usdomain.domain.options.playerPlaySounds = true
        usdomain.domain.options.playerPlayMusic = true
        try {
            usconsoleform.ConsoleForm.speechSystem.sayTextWithMacros(this.rule.reply)
        } finally {
            usdomain.domain.options.playerSpeak = oldSpeak
            usdomain.domain.options.playerPlaySounds = oldPlaySounds
            usdomain.domain.options.playerPlayMusic = oldPlayMusic
        }
    }
    */

    /* TODO: use or remove -- more fine grained tracking of editing reply -- maybe timed autosave?
    ReplyMemoMouseUp(Sender: TObject, Button: TMouseButton, Shift: TShiftState, X: int, Y: int): void {
        // more fine grained tracking of changes to this field...
        this.commitChangesToRule()
    }
    */

    view() {
        const world: TWorld = this.domain.world
        const worldCommandList: TSCommandList = this.domain.worldCommandList
        const rule: TSRule | null = this.domain.editedRule

        const requirements: TSDesiredStateVariableWrapper[] = []
        const changes: TSDesiredStateVariableWrapper[] = []
        if (rule) {
            // Make copies
            rule.compile(rule.decompile(rule.requirements), requirements)
            rule.compile(rule.decompile(rule.changes), changes)
        }

        function InsertMusicButtonClick() { console.log("InsertMusicButtonClick") }

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

        function requirementsChange(newRequirements: TSDesiredStateVariableWrapper[]) {
            if (!rule) throw new Error("Rule must be defined first")
            const value = rule.decompile(newRequirements)
            worldCommandList.ruleFieldChange(rule, TSRuleField.kRuleRequirements, value)
        }

        function changesChange(newChanges: TSDesiredStateVariableWrapper[]) {
            if (!rule) throw new Error("Rule must be defined first")
            const value = rule.decompile(newChanges)
            worldCommandList.ruleFieldChange(rule, TSRuleField.kRuleChanges, value)
        }

        function testReply() {
            if (!rule) throw new Error("Rule must be defined first")
            const text = rule.reply
            // TODO: Make this use the speech system if one is available
            alert(text)
        }

        return m(".IndividualRuleView.ba.bg-light-gray.w-100.pa1",
            m("div.PanelButtonBar.TPanel.ma1",
                {
                },
                m("span", {
                    onclick: () => this.expanded = !this.expanded
                    }, "Rule Viewer " + expander(this.expanded)
                ),
                m("span.dib.w2.RuleNumberLabel.TLabel",
                    {
                        title: "The index of the edited rule in the table",
                    },
                    rule ? "#" + (world.rules.indexOf(rule) + 1) : "",
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
                        disabled: !rule,
                        title: "Duplicate the rule showing in the rule editor panel",
                    },
                    "Duplicate",
                ),
                m("button.DeleteRuleButton.TSpeedButton.ml1",
                    {
                        onclick: () => this.RuleDeleteClick(),
                        disabled: !rule,
                        title: "Delete all selected rules",
                    },
                    "Delete",
                ),
                m("button.MoveUpButton.TSpeedButton.ml1",
                    {
                        onclick: () => this.MoveUpClick(),
                        disabled: !rule,
                        title: "Raise all selected rules",
                    },
                    "Raise",
                ),
                m("button.MoveDownButton.TSpeedButton.ml1",
                    {
                        onclick: () => this.MoveDownClick(),
                        disabled: !rule,
                        title: "Lower all selected rules",
                    },
                    "Lower",
                ),
                (this.expanded && this.domain.editedRule) ? [
                    m("button.insertSound.TSpeedButton.ml1",
                        {
                            onclick: () => this.insertSoundClick(),
                            disabled: !rule,
                            title: "Insert a sound into a reply",
                        },
                        "+Sound",
                    ),
                    m("button.InsertMusicButton.TSpeedButton.ml1",
                        {
                            onclick: () => this.insertMusicClick(),
                            disabled: !rule,
                            title: "Insert music into a reply",
                        },
                        "+Music",
                    ),
                    m("button.InsertImageButton.TSpeedButton.ml1",
                        {
                            onclick: () => this.insertImageClick(),
                            disabled: !rule,
                            title: "Insert picture image into a reply",
                        },
                        "+Picture",
                    ),
                ] : [],
                m("button.ml4.w3", {
                    onclick: () => this.ruleEditorForm.search(),
                    title: "Search for a rule containing some text"
                }, "Search"),
            ),
            !this.expanded ? [] : !rule ? ["Please select a rule to edit it"] : [
                m("div.Rule",
                    m(".Context.mt1",
                        m("button.ContextSpeedButton.pt1.w-10rem.w4.mr1",
                            {
                                onclick: () => this.SpeedButtonClick(TSRuleField.kRuleContext),
                                title: "Browse all rules with this context",
                            },
                            Glyph.context + " Context",
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
                        m("button.RequirementsSpeedButton.pt1.w-10rem.mr1.v-top",
                            {
                                onclick: () => this.SpeedButtonClick(TSRuleField.kRuleRequirements),
                                title: "Browse all rules with the selected requirement",
                            },
                            Glyph.requirements + " Requirements",
                        ),
                        m(TSLogicListBox, {
                            style: { width: "35rem", },
                            selections: requirements,
                            items: world.getVariableNames(),
                            world: world,
                            onchange: requirementsChange,
                            onselect: (variable: TSVariable) => {
                                this.SpeedButtonClick(TSRuleField.kRuleRequirements, variable)
                            }
                        })
                    ),
                    m(".Command.mt1",
                        m("button.CommandSpeedButton.pt1.w-10rem.mr1",
                            {
                                onclick: () => this.SpeedButtonClick(TSRuleField.kRuleCommand),
                                title: "Browse all rules with this command",
                            },
                            Glyph.command + " Command",
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
                        m("button.ReplySpeedButton.pt1.w-10rem.mr1",
                            {   
                                onclick: testReply,
                                title: "Test saying the reply",
                            },
                            Glyph.reply + " Reply",
                        ),
                        m("textarea.ReplyMemo.TMemo",
                            {
                                style: {
                                    width: "35rem",
                                    height: "5em",
                                },
                                oncreate: (vnode: any) => {
                                    this.replyTextArea = <HTMLTextAreaElement>(vnode.dom)
                                },
                                value: rule.reply,
                                onchange: replyChange
                            },
                        ),
                    ),
                    m(".Move.mt1",
                        m("button.MoveSpeedButton.pt1.w-10rem.mr1",
                            {
                                onclick: () => this.SpeedButtonClick(TSRuleField.kRuleMove),
                                title: "Browse all rules with this move",
                            },
                            Glyph.move + " Move",
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
                    m("button.ChangesSpeedButton.pt1.w-10rem.mr1.v-top",
                        {
                            onclick: () => this.SpeedButtonClick(TSRuleField.kRuleChanges),
                            title: "Browse all rules with the selected change",
                        },
                        Glyph.changes + " Changes",
                    ),
                    m(TSLogicListBox, {
                        style: { width: "35rem", },
                        selections: changes,
                        items: world.getVariableNames(),
                        world: world,
                        onchange: changesChange,
                        onselect: (variable: TSVariable) => {
                            this.SpeedButtonClick(TSRuleField.kRuleChanges, variable)
                        },
                    })
                ),
            ]
        )
    }
}
