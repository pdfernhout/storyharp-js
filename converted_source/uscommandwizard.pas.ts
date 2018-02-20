// unit uscommandwizard

from conversion_common import *
import uschangelog
import uscommands
import usconsoleform
import usruleeditorform
import usworld
import usdomain
import delphi_compatability

const uscommandwizard = uscommandwizard || {}

// var
let CommandWizardForm: TCommandWizardForm


// const
const kStartPage = 0
const kContextPage = 1
const kCommandsPage = 2
const kSequencePage = 3
const kFinishPage = 4



export class TCommandWizardForm {
    notebook: TNotebook = new TNotebook()
    Label1: TLabel = new TLabel()
    Label2: TLabel = new TLabel()
    NewCommandsMemo: TMemo = new TMemo()
    GenerateSequence: TCheckBox = new TCheckBox()
    PrefixEdit: TEdit = new TEdit()
    PrefixLabel: TLabel = new TLabel()
    sequenceEndPanel: TPanel = new TPanel()
    endSequenceLabel: TLabel = new TLabel()
    endSequenceLoopToFirst: TRadioButton = new TRadioButton()
    endSequenceRemoveTheLastCommand: TRadioButton = new TRadioButton()
    endSequenceLeaveLastCommand: TRadioButton = new TRadioButton()
    helpButton: TButton = new TButton()
    goBack: TButton = new TButton()
    goNext: TButton = new TButton()
    cancel: TButton = new TButton()
    Image1: TImage = new TImage()
    Label7: TLabel = new TLabel()
    Label17: TLabel = new TLabel()
    Label26: TLabel = new TLabel()
    Image3: TImage = new TImage()
    Label8: TLabel = new TLabel()
    Label11: TLabel = new TLabel()
    Image2: TImage = new TImage()
    Label9: TLabel = new TLabel()
    Label12: TLabel = new TLabel()
    Image4: TImage = new TImage()
    Label13: TLabel = new TLabel()
    prefixArrow: TImage = new TImage()
    sequenceEndArrow: TImage = new TImage()
    prefixNote: TLabel = new TLabel()
    Label18: TLabel = new TLabel()
    Label16: TLabel = new TLabel()
    Label20: TLabel = new TLabel()
    Label3: TLabel = new TLabel()
    Image8: TImage = new TImage()
    ContextBox: TListBox = new TListBox()
    commandStartPageImage: TImage = new TImage()
    Label5: TLabel = new TLabel()
    contextStartPageImage: TImage = new TImage()
    Label6: TLabel = new TLabel()
    replyStartPageImage: TImage = new TImage()
    Label10: TLabel = new TLabel()
    contextListImage: TImage = new TImage()
    Label4: TLabel = new TLabel()
    Label14: TLabel = new TLabel()
    Label21: TLabel = new TLabel()
    Label22: TLabel = new TLabel()
    Label23: TLabel = new TLabel()
    Label19: TLabel = new TLabel()
    Label15: TLabel = new TLabel()
    requirementsStartPageImage: TImage = new TImage()
    changesStartPageImage: TImage = new TImage()
    Label24: TLabel = new TLabel()
    Label25: TLabel = new TLabel()
    Label27: TLabel = new TLabel()
    newCommandsForContextLabel: TLabel = new TLabel()
    lastFloodedContextPrefix: string = ""
    generated: boolean = false
    TCommandWizardForm.prototype = new TForm()
    TCommandWizardForm.prototype.constructor = TCommandWizardForm
    
    //$R *.DFM
    FormActivate(Sender: TObject): void {
        this.contextStartPageImage.Picture.Bitmap = usconsoleform.ConsoleForm.ContextButton.Glyph
        this.commandStartPageImage.Picture.Bitmap = usconsoleform.ConsoleForm.CommandButton.Glyph
        this.replyStartPageImage.Picture.Bitmap = usruleeditorform.RuleEditorForm.replyPicture.Picture.Bitmap
        this.requirementsStartPageImage.Picture.Bitmap = usconsoleform.ConsoleForm.RequirementsButton.Glyph
        this.changesStartPageImage.Picture.Bitmap = usconsoleform.ConsoleForm.ChangesButton.Glyph
        this.contextListImage.Picture.Bitmap = usconsoleform.ConsoleForm.ContextButton.Glyph
    }
    
    initialize(): boolean {
        let result = false
        let variable: TSVariable
        let i: int
        let j: int
        
        this.generated = false
        this.notebook.PageIndex = kStartPage
        usdomain.domain.world.addContextsToListBox(this.ContextBox)
        if (this.ContextBox.Items.Count < 1) {
            result = false
            ShowMessage("You must create at least one context before using the command wizard.")
            return result
        }
        //self.GenerateSequenceClick(self);
        this.NewCommandsMemo.Text = ""
        this.GenerateSequence.Checked = false
        this.PrefixEdit.Text = ""
        this.endSequenceLoopToFirst.Checked = true
        this.lastFloodedContextPrefix = ""
        for (i = 0; i <= usdomain.domain.world.variables.Count - 1; i++) {
            variable = usworld.TSVariable(usdomain.domain.world.variables[i])
            if (variable.selected) {
                for (j = 0; j <= this.ContextBox.Items.Count - 1; j++) {
                    if (this.ContextBox.Items[j] === variable.phrase) {
                        this.ContextBox.ItemIndex = j
                        break
                    }
                }
            }
        }
        result = true
        return result
    }
    
    goNextClick(Sender: TObject): void {
        if (this.notebook.PageIndex === kFinishPage) {
            this.generateRules()
        } else if (this.notebook.PageIndex === kContextPage) {
            if ((this.ContextBox.ItemIndex < 0)) {
                ShowMessage("You must select a context to proceed.")
                return
            }
            if ((trim(this.PrefixEdit.Text) === "") || (trim(this.PrefixEdit.Text) === this.lastFloodedContextPrefix)) {
                this.PrefixEdit.Text = this.ContextBox.Items[this.ContextBox.ItemIndex]
                this.lastFloodedContextPrefix = this.ContextBox.Items[this.ContextBox.ItemIndex]
            }
            this.newCommandsForContextLabel.Caption = "New commands for: " + this.ContextBox.Items[this.ContextBox.ItemIndex]
        } else if (this.notebook.PageIndex === kCommandsPage) {
            if ((trim(this.NewCommandsMemo.Text) === "")) {
                ShowMessage("You must enter one or more commands to proceed.")
                return
            }
        } else if (this.notebook.PageIndex === kSequencePage) {
            if (this.GenerateSequence.Checked) {
                if ((trim(this.PrefixEdit.Text) === "")) {
                    ShowMessage("You must enter a prefix to proceed.")
                    return
                }
            }
        }
        if (this.notebook.PageIndex < kFinishPage) {
            this.notebook.PageIndex = this.notebook.PageIndex + 1
        }
    }
    
    goBackClick(Sender: TObject): void {
        if (this.notebook.PageIndex > kStartPage) {
            this.notebook.PageIndex = this.notebook.PageIndex - 1
        }
    }
    
    notebookPageChanged(Sender: TObject): void {
        this.goBack.Enabled = this.notebook.PageIndex > kStartPage
        if (this.notebook.PageIndex !== kFinishPage) {
            this.goNext.Caption = "&Next >>"
        } else {
            this.goNext.Caption = "&Finish"
        }
    }
    
    cancelClick(Sender: TObject): void {
        this.ModalResult = mrCancel
    }
    
    generateRules(): void {
        let stream: TStringStream
        let line: string
        let previousContext: string
        let commandPhrase: string
        let commandResponse: string
        let character: string
        let pipeRead: boolean
        let newRule: TSRule
        let position: TPoint
        let index: int
        let newRulesCommand: TSNewRulesCommand
        let context: string
        let requirements: string
        let changes: string
        let prefix: string
        
        uschangelog.ChangeLogForm.addToLog(this.NewCommandsMemo.Text)
        position = usruleeditorform.RuleEditorForm.goodPosition()
        stream = delphi_compatability.TStringStream().Create(this.NewCommandsMemo.Text)
        previousContext = ""
        index = 1
        newRulesCommand = uscommands.TSNewRulesCommand().create()
        newRulesCommand.creator = "command sequence wizard"
        newRule = null
        try {
            character = stream.ReadString(1)
            while (character !== "") {
                line = ""
                commandPhrase = ""
                commandResponse = ""
                pipeRead = false
                prefix = trim(this.PrefixEdit.Text)
                while (!((character === Character(13)) || (character === Character(10)) || (character === ""))) {
                    line = line + character
                    if (pipeRead) {
                        commandResponse = commandResponse + character
                    } else if (character === "|") {
                        pipeRead = true
                    } else {
                        commandPhrase = commandPhrase + character
                    }
                    character = stream.ReadString(1)
                }
                //  	ShowMessage('name: ' + contextName);
                //  ShowMessage('description: ' + contextDescription);
                context = ""
                if (this.ContextBox.ItemIndex >= 0) {
                    context = this.ContextBox.Items[this.ContextBox.ItemIndex]
                }
                if (trim(commandResponse) === "") {
                    commandResponse = "Nothing happens."
                }
                if ((trim(commandPhrase) !== "") && (trim(context) !== "")) {
                    newRule = usdomain.domain.world.newRule()
                    newRulesCommand.addRule(newRule)
                    usruleeditorform.RuleEditorForm.lastChoice = newRule
                    newRule.position = position
                    newRule.setContext(trim(context))
                    newRule.setCommand(trim(commandPhrase))
                    newRule.setReply(trim(commandResponse))
                    if (this.GenerateSequence.Checked && (prefix !== "")) {
                        if (index === 1) {
                            requirements = "~" + prefix + " started"
                            changes = prefix + " started & " + prefix + " " + IntToStr(index) + "0"
                        } else {
                            requirements = prefix + " " + IntToStr(index - 1) + "0"
                            changes = "~" + prefix + " " + IntToStr(index - 1) + "0 & " + prefix + " " + IntToStr(index) + "0"
                        }
                        newRule.setRequirements(requirements)
                        newRule.setChanges(changes)
                    }
                    position.Y = position.Y + 60
                    index += 1
                }
                while ((character === Character(13)) || (character === Character(10))) {
                    character = stream.ReadString(1)
                }
            }
            if (this.GenerateSequence.Checked && (prefix !== "") && (newRule !== null) && (index > 2)) {
                // cleanup for last rule
                changes = newRule.changesString
                if (this.endSequenceLoopToFirst.Checked) {
                    changes = "~" + prefix + " " + IntToStr(index - 2) + "0 & " + "~" + prefix + " started"
                } else if (this.endSequenceLeaveLastCommand.Checked) {
                    changes = ""
                } else if (this.endSequenceRemoveTheLastCommand.Checked) {
                    changes = "~" + prefix + " " + IntToStr(index - 2) + "0"
                }
                newRule.setChanges(changes)
            }
        } finally {
            stream.free
        }
        if (newRulesCommand.rules.Count > 0) {
            usdomain.domain.worldCommandList.doCommand(newRulesCommand)
        } else {
            newRulesCommand.free
        }
        usruleeditorform.RuleEditorForm.updateForRuleChange()
        usruleeditorform.RuleEditorForm.adjustScrollBars()
        this.ModalResult = 1
        this.generated = true
    }
    
    GenerateSequenceClick(Sender: TObject): void {
        let doSequence: boolean
        
        doSequence = this.GenerateSequence.Checked
        this.prefixArrow.Visible = doSequence
        this.PrefixLabel.Enabled = doSequence
        this.PrefixEdit.Visible = doSequence
        this.prefixNote.Enabled = doSequence
        this.sequenceEndArrow.Visible = doSequence
        this.endSequenceLabel.Enabled = doSequence
        this.endSequenceLoopToFirst.Enabled = doSequence
        this.endSequenceLeaveLastCommand.Enabled = doSequence
        this.endSequenceRemoveTheLastCommand.Enabled = doSequence
    }
    
    //
    //talk to the grue|The grue won't listen.
    //talk to the grue|The grue seems to be getting very agitated.
    //talk to the grue|The grue seems about to fly into a rage.
    //talk to the grue|The grue devours you (except your bones of course).
    //grue pit
    //
    FormCloseQuery(Sender: TObject, CanClose: boolean): void {
        let result: byte
        
        if (this.generated) {
            return CanClose
        }
        if (trim(this.NewCommandsMemo.Text) !== "") {
            result = MessageDialog("You are about to close this wizard" + chr(13) + "and lose any work done with it." + chr(13) + chr(13) + "Is this OK?", mtConfirmation, {mbYes, mbNo, }, 0)
            if (result !== mrYes) {
                return CanClose
            }
            uschangelog.ChangeLogForm.addToLog(this.NewCommandsMemo.Text)
        }
        this.ModalResult = mrCancel
        return CanClose
    }
    
    helpButtonClick(Sender: TObject): void {
        delphi_compatability.Application.HelpJump("Making_new_rules_using_the_new_commands_wizard")
    }
    
}

