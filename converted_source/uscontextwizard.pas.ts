// unit uscontextwizard

from conversion_common import *
import uschangelog
import uscommands
import usconsoleform
import usruleeditorform
import usworld
import usdomain
import delphi_compatability

const uscontextwizard = uscontextwizard || {}

// var
let ContextWizardForm: TContextWizardForm


// const
const kStartPage = 0
const kContextsPage = 1
const kDescriptionPage = 2
const kFinishPage = 3



export class TContextWizardForm {
    goBack: TButton = new TButton()
    goNext: TButton = new TButton()
    cancel: TButton = new TButton()
    helpButton: TButton = new TButton()
    notebook: TNotebook = new TNotebook()
    Label1: TLabel = new TLabel()
    Label2: TLabel = new TLabel()
    Label3: TLabel = new TLabel()
    Label5: TLabel = new TLabel()
    Label6: TLabel = new TLabel()
    Label21: TLabel = new TLabel()
    Label22: TLabel = new TLabel()
    Image3: TImage = new TImage()
    NewContextsMemo: TMemo = new TMemo()
    DescribeLabel: TLabel = new TLabel()
    DescribeLabelExtra: TLabel = new TLabel()
    Label4: TLabel = new TLabel()
    DescribeImage: TImage = new TImage()
    DescribeEdit: TEdit = new TEdit()
    Label13: TLabel = new TLabel()
    Label14: TLabel = new TLabel()
    Label18: TLabel = new TLabel()
    Label7: TLabel = new TLabel()
    Label9: TLabel = new TLabel()
    Image1: TImage = new TImage()
    Label10: TLabel = new TLabel()
    Label26: TLabel = new TLabel()
    Label19: TLabel = new TLabel()
    Image2: TImage = new TImage()
    Label8: TLabel = new TLabel()
    commandStartPageImage: TImage = new TImage()
    contextStartPageImage: TImage = new TImage()
    replyStartPageImage: TImage = new TImage()
    commandImage: TImage = new TImage()
    Label11: TLabel = new TLabel()
    Label12: TLabel = new TLabel()
    Label15: TLabel = new TLabel()
    Label16: TLabel = new TLabel()
    generated: boolean = false
    TContextWizardForm.prototype = new TForm()
    TContextWizardForm.prototype.constructor = TContextWizardForm
    
    //$R *.DFM
    FormActivate(Sender: TObject): void {
        this.contextStartPageImage.Picture.Bitmap = usconsoleform.ConsoleForm.ContextButton.Glyph
        this.commandStartPageImage.Picture.Bitmap = usconsoleform.ConsoleForm.CommandButton.Glyph
        this.replyStartPageImage.Picture.Bitmap = usruleeditorform.RuleEditorForm.replyPicture.Picture.Bitmap
        this.commandImage.Picture.Bitmap = usconsoleform.ConsoleForm.CommandButton.Glyph
    }
    
    initialize(): boolean {
        let result = false
        this.generated = false
        this.notebook.PageIndex = kStartPage
        this.NewContextsMemo.Text = ""
        result = true
        return result
    }
    
    goBackClick(Sender: TObject): void {
        if (this.notebook.PageIndex > kStartPage) {
            this.notebook.PageIndex = this.notebook.PageIndex - 1
        }
    }
    
    goNextClick(Sender: TObject): void {
        if (this.notebook.PageIndex === kFinishPage) {
            this.generateRules()
        } else if (this.notebook.PageIndex === kContextsPage) {
            if ((trim(this.NewContextsMemo.Text) === "")) {
                ShowMessage("You must enter one or more contexts to proceed.")
                return
            }
        } else if (this.notebook.PageIndex === kDescriptionPage) {
            if ((trim(this.DescribeEdit.Text) === "")) {
                ShowMessage("You must enter a command to be used to describe these contexts.")
                return
            }
        }
        if (this.notebook.PageIndex < kFinishPage) {
            this.notebook.PageIndex = this.notebook.PageIndex + 1
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
        let contextName: string
        let contextDescription: string
        let character: string
        let pipeRead: boolean
        let newRule: TSRule
        let position: TPoint
        let newRulesCommand: TSNewRulesCommand
        
        uschangelog.ChangeLogForm.addToLog(this.NewContextsMemo.Text)
        position = usruleeditorform.RuleEditorForm.goodPosition()
        stream = delphi_compatability.TStringStream().Create(this.NewContextsMemo.Text)
        previousContext = ""
        newRulesCommand = uscommands.TSNewRulesCommand().create()
        newRulesCommand.creator = "new context wizard"
        try {
            character = stream.ReadString(1)
            while (character !== "") {
                line = ""
                contextName = ""
                contextDescription = ""
                pipeRead = false
                while (!((character === Character(13)) || (character === Character(10)) || (character === ""))) {
                    line = line + character
                    if (pipeRead) {
                        contextDescription = contextDescription + character
                    } else if (character === "|") {
                        pipeRead = true
                    } else {
                        contextName = contextName + character
                    }
                    character = stream.ReadString(1)
                }
                if (trim(contextName) !== "") {
                    if ((trim(this.DescribeEdit.Text) !== "")) {
                        //ShowMessage('name: ' + contextName);
                        //ShowMessage('description: ' + contextDescription);
                        newRule = usdomain.domain.world.newRule()
                        newRulesCommand.addRule(newRule)
                        usruleeditorform.RuleEditorForm.lastChoice = newRule
                        newRule.position = position
                        newRule.setContext(trim(contextName))
                        newRule.setCommand(trim(this.DescribeEdit.Text))
                        if (trim(contextDescription) !== "") {
                            newRule.setReply(trim(contextDescription))
                        } else {
                            newRule.setReply("There is nothing of interest here.")
                        }
                    }
                    previousContext = trim(contextName)
                    position.Y = position.Y + 60
                }
                while ((character === Character(13)) || (character === Character(10))) {
                    character = stream.ReadString(1)
                }
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
        this.ModalResult = mrOK
        this.generated = true
    }
    
    //
    //cave|You are in a big cave.
    //forest|You are in a lively forest.
    //spring|You are standing near a burbling spring.
    //
    //
    //well house|You are in a well house for a small spring.
    //grate|You are standing above a grate.
    //forest|You are wandering around in dense forest.
    //glade|You are in a forest glade.
    //stream|You are walking along a dry stream bed.
    //
    FormCloseQuery(Sender: TObject, CanClose: boolean): void {
        let result: byte
        
        if (this.generated) {
            return CanClose
        }
        if (trim(this.NewContextsMemo.Text) !== "") {
            result = MessageDialog("You are about to close this wizard" + chr(13) + "and lose any work done with it." + chr(13) + chr(13) + "Is this OK?", mtConfirmation, {mbYes, mbNo, }, 0)
            if (result !== mrYes) {
                CanClose = false
                return CanClose
            }
            uschangelog.ChangeLogForm.addToLog(this.NewContextsMemo.Text)
        }
        return CanClose
    }
    
    helpButtonClick(Sender: TObject): void {
        delphi_compatability.Application.HelpJump("Making_new_rules_using_the_new_contexts_wizard")
    }
    
}

//with forwardEdit do
//  if enabled then color := clWindow else color := clBtnFace;
