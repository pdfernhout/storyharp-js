// unit uslinkwizard

from conversion_common import *
import usconsoleform
import uschangelog
import uscommands
import usruleeditorform
import usdomain
import usworld
import delphi_compatability

const uslinkwizard = uslinkwizard || {}

// var
let LinkWizardForm: TLinkWizardForm


// const
const kStartPage = 0
const kContextsPage = 1
const kForwardPage = 2
const kBackwardPage = 3
const kFinishPage = 4



export class TLinkWizardForm {
    notebook: TNotebook = new TNotebook()
    Label10: TLabel = new TLabel()
    ForwardLabel: TLabel = new TLabel()
    Label13: TLabel = new TLabel()
    ForwardEdit: TEdit = new TEdit()
    forwardReplyLabel: TLabel = new TLabel()
    ForwardMemo: TMemo = new TMemo()
    forwardReplyNote: TLabel = new TLabel()
    helpButton: TButton = new TButton()
    goBack: TButton = new TButton()
    goNext: TButton = new TButton()
    cancel: TButton = new TButton()
    Label11: TLabel = new TLabel()
    BackwardLabel: TLabel = new TLabel()
    Label12: TLabel = new TLabel()
    BackwardEdit: TEdit = new TEdit()
    backwardReplyLabel: TLabel = new TLabel()
    BackwardMemo: TMemo = new TMemo()
    Image1: TImage = new TImage()
    Label4: TLabel = new TLabel()
    Label17: TLabel = new TLabel()
    Label26: TLabel = new TLabel()
    Image3: TImage = new TImage()
    Image2: TImage = new TImage()
    Image4: TImage = new TImage()
    forwardReplyArrow: TImage = new TImage()
    Image6: TImage = new TImage()
    backwardReplyArrow: TImage = new TImage()
    Label1: TLabel = new TLabel()
    backwardReplyNote: TLabel = new TLabel()
    Label18: TLabel = new TLabel()
    Label14: TLabel = new TLabel()
    Label19: TLabel = new TLabel()
    Label3: TLabel = new TLabel()
    Label8: TLabel = new TLabel()
    Label9: TLabel = new TLabel()
    Image8: TImage = new TImage()
    FirstContextBox: TListBox = new TListBox()
    SecondContextBox: TListBox = new TListBox()
    commandStartPageImage: TImage = new TImage()
    Label2: TLabel = new TLabel()
    contextStartPageImage: TImage = new TImage()
    Label5: TLabel = new TLabel()
    replyStartPageImage: TImage = new TImage()
    Label6: TLabel = new TLabel()
    Label7: TLabel = new TLabel()
    firstContextImage: TImage = new TImage()
    secondContextImage: TImage = new TImage()
    forwardCommandImage: TImage = new TImage()
    forwardReplyImage: TImage = new TImage()
    backwardCommandImage: TImage = new TImage()
    backwardReplyImage: TImage = new TImage()
    Label15: TLabel = new TLabel()
    Label16: TLabel = new TLabel()
    forwardSummary: TLabel = new TLabel()
    backwardSummary: TLabel = new TLabel()
    generated: boolean = false
    TLinkWizardForm.prototype = new TForm()
    TLinkWizardForm.prototype.constructor = TLinkWizardForm
    
    //$R *.DFM
    FormActivate(Sender: TObject): void {
        this.contextStartPageImage.Picture.Bitmap = usconsoleform.ConsoleForm.ContextButton.Glyph
        this.commandStartPageImage.Picture.Bitmap = usconsoleform.ConsoleForm.CommandButton.Glyph
        this.replyStartPageImage.Picture.Bitmap = usruleeditorform.RuleEditorForm.replyPicture.Picture.Bitmap
        this.firstContextImage.Picture.Bitmap = usconsoleform.ConsoleForm.ContextButton.Glyph
        this.secondContextImage.Picture.Bitmap = usconsoleform.ConsoleForm.ContextButton.Glyph
        this.forwardCommandImage.Picture.Bitmap = usconsoleform.ConsoleForm.CommandButton.Glyph
        this.forwardReplyImage.Picture.Bitmap = usruleeditorform.RuleEditorForm.replyPicture.Picture.Bitmap
        this.backwardCommandImage.Picture.Bitmap = usconsoleform.ConsoleForm.CommandButton.Glyph
        this.backwardReplyImage.Picture.Bitmap = usruleeditorform.RuleEditorForm.replyPicture.Picture.Bitmap
    }
    
    initialize(): boolean {
        let result = false
        let variable: TSVariable
        let i: int
        let j: int
        let contextCount: int
        
        result = false
        this.generated = false
        this.notebook.PageIndex = kStartPage
        usdomain.domain.world.addContextsToListBox(this.FirstContextBox)
        if (this.FirstContextBox.Items.Count < 2) {
            ShowMessage("You must create at least two contexts before using the link wizard.")
            return result
        }
        usdomain.domain.world.addContextsToListBox(this.SecondContextBox)
        contextCount = 0
        this.ForwardEditChange(this)
        this.BackwardEditChange(this)
        this.ForwardEdit.Text = ""
        this.ForwardMemo.Text = ""
        this.BackwardEdit.Text = ""
        this.BackwardMemo.Text = ""
        result = true
        for (i = 0; i <= usdomain.domain.world.variables.Count - 1; i++) {
            // must be done last because of exit
            variable = usworld.TSVariable(usdomain.domain.world.variables[i])
            if (variable.selected) {
                if (contextCount === 0) {
                    for (j = 0; j <= this.FirstContextBox.Items.Count - 1; j++) {
                        if (this.FirstContextBox.Items[j] === variable.phrase) {
                            this.FirstContextBox.ItemIndex = j
                            contextCount += 1
                            break
                        }
                    }
                } else {
                    for (j = 0; j <= this.SecondContextBox.Items.Count - 1; j++) {
                        if (this.SecondContextBox.Items[j] === variable.phrase) {
                            this.SecondContextBox.ItemIndex = j
                            return result
                        }
                    }
                }
            }
        }
        return result
    }
    
    goBackClick(Sender: TObject): void {
        if (this.notebook.PageIndex > kStartPage) {
            this.notebook.PageIndex = this.notebook.PageIndex - 1
        }
    }
    
    goNextClick(Sender: TObject): void {
        let first: string
        let second: string
        
        first = trim(this.firstContext())
        second = trim(this.secondContext())
        if (this.notebook.PageIndex === kFinishPage) {
            this.generateRules()
        } else if (this.notebook.PageIndex === kContextsPage) {
            if ((first === "") || (second === "")) {
                ShowMessage("Both contexts must be entered to proceed.")
                return
            }
            if (first === second) {
                ShowMessage("The two contexts must have different names.")
                return
            }
            this.ForwardLabel.Caption = first + " --> " + second
            this.BackwardLabel.Caption = second + " --> " + first
        }
        if (this.notebook.PageIndex === kForwardPage) {
            if ((trim(this.ForwardEdit.Text) === "") && (trim(this.ForwardMemo.Text) !== "")) {
                ShowMessage("You must enter a command phrase if you enter a reply.")
                return
            }
        }
        if (this.notebook.PageIndex === kBackwardPage) {
            if ((trim(this.BackwardEdit.Text) === "") && (trim(this.BackwardMemo.Text) !== "")) {
                ShowMessage("You must enter a command phrase if you enter a reply.")
                return
            }
            if ((trim(this.BackwardEdit.Text) === "") && (trim(this.ForwardEdit.Text) === "")) {
                ShowMessage("You must enter a command on this page or the previous page to generate a link.")
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
            if (trim(this.ForwardEdit.Text) === "") {
                this.forwardSummary.Caption = ""
            } else {
                this.forwardSummary.Caption = this.firstContext() + "  ---  " + trim(this.ForwardEdit.Text) + "  -->  " + this.secondContext()
            }
            if (trim(this.BackwardEdit.Text) === "") {
                this.backwardSummary.Caption = ""
            } else {
                this.backwardSummary.Caption = this.secondContext() + "  ---  " + trim(this.BackwardEdit.Text) + "  -->  " + this.firstContext()
            }
        }
    }
    
    cancelClick(Sender: TObject): void {
        this.ModalResult = mrCancel
    }
    
    makeLink(firstContext: string, secondContext: string, command: string, reply: string): TSRule {
        let result = new TSRule()
        let dx: int
        let dy: int
        
        result = null
        if (firstContext === "") {
            return result
        }
        result = usdomain.domain.world.newRule()
        usruleeditorform.RuleEditorForm.lastChoice = result
        result.setContext(firstContext)
        result.setCommand(command)
        if (trim(reply) !== "") {
            result.setReply(reply)
        } else {
            result.setReply("You " + command + ".")
        }
        result.setMove(secondContext)
        result.position.X = (result.context.position.X + result.move.position.X) / 2
        result.position.Y = (result.context.position.Y + result.move.position.Y) / 2
        dx = result.context.position.X - result.move.position.X
        dy = result.context.position.Y - result.move.position.Y
        if (abs(dy) >= abs(dx)) {
            if (dy >= 0) {
                //now determine offset
                result.position.X = result.position.X - 100
            } else {
                result.position.X = result.position.X + 100
            }
        } else {
            if (dx < 0) {
                result.position.Y = result.position.Y - 30
            } else {
                result.position.Y = result.position.Y + 30
            }
        }
        return result
    }
    
    firstContext(): string {
        let result = ""
        result = ""
        if (this.FirstContextBox.ItemIndex >= 0) {
            result = this.FirstContextBox.Items[this.FirstContextBox.ItemIndex]
        }
        return result
    }
    
    secondContext(): string {
        let result = ""
        result = ""
        if (this.SecondContextBox.ItemIndex >= 0) {
            result = this.SecondContextBox.Items[this.SecondContextBox.ItemIndex]
        }
        return result
    }
    
    generateRules(): void {
        let newRulesCommand: TSNewRulesCommand
        let newRule: TSRule
        
        uschangelog.ChangeLogForm.addToLog(this.ForwardMemo.Text)
        uschangelog.ChangeLogForm.addToLog(this.BackwardMemo.Text)
        newRulesCommand = uscommands.TSNewRulesCommand().create()
        newRulesCommand.creator = "link wizard"
        newRule = this.makeLink(trim(this.firstContext()), trim(this.secondContext()), trim(this.ForwardEdit.Text), trim(this.ForwardMemo.Text))
        if (newRule !== null) {
            newRulesCommand.addRule(newRule)
        }
        newRule = this.makeLink(trim(this.secondContext()), trim(this.firstContext()), trim(this.BackwardEdit.Text), trim(this.BackwardMemo.Text))
        if (newRule !== null) {
            newRulesCommand.addRule(newRule)
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
    
    ForwardEditChange(Sender: TObject): void {
        let haveText: boolean
        
        haveText = this.ForwardEdit.Text !== ""
        this.forwardReplyArrow.Visible = haveText
        this.forwardReplyLabel.Enabled = haveText
        this.forwardReplyImage.Visible = haveText
        this.ForwardMemo.Visible = haveText
        this.forwardReplyNote.Enabled = haveText
    }
    
    BackwardEditChange(Sender: TObject): void {
        let haveText: boolean
        
        haveText = this.BackwardEdit.Text !== ""
        this.backwardReplyArrow.Visible = haveText
        this.backwardReplyLabel.Enabled = haveText
        this.backwardReplyImage.Visible = haveText
        this.BackwardMemo.Visible = haveText
        this.backwardReplyNote.Enabled = haveText
    }
    
    FormCloseQuery(Sender: TObject, CanClose: boolean): void {
        let result: byte
        
        if (this.generated) {
            return CanClose
        }
        if ((trim(this.ForwardEdit.Text) !== "") || (trim(this.ForwardMemo.Text) !== "") || (trim(this.BackwardEdit.Text) !== "") || (trim(this.BackwardMemo.Text) !== "")) {
            result = MessageDialog("You are about to close this wizard" + chr(13) + "and lose any work done with it." + chr(13) + chr(13) + "Is this OK?", mtConfirmation, {mbYes, mbNo, }, 0)
            if (result !== mrYes) {
                CanClose = false
                return CanClose
            }
            uschangelog.ChangeLogForm.addToLog(this.ForwardMemo.Text)
            uschangelog.ChangeLogForm.addToLog(this.BackwardMemo.Text)
        }
        return CanClose
    }
    
    helpButtonClick(Sender: TObject): void {
        delphi_compatability.Application.HelpJump("Making_new_rules_using_the_new_moves_wizard")
    }
    
}

