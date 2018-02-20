// unit usregistrationform

from conversion_common import *
import usconsoleform
import delphi_compatability

const usregistrationform = usregistrationform || {}

// var
let RegistrationForm: TRegistrationForm


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// Encode the data to be sent to the CGI                                     
function Encode(msg: string): string {
    let result = ""
    let I: int
    
    result = ""
    for (I = 1; I <= len(msg); I++) {
        if (msg[I] === " ") {
            result = result + "+"
        } else if (msg[I] in {range("a", "z" + 1), range("A", "Z" + 1), }) {
            result = result + "%" + UNRESOLVED.IntToHex(ord(msg[I]), 2)
        } else {
            result = result + msg[I]
        }
    }
    return result
}


export class TRegistrationForm {
    UserNameEdit: TEdit = new TEdit()
    RegistrationCodeEdit: TEdit = new TEdit()
    Label1: TLabel = new TLabel()
    Label2: TLabel = new TLabel()
    OKButton: TButton = new TButton()
    CancelButton: TButton = new TButton()
    DisplayMemo: TMemo = new TMemo()
    DateInstalledLabel: TLabel = new TLabel()
    DaysSinceInstallationLabel: TLabel = new TLabel()
    EnableEditorButton: TButton = new TButton()
    OnlineRegistrationButton: TButton = new TButton()
    HttpClient: THttpCli = new THttpCli()
    AbortButton: TButton = new TButton()
    TRegistrationForm.prototype = new TForm()
    TRegistrationForm.prototype.constructor = TRegistrationForm
    
    //$R *.DFM
    OKButtonClick(Sender: TObject): void {
        if (len(trim(this.UserNameEdit.Text)) < 3) {
            ShowMessage("A name must be entered")
            return
        }
        if ((lowercase(trim(this.RegistrationCodeEdit.Text))) !== "htya9513") {
            ShowMessage("Invalid registration code. Please see http://www.kurtz-fernhout.com for instructions on how to register.")
            return
        }
        ShowMessage("Thank you for supporting our work.")
        usconsoleform.ConsoleForm.registrationOK(trim(this.UserNameEdit.Text))
        this.ModalResult = 1
    }
    
    CancelButtonClick(Sender: TObject): void {
        this.ModalResult = mrCancel
    }
    
    UserNameEditKeyPress(Sender: TObject, Key: char): void {
        //	OkButton.enabled := (UserNameEdit.Text <> '') and (RegistrationCodeEdit.Text <> '');
        return Key
    }
    
    RegistrationCodeEditKeyPress(Sender: TObject, Key: char): void {
        //	OkButton.enabled := (UserNameEdit.Text <> '') and (RegistrationCodeEdit.Text <> '');
        return Key
    }
    
    FormActivate(Sender: TObject): void {
        this.DateInstalledLabel.Caption = "Date editor enabled: " + UNRESOLVED.DateToStr(UNRESOLVED.Date)
        this.DaysSinceInstallationLabel.Caption = "Days since editor enabled: " + "1"
        if (this.DateInstalledLabel.Visible) {
            this.EnableEditorButton.Caption = "Disable Editor"
        } else {
            this.EnableEditorButton.Caption = "Enable Editor"
        }
    }
    
    EnableEditorButtonClick(Sender: TObject): void {
        if (this.DateInstalledLabel.Visible) {
            this.DateInstalledLabel.Visible = false
            this.DaysSinceInstallationLabel.Visible = false
            usconsoleform.ConsoleForm.MenuEditStory.enabled = false
            usconsoleform.ConsoleForm.MenuOptionsShowVariables.enabled = false
            usconsoleform.ConsoleForm.MenuOptionsUpdateEditorSelections.enabled = false
            this.ModalResult = mrCancel
        } else {
            this.DateInstalledLabel.Visible = true
            this.DaysSinceInstallationLabel.Visible = true
            usconsoleform.ConsoleForm.MenuEditStory.enabled = true
            usconsoleform.ConsoleForm.MenuOptionsShowVariables.enabled = true
            usconsoleform.ConsoleForm.MenuOptionsUpdateEditorSelections.enabled = true
            ShowMessage("The editor is now enabled." + chr(13) + "If you find value in it, please register." + chr(13) + "You must register to legally distribute or publically present" + chr(13) + "anything produced directly or indirectly through using the editor." + chr(13) + "See the license for details.")
            this.ModalResult = mrCancel
        }
    }
    
    OnlineRegistrationButtonClick(Sender: TObject): void {
        let DataIn: TMemoryStream
        let DataOut: TMemoryStream
        let Buf: string
        
        if (trim(this.UserNameEdit.Text) === "") {
            ShowMessage("You must enter your name first")
            return
        }
        this.DisplayMemo.Text = "Connecting to http://www.kurtz-fernhout.com"
        DataIn = delphi_compatability.TMemoryStream.create
        DataOut = delphi_compatability.TMemoryStream.create
        try {
            Buf = "N=" + Encode(this.UserNameEdit.Text)
            DataOut.write(Buf[1], len(Buf))
            DataOut.Seek(0, delphi_compatability.soFromBeginning)
            this.HttpClient.SendStream = DataOut
            this.HttpClient.RcvdStream = DataIn
            // none for now - not sure what this is
            this.HttpClient.Proxy = ""
            this.HttpClient.ProxyPort = "80"
            this.HttpClient.URL = "http://www.kurtz-fernhout.com/qny147/register.cgi"
            this.OnlineRegistrationButton.Enabled = false
            this.AbortButton.Enabled = true
            try {
                try {
                    this.HttpClient.post
                    DataIn.Seek(0, 0)
                    this.DisplayMemo.Lines.LoadFromStream(DataIn)
                } catch (Exception e) {
                    this.DisplayMemo.Lines.Add("Failed : " + this.HttpClient.ReasonPhrase)
                }
            } finally {
                this.OnlineRegistrationButton.Enabled = true
                this.AbortButton.Enabled = false
            }
        } finally {
            DataOut.free
            DataIn.free
        }
    }
    
}

