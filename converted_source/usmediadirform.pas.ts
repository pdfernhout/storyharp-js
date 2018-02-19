// unit usmediadirform

from conversion_common import *
import usspeech
import usdomain
import delphi_compatability

const usmediadirform = usmediadirform || {}

// var
let ExtraMediaDirectoryForm: TExtraMediaDirectoryForm



export class TExtraMediaDirectoryForm {
    Close: TButton = new TButton()
    cancel: TButton = new TButton()
    helpButton: TSpeedButton = new TSpeedButton()
    Label1: TLabel = new TLabel()
    Label2: TLabel = new TLabel()
    Label3: TLabel = new TLabel()
    extraMediaDirectoryEdit: TEdit = new TEdit()
    Label4: TLabel = new TLabel()
    Label5: TLabel = new TLabel()
    Label6: TLabel = new TLabel()
    openWorldFileDirectory: TEdit = new TEdit()
    exeDirectory: TEdit = new TEdit()
    windowsMediaDirectory: TEdit = new TEdit()
    TExtraMediaDirectoryForm.prototype = new TForm()
    TExtraMediaDirectoryForm.prototype.constructor = TExtraMediaDirectoryForm
    
    //$R *.DFM
    FormActivate(Sender: TObject): void {
        this.openWorldFileDirectory.Text = usdomain.domain.worldFileName
        this.openWorldFileDirectory.Text = ExtractFilePath(usdomain.domain.worldFileName)
        if (this.openWorldFileDirectory.Text === "") {
            this.openWorldFileDirectory.Text = UNRESOLVED.GetCurrentDir + "\\"
        }
        this.extraMediaDirectoryEdit.Text = usdomain.domain.options.extraMediaDirectory
        this.exeDirectory.Text = ExtractFilePath(delphi_compatability.Application.exeName)
        this.windowsMediaDirectory.Text = usspeech.getWindowsMediaDirectory()
    }
    
    CloseClick(Sender: TObject): void {
        if (!UNRESOLVED.directoryExists(this.extraMediaDirectoryEdit.Text)) {
            ShowMessage("The directory you specified does not exist.")
            return
        }
        usdomain.domain.options.extraMediaDirectory = this.extraMediaDirectoryEdit.Text
        this.ModalResult = mrOK
    }
    
    helpButtonClick(Sender: TObject): void {
        delphi_compatability.Application.HelpJump("Setting_the_sounds_and_music_directory")
    }
    
}

