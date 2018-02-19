// unit usabout

from conversion_common import *
import ufilesupport
import usdomain
import delphi_compatability

const usabout = usabout || {}

// var
let AboutForm: TAboutForm


// const
const kAsSplash = true
const kAsAbout = false



export class TAboutForm {
    OKButton: TButton = new TButton()
    Image1: TImage = new TImage()
    Label1: TLabel = new TLabel()
    Label2: TLabel = new TLabel()
    Label3: TLabel = new TLabel()
    Label4: TLabel = new TLabel()
    versionLabel: TLabel = new TLabel()
    Label6: TLabel = new TLabel()
    Label7: TLabel = new TLabel()
    Label8: TLabel = new TLabel()
    registeredToLabel: TLabel = new TLabel()
    TAboutForm.prototype = new TForm()
    TAboutForm.prototype.constructor = TAboutForm
    
    //$R *.DFM
    FormCreate(Sender: TObject): void {
        this.versionLabel.Caption = ufilesupport.gVersionName
    }
    
    OKButtonClick(Sender: TObject): void {
        this.ModalResult = mrOK
    }
    
    setUpAsSplashOrAbout(splash: boolean): void {
        if (splash) {
            this.OKButton.Visible = false
            this.Caption = ""
            this.BorderIcons = {}
            this.showNameString("")
        } else {
            this.Caption = "About StoryHarp"
            this.BorderIcons = {delphi_compatability.TBorderIcon.biSystemMenu, delphi_compatability.TBorderIcon.biMinimize, delphi_compatability.TBorderIcon.biMaximize, }
            this.OKButton.Visible = true
            this.showNameString(usdomain.domain.registrationName)
        }
    }
    
    showNameString(aName: String): void {
        if (aName === "") {
            this.registeredToLabel.Caption = ""
        } else if (usdomain.domain.playerOnly) {
            this.registeredToLabel.Caption = "Player-only mode"
        } else if (!usdomain.domain.registered) {
            this.registeredToLabel.Caption = "Unregistered"
        } else {
            if ((len(aName) > 1) && (aName[len(aName)] === ",")) {
                aName = UNRESOLVED.copy(aName, 1, len(aName) - 1)
            }
            this.registeredToLabel.Caption = "Registered to: " + aName
        }
    }
    
}

