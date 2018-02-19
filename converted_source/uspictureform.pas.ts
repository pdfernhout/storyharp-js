// unit uspictureform

from conversion_common import *
import ucursor
import usdomain
import delphi_compatability

const uspictureform = uspictureform || {}

// var
let PictureForm: TPictureForm


//$R *.DFM
// Returns type: int
function localIntMin(a: int, b: int) {
    let result = 0
    result = a
    if (b < a) {
        result = b
    }
    return result
}

// Returns type: int
function localIntMax(a: int, b: int) {
    let result = 0
    result = a
    if (b > a) {
        result = b
    }
    return result
}


export class TPictureForm {
    controlsPanel: TPanel = new TPanel()
    FirstPictureButton: TSpeedButton = new TSpeedButton()
    PreviousPictureButton: TSpeedButton = new TSpeedButton()
    NextPictureButton: TSpeedButton = new TSpeedButton()
    LastPictureButton: TSpeedButton = new TSpeedButton()
    numbersLabel: TLabel = new TLabel()
    PictureScrollBox: TScrollBox = new TScrollBox()
    PictureImage: TImage = new TImage()
    CaptionRichEdit: TRichEdit = new TRichEdit()
    pictureNames: TStringList = new TStringList()
    commands: TStringList = new TStringList()
    replies: TStringList = new TStringList()
    selectedPictureIndex: int = 0
    TPictureForm.prototype = new TForm()
    TPictureForm.prototype.constructor = TPictureForm
    
    FormCreate(Sender: TObject): void {
        this.pictureNames = delphi_compatability.TStringList.create
        this.commands = delphi_compatability.TStringList.create
        this.replies = delphi_compatability.TStringList.create
        usdomain.domain.setFormSize(this, usdomain.domain.options.pictureWindowRect)
        this.numbersLabel.Caption = ""
        this.controlsPanel.BevelOuter = UNRESOLVED.bvNone
    }
    
    FormDestroy(Sender: TObject): void {
        this.pictureNames.free
        this.pictureNames = null
        this.commands.free
        this.commands = null
        this.replies.free
        this.replies = null
    }
    
    updateViews(): void {
        this.Caption = "StoryHarp Pictures - " + ExtractFileName(usdomain.domain.worldFileName)
    }
    
    addPictureFromFile(aFileName: String, reply: String): void {
        if (!usdomain.domain.options.showPictures) {
            return
        }
        if (this.pictureNames.IndexOf(aFileName) < 0) {
            // assumes file existence has been verified by caller (speech system)
            this.pictureNames.Add(aFileName)
            //commands.add(command);
            this.replies.Add(reply)
            this.selectedPictureIndex = this.pictureNames.Count - 1
            this.loadSelectedPicture()
        } else {
            this.selectedPictureIndex = this.pictureNames.IndexOf(aFileName)
            this.loadSelectedPicture()
        }
    }
    
    loadSelectedPicture(): void {
        let fileName: String
        
        fileName = this.pictureNames.Strings[this.selectedPictureIndex]
        try {
            ucursor.cursor_startWait()
            try {
                this.PictureImage.Picture.Bitmap.LoadFromFile(fileName)
            } catch (Exception e) {
                ShowMessage("Picture file " + fileName + " not found or could not load.")
                ucursor.cursor_stopWait()
                return
            }
        } finally {
            ucursor.cursor_stopWait()
        }
        this.PictureImage.SetBounds(localIntMax(0, this.PictureScrollBox.Width / 2 - this.PictureImage.Width / 2), localIntMax(0, this.PictureScrollBox.Height / 2 - this.PictureImage.Height / 2), this.PictureImage.Width, this.PictureImage.Height)
        this.numbersLabel.Caption = IntToStr(this.selectedPictureIndex + 1) + " of " + IntToStr(this.pictureNames.Count)
        this.CaptionRichEdit.text = this.replies.Strings[this.selectedPictureIndex]
        this.FirstPictureButton.Enabled = this.selectedPictureIndex > 0
        this.PreviousPictureButton.Enabled = this.FirstPictureButton.Enabled
        this.LastPictureButton.Enabled = this.selectedPictureIndex < this.pictureNames.Count - 1
        this.NextPictureButton.Enabled = this.LastPictureButton.Enabled
        if (!this.Visible) {
            this.Show()
        }
        this.BringToFront()
    }
    
    FormResize(Sender: TObject): void {
        //with CaptionRichEdit do setBounds(0, self.clientHeight - height, self.clientWidth, height);
        //controlsPanel.height
        //- CaptionRichEdit.height
        this.PictureScrollBox.SetBounds(0, 0, this.ClientWidth, this.ClientHeight - this.controlsPanel.Height)
        this.controlsPanel.SetBounds(0, this.ClientHeight - this.controlsPanel.Height, this.ClientWidth, this.controlsPanel.Height)
        this.PictureImage.SetBounds(localIntMax(0, this.PictureScrollBox.Width / 2 - this.PictureImage.Width / 2), localIntMax(0, this.PictureScrollBox.Height / 2 - this.PictureImage.Height / 2), this.PictureImage.Width, this.PictureImage.Height)
    }
    
    clearPictures(): void {
        this.pictureNames.Clear()
        this.FirstPictureButton.Enabled = false
        this.PreviousPictureButton.Enabled = false
        this.LastPictureButton.Enabled = false
        this.NextPictureButton.Enabled = false
        this.Hide()
    }
    
    FirstPictureButtonClick(Sender: TObject): void {
        this.selectedPictureIndex = 0
        this.loadSelectedPicture()
    }
    
    PreviousPictureButtonClick(Sender: TObject): void {
        this.selectedPictureIndex = localIntMax(0, this.selectedPictureIndex - 1)
        this.loadSelectedPicture()
    }
    
    NextPictureButtonClick(Sender: TObject): void {
        this.selectedPictureIndex = localIntMin(this.pictureNames.Count - 1, this.selectedPictureIndex + 1)
        this.loadSelectedPicture()
    }
    
    LastPictureButtonClick(Sender: TObject): void {
        this.selectedPictureIndex = this.pictureNames.Count - 1
        this.loadSelectedPicture()
    }
    
    // palette stuff 
    // ----------------------------------------------------------------------------- *palette stuff 
    GetPalette(): HPALETTE {
        let result = new HPALETTE()
        result = this.PictureImage.Picture.Bitmap.Palette
        return result
    }
    
    //overriden because paint box will not update correctly
    //makes window take first priority for palettes
    PaletteChanged(Foreground: boolean): boolean {
        let result = false
        let oldPalette: HPALETTE
        let palette: HPALETTE
        let windowHandle: HWnd
        let DC: HDC
        
        result = false
        if (delphi_compatability.Application.terminated) {
            return result
        }
        palette = this.GetPalette()
        if (palette !== 0) {
            DC = this.GetDeviceContext(windowHandle)
            oldPalette = UNRESOLVED.selectPalette(DC, palette, !Foreground)
            if ((UNRESOLVED.realizePalette(DC) !== 0) && (this.PictureImage !== null)) {
                // if palette changed, repaint drawing 
                this.PictureImage.Invalidate()
            }
            UNRESOLVED.selectPalette(DC, oldPalette, true)
            UNRESOLVED.realizePalette(DC)
            UNRESOLVED.releaseDC(windowHandle, DC)
        }
        result = TForm.prototype.PaletteChanged.call(this, Foreground)
        return result
    }
    
}

