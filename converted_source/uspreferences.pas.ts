// unit USPreferences

from conversion_common import *
import usdomain
import delphi_compatability

// var
let PreferencesForm: TPreferencesForm



export class TPreferencesForm {
    Close: TButton = new TButton()
    cancel: TButton = new TButton()
    helpButton: TSpeedButton = new TSpeedButton()
    Panel1: TPanel = new TPanel()
    Label1: TLabel = new TLabel()
    tableFontNameEdit: TEdit = new TEdit()
    changeTableFont: TButton = new TButton()
    backgroundColorPanel: TPanel = new TPanel()
    textColorPanel: TPanel = new TPanel()
    showMapCommands: TCheckBox = new TCheckBox()
    Label3: TLabel = new TLabel()
    Label2: TLabel = new TLabel()
    changeBackgroundColor: TButton = new TButton()
    changeTextColor: TButton = new TButton()
    ColorDialog: TColorDialog = new TColorDialog()
    FontDialog: TFontDialog = new TFontDialog()
    Label4: TLabel = new TLabel()
    mapFontNameEdit: TEdit = new TEdit()
    changeMapFont: TButton = new TButton()
    Label5: TLabel = new TLabel()
    browserFontNameEdit: TEdit = new TEdit()
    changeBrowserFont: TButton = new TButton()
    Label6: TLabel = new TLabel()
    mapCommandsColorPanel: TPanel = new TPanel()
    ChangeMapCommandsColor: TButton = new TButton()
    symbolButtons: TCheckBox = new TCheckBox()
    options: DomainOptionsStructure = new DomainOptionsStructure()
    TPreferencesForm.prototype = new TForm()
    TPreferencesForm.prototype.constructor = TPreferencesForm
    
    //$R *.DFM
    FormActivate(Sender: TObject): void {
        this.tableFontNameEdit.Text = this.options.tableFontName + ", " + IntToStr(this.options.tableFontSize)
        this.mapFontNameEdit.Text = this.options.mapFontName + ", " + IntToStr(this.options.mapFontSize)
        this.browserFontNameEdit.Text = this.options.browserFontName + ", " + IntToStr(this.options.browserFontSize)
        this.backgroundColorPanel.Color = this.options.selectedItemColor
        this.textColorPanel.Color = this.options.selectedTextColor
        this.showMapCommands.Checked = this.options.showCommandPrefixInMap
        this.symbolButtons.Checked = this.options.buttonSymbols
    }
    
    changeTableFontClick(Sender: TObject): void {
        this.FontDialog.Font.Name = this.options.tableFontName
        this.FontDialog.Font.Size = this.options.tableFontSize
        if (this.FontDialog.Execute()) {
            this.options.tableFontName = this.FontDialog.Font.Name
            this.options.tableFontSize = this.FontDialog.Font.Size
            this.tableFontNameEdit.Text = this.options.tableFontName + ", " + IntToStr(this.options.tableFontSize)
        }
    }
    
    changeMapFontClick(Sender: TObject): void {
        this.FontDialog.Font.Name = this.options.mapFontName
        this.FontDialog.Font.Size = this.options.mapFontSize
        if (this.FontDialog.Execute()) {
            this.options.mapFontName = this.FontDialog.Font.Name
            this.options.mapFontSize = this.FontDialog.Font.Size
            this.mapFontNameEdit.Text = this.options.mapFontName + ", " + IntToStr(this.options.mapFontSize)
        }
    }
    
    changeBrowserFontClick(Sender: TObject): void {
        this.FontDialog.Font.Name = this.options.browserFontName
        this.FontDialog.Font.Size = this.options.browserFontSize
        if (this.FontDialog.Execute()) {
            this.options.browserFontName = this.FontDialog.Font.Name
            this.options.browserFontSize = this.FontDialog.Font.Size
            this.browserFontNameEdit.Text = this.options.browserFontName + ", " + IntToStr(this.options.browserFontSize)
        }
    }
    
    changeBackgroundColorClick(Sender: TObject): void {
        this.ColorDialog.Color = this.backgroundColorPanel.Color
        if (this.ColorDialog.Execute()) {
            this.backgroundColorPanel.Color = this.ColorDialog.Color
            this.options.selectedItemColor = this.backgroundColorPanel.Color
        }
    }
    
    changeTextColorClick(Sender: TObject): void {
        this.ColorDialog.Color = this.textColorPanel.Color
        if (this.ColorDialog.Execute()) {
            this.textColorPanel.Color = this.ColorDialog.Color
            this.options.selectedTextColor = this.textColorPanel.Color
        }
    }
    
    ChangeMapCommandsColorClick(Sender: TObject): void {
        this.ColorDialog.Color = this.mapCommandsColorPanel.Color
        if (this.ColorDialog.Execute()) {
            this.mapCommandsColorPanel.Color = this.ColorDialog.Color
            this.options.commandTextColorInMap = this.mapCommandsColorPanel.Color
        }
    }
    
    showMapCommandsClick(Sender: TObject): void {
        this.options.showCommandPrefixInMap = this.showMapCommands.Checked
    }
    
    symbolButtonsClick(Sender: TObject): void {
        this.options.buttonSymbols = this.symbolButtons.Checked
    }
    
    helpButtonClick(Sender: TObject): void {
        delphi_compatability.Application.HelpJump("Changing_editor_preferences")
    }
    
}

