unit USPreferences;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Buttons,
  USDomain;

type
  TPreferencesForm = class(TForm)
    Close: TButton;
    cancel: TButton;
    helpButton: TSpeedButton;
    Panel1: TPanel;
    Label1: TLabel;
    tableFontNameEdit: TEdit;
    changeTableFont: TButton;
    backgroundColorPanel: TPanel;
    textColorPanel: TPanel;
    showMapCommands: TCheckBox;
    Label3: TLabel;
    Label2: TLabel;
    changeBackgroundColor: TButton;
    changeTextColor: TButton;
    ColorDialog: TColorDialog;
    FontDialog: TFontDialog;
    Label4: TLabel;
    mapFontNameEdit: TEdit;
    changeMapFont: TButton;
    Label5: TLabel;
    browserFontNameEdit: TEdit;
    changeBrowserFont: TButton;
    Label6: TLabel;
    mapCommandsColorPanel: TPanel;
    ChangeMapCommandsColor: TButton;
    symbolButtons: TCheckBox;
    procedure FormActivate(Sender: TObject);
    procedure changeBackgroundColorClick(Sender: TObject);
    procedure changeTextColorClick(Sender: TObject);
    procedure changeTableFontClick(Sender: TObject);
    procedure changeBrowserFontClick(Sender: TObject);
    procedure changeMapFontClick(Sender: TObject);
    procedure showMapCommandsClick(Sender: TObject);
    procedure ChangeMapCommandsColorClick(Sender: TObject);
    procedure symbolButtonsClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    options: DomainOptionsStructure;
  end;

var
  PreferencesForm: TPreferencesForm;

implementation

{$R *.DFM}

procedure TPreferencesForm.FormActivate(Sender: TObject);
	begin
  with options do
    begin
  	tableFontNameEdit.text := options.tableFontName + ', ' + intToStr(options.tableFontSize);
  	mapFontNameEdit.text := options.mapFontName + ', ' + intToStr(options.mapFontSize);
  	browserFontNameEdit.text := options.browserFontName + ', ' + intToStr(options.browserFontSize);
    backgroundColorPanel.color := selectedItemColor;
    textColorPanel.color := selectedTextColor;
    showMapCommands.checked := showCommandPrefixInMap;
    symbolButtons.checked := buttonSymbols;
    end;
	end;

procedure TPreferencesForm.changeTableFontClick(Sender: TObject);
	begin
  fontDialog.font.name := options.tableFontName;
  fontDialog.font.size := options.tableFontSize;
  if fontDialog.execute then
    begin
    options.tableFontName := fontDialog.font.name;
    options.tableFontSize := fontDialog.font.size;
  	tableFontNameEdit.text := options.tableFontName + ', ' + intToStr(options.tableFontSize);
    end;
	end;

procedure TPreferencesForm.changeMapFontClick(Sender: TObject);
	begin
  fontDialog.font.name := options.mapFontName;
  fontDialog.font.size := options.mapFontSize;
  if fontDialog.execute then
    begin
    options.mapFontName := fontDialog.font.name;
    options.mapFontSize := fontDialog.font.size;
  	mapFontNameEdit.text := options.mapFontName + ', ' + intToStr(options.mapFontSize);
    end;
	end;

procedure TPreferencesForm.changeBrowserFontClick(Sender: TObject);
	begin
  fontDialog.font.name := options.browserFontName;
  fontDialog.font.size := options.browserFontSize;
  if fontDialog.execute then
    begin
    options.browserFontName := fontDialog.font.name;
    options.browserFontSize := fontDialog.font.size;
  	browserFontNameEdit.text := options.browserFontName + ', ' + intToStr(options.browserFontSize);
    end;
	end;

procedure TPreferencesForm.changeBackgroundColorClick(Sender: TObject);
	begin
  colorDialog.color := backgroundColorPanel.color;
  if colorDialog.execute then
    begin
    backgroundColorPanel.color := colorDialog.color;
    options.selectedItemColor := backgroundColorPanel.color;
    end;
	end;

procedure TPreferencesForm.changeTextColorClick(Sender: TObject);
	begin
  colorDialog.color := textColorPanel.color;
  if colorDialog.execute then
    begin
    textColorPanel.color := colorDialog.color;
    options.selectedTextColor := textColorPanel.color;
    end;
	end;

procedure TPreferencesForm.ChangeMapCommandsColorClick(Sender: TObject);
	begin
  colorDialog.color := mapCommandsColorPanel.color;
  if colorDialog.execute then
    begin
    mapCommandsColorPanel.color := colorDialog.color;
    options.commandTextColorInMap := mapCommandsColorPanel.color;
    end;
	end;

procedure TPreferencesForm.showMapCommandsClick(Sender: TObject);
	begin
  options.showCommandPrefixInMap := showMapCommands.checked;
	end;

procedure TPreferencesForm.symbolButtonsClick(Sender: TObject);
	begin
  options.buttonSymbols := symbolButtons.checked;
	end;

procedure TPreferencesForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('Changing_editor_preferences'); 
  end;

end.
