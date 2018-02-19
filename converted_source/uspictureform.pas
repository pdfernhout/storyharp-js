unit uspictureform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, StdCtrls, ComCtrls;

type
  TPictureForm = class(TForm)
    controlsPanel: TPanel;
    FirstPictureButton: TSpeedButton;
    PreviousPictureButton: TSpeedButton;
    NextPictureButton: TSpeedButton;
    LastPictureButton: TSpeedButton;
    numbersLabel: TLabel;
    PictureScrollBox: TScrollBox;
    PictureImage: TImage;
    CaptionRichEdit: TRichEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FirstPictureButtonClick(Sender: TObject);
    procedure PreviousPictureButtonClick(Sender: TObject);
    procedure NextPictureButtonClick(Sender: TObject);
    procedure LastPictureButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    { palette stuff }
		function GetPalette: HPALETTE; override;
		function PaletteChanged(Foreground: Boolean): Boolean; override;
  public
    { Public declarations }
    pictureNames: TStringList;
    commands: TStringList;
    replies: TStringList;
    selectedPictureIndex: integer;
		procedure addPictureFromFile(const aFileName: string; const reply: string);
		procedure loadSelectedPicture;
		procedure clearPictures;
		procedure updateViews;
  end;

var
  PictureForm: TPictureForm;

implementation

{$R *.DFM}

uses USDomain, ucursor;

function localIntMin(a, b: integer): integer;
  begin
  result := a;
  if b < a then
  	result := b;
  end;

function localIntMax(a, b: integer): integer;
  begin
  result := a;
  if b > a then
  	result := b;
  end;

procedure TPictureForm.FormCreate(Sender: TObject);
	begin
  pictureNames := TStringList.create;
  commands := TStringList.create;
  replies := TStringList.create;
  domain.setFormSize(self, domain.options.pictureWindowRect);
  numbersLabel.caption := '';
  controlsPanel.bevelOuter := bvNone;
	end;

procedure TPictureForm.FormDestroy(Sender: TObject);
	begin
  pictureNames.free;
  pictureNames := nil;
  commands.free;
  commands := nil;
  replies.free;
  replies := nil;
	end;

procedure TPictureForm.updateViews;
  begin
  self.caption := 'StoryHarp Pictures - ' + ExtractFileName(domain.worldFileName);
  end;

procedure TPictureForm.addPictureFromFile(const aFileName: string; const reply: string);
  begin
  if not domain.options.showPictures then exit;
  // assumes file existence has been verified by caller (speech system)
  if pictureNames.indexOf(aFileName) < 0 then
    begin
    pictureNames.add(aFileName);
    //commands.add(command);
    replies.add(reply);
  	selectedPictureIndex := pictureNames.count - 1;
  	self.loadSelectedPicture;
    end
  else
    begin
  	selectedPictureIndex := pictureNames.indexOf(aFileName);
    self.loadSelectedPicture;
    end;
  end;

procedure TPictureForm.loadSelectedPicture;
  var
    fileName: string;
  begin
  fileName := pictureNames.strings[selectedPictureIndex];
  try
    cursor_startWait;
  	try
  		PictureImage.picture.bitmap.loadFromFile(fileName);
  	except
    	showMessage('Picture file ' + fileName + ' not found or could not load.');
      cursor_stopWait;
    	exit;
  	end;
  finally
    cursor_stopWait;
  end;
  with PictureImage do setBounds(localIntMax(0, PictureScrollBox.width div 2 - width div 2),
    localIntMax(0, PictureScrollBox.height div 2 - height div 2), width, height);
  numbersLabel.caption := intToStr(selectedPictureIndex + 1) + ' of ' + intToStr(pictureNames.count);
  CaptionRichEdit.text := replies.strings[selectedPictureIndex];

  FirstPictureButton.enabled := selectedPictureIndex > 0;
  PreviousPictureButton.enabled := FirstPictureButton.enabled;
  LastPictureButton.enabled := selectedPictureIndex < pictureNames.count - 1;
  NextPictureButton.enabled := LastPictureButton.enabled;
  if not self.visible then self.show;
  self.bringToFront;
  end;

procedure TPictureForm.FormResize(Sender: TObject);
	begin
  //with CaptionRichEdit do setBounds(0, self.clientHeight - height, self.clientWidth, height);
  with PictureScrollBox do setBounds(0, 0{controlsPanel.height}, self.clientWidth,
    self.clientHeight - controlsPanel.height {- CaptionRichEdit.height});
  with controlsPanel do setBounds(0, self.clientHeight - height, self.clientWidth, height);
  with PictureImage do setBounds(localIntMax(0, PictureScrollBox.width div 2 - width div 2),
    localIntMax(0, PictureScrollBox.height div 2 - height div 2), width, height);
	end;

procedure TPictureForm.clearPictures;
  begin
  pictureNames.clear;
  FirstPictureButton.enabled := false;
  PreviousPictureButton.enabled := false;
  LastPictureButton.enabled := false;
  NextPictureButton.enabled := false;
  self.hide;
  end;

procedure TPictureForm.FirstPictureButtonClick(Sender: TObject);
	begin
  selectedPictureIndex := 0;
  self.loadSelectedPicture;
	end;

procedure TPictureForm.PreviousPictureButtonClick(Sender: TObject);
	begin
  selectedPictureIndex := localIntMax(0, selectedPictureIndex - 1);
  self.loadSelectedPicture;
	end;

procedure TPictureForm.NextPictureButtonClick(Sender: TObject);
	begin
  selectedPictureIndex := localIntMin(pictureNames.count - 1, selectedPictureIndex + 1);
  self.loadSelectedPicture;
	end;

procedure TPictureForm.LastPictureButtonClick(Sender: TObject);
	begin
  selectedPictureIndex := pictureNames.count - 1;
  self.loadSelectedPicture;
	end;

{ ----------------------------------------------------------------------------- *palette stuff }
function TPictureForm.GetPalette: HPALETTE;
  begin
  result := PictureImage.picture.bitmap.palette;
  end;

{overriden because paint box will not update correctly}
{makes window take first priority for palettes}
function TPictureForm.PaletteChanged(Foreground: Boolean): Boolean;
	var
  	oldPalette, palette: HPALETTE;
  	windowHandle: HWnd;
  	DC: HDC;
	begin
  result := false;
  if Application.terminated then exit;
  palette := getPalette;
  if palette <> 0 then
  	begin
    DC := getDeviceContext(WindowHandle);
    oldPalette := selectPalette(DC, palette, not foreground);
    { if palette changed, repaint drawing }
    if (realizePalette(DC) <> 0) and (PictureImage <> nil) then
      PictureImage.invalidate;
    selectPalette(DC, oldPalette, True);
    realizePalette(DC);
    releaseDC(windowHandle, DC);
  	end;
  result := inherited paletteChanged(foreground);
	end;

end.
