unit usmediadirform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TExtraMediaDirectoryForm = class(TForm)
    Close: TButton;
    cancel: TButton;
    helpButton: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    extraMediaDirectoryEdit: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    openWorldFileDirectory: TEdit;
    exeDirectory: TEdit;
    windowsMediaDirectory: TEdit;
    procedure CloseClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ExtraMediaDirectoryForm: TExtraMediaDirectoryForm;

implementation

{$R *.DFM}

uses FileCtrl, USDomain, USSpeech;

procedure TExtraMediaDirectoryForm.FormActivate(Sender: TObject);
	begin
  openWorldFileDirectory.text := domain.worldFileName;
  openWorldFileDirectory.text := ExtractFilePath(domain.worldFileName);
  if openWorldFileDirectory.text = '' then
    openWorldFileDirectory.text := GetCurrentDir + '\';
  extraMediaDirectoryEdit.text := domain.options.extraMediaDirectory;
  exeDirectory.text := ExtractFilePath(Application.exeName);
  windowsMediaDirectory.text := getWindowsMediaDirectory;
	end;

procedure TExtraMediaDirectoryForm.CloseClick(Sender: TObject);
	begin
  if not directoryExists(extraMediaDirectoryEdit.text) then
    begin
    ShowMessage('The directory you specified does not exist.');
    exit;
    end;
  domain.options.extraMediaDirectory := extraMediaDirectoryEdit.text;
  modalResult := mrOK;
	end;

procedure TExtraMediaDirectoryForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('Setting_the_sounds_and_music_directory'); 
  end;

end.
