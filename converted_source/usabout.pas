unit usabout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TAboutForm = class(TForm)
    OKButton: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    versionLabel: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    registeredToLabel: TLabel;
    procedure OKButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
		procedure showNameString(aName: string);
		procedure setUpAsSplashOrAbout(splash: boolean);
  end;

var
  AboutForm: TAboutForm;
const
  kAsSplash = true; kAsAbout = false;

implementation

{$R *.DFM}

uses USDomain, UFileSupport;

procedure TAboutForm.FormCreate(Sender: TObject);
  begin
  versionLabel.caption := gVersionName;
  end;

procedure TAboutForm.OKButtonClick(Sender: TObject);
	begin
  modalResult := mrOK;
	end;

procedure TAboutForm.setUpAsSplashOrAbout(splash: boolean);
  begin
  if splash then
    begin
    okButton.visible := false;
    caption := '';
    borderIcons := [];
    showNameString('');
    end
  else
    begin
  	caption := 'About StoryHarp';
  	borderIcons := [biSystemMenu,biMinimize,biMaximize];
  	okButton.visible := true;
    showNameString(domain.registrationName);
    end
  end;

procedure TAboutForm.showNameString(aName: string);
  begin
  if aName = '' then
    registeredToLabel.caption := ''
  else if domain.playerOnly then
    registeredToLabel.caption := 'Player-only mode'
  else if not domain.registered then
    registeredToLabel.caption := 'Unregistered'
  else
    begin
    if (length(aName) > 1) and (aName[length(aName)] = ',')  then
      aName := copy(aName, 1, length(aName) - 1);
    registeredToLabel.caption := 'Registered to: ' + aName;
    end;
  end;

end.
