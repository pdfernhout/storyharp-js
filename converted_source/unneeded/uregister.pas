unit uregister;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Grids, ComCtrls;

type
  TRegistrationForm = class(TForm)
    Label11: TLabel;
    Panel1: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    registrationNameEdit: TEdit;
    registrationCodeEdit: TEdit;
    Label1: TLabel;
    orderPageEdit: TEdit;
    Label2: TLabel;
    Label5: TLabel;
    openBrowser: TButton;
    close: TButton;
    readLicense: TButton;
    moreInfo: TButton;
    registerMe: TButton;
    Label6: TLabel;
    PrintButton: TButton;
    thankYou: TRichEdit;
    PrintDialog: TPrintDialog;
    procedure openBrowserClick(Sender: TObject);
    procedure registerMeClick(Sender: TObject);
    procedure closeClick(Sender: TObject);
    procedure moreInfoClick(Sender: TObject);
    procedure readLicenseClick(Sender: TObject);
    procedure PrintButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    printedDetails: boolean;
    opened: boolean;
  end;

var
  RegistrationForm: TRegistrationForm;

implementation
          
{$R *.DFM}

uses ShellAPI, USDomain, UFileSupport, URegisterSupport, Printers;

procedure TRegistrationForm.FormActivate(Sender: TObject);
	begin
  opened := true;
	end;

procedure TRegistrationForm.openBrowserClick(Sender: TObject);
  begin
  ShellExecute(Handle, pChar('open'), pChar('http://www.kurtz-fernhout.com/order.htm'), nil, nil, SW_SHOWNORMAL);
  end;

procedure TRegistrationForm.registerMeClick(Sender: TObject);
  var cr: string;
  begin
  if RegistrationMatch(registrationNameEdit.text, registrationCodeEdit.text) then
    begin
    registerMe.hide;
    thankYou.clear;
    cr := chr(13) + chr(10);
    thankYou.text :=
      'StoryHarp version 1.x Registration Details' + cr +
			'-------------------------------------------' + cr +
      'Registration name:  ' + registrationNameEdit.text + cr +
      'Registration code:  ' + registrationCodeEdit.text + cr + cr +
    	'Welcome to the StoryHarp community!' + cr +
			'You now own a fully registered copy of StoryHarp version 1.x.' + cr + cr +
			'Your registration also entitles you to free upgrades of StoryHarp up to but not including version 2.0. See the help system for details.' + cr + cr +
			'We will save encoded registration information in your INI settings file in your Windows directory. You should keep a copy of your name and code in a safe place in case you need to reinstall StoryHarp.' + cr + cr +
      '(You should click Print now to print this message.)' + cr + cr +
			'Be sure to visit our web site to read the latest news about StoryHarp. Thank you for your business!' + cr + cr +
			'--- Paul Fernhout and Cynthia Kurtz' + cr +
			'Kurtz-Fernhout Software' + cr +
			'http://www.kurtz-fernhout.com' + cr +
			'-------------------------------------------';
    thankYou.left := 4;
    thankYou.top := 4;
    thankYou.show;
    thankYou.bringToFront;
    domain.registrationName := registrationNameEdit.text;
    domain.registrationCode := registrationCodeEdit.text;
    domain.registered := true;
    domain.justRegistered := true;
    PrintButton.visible := true;
    end
  else                                     //309 352 0302
    begin
    MessageDlg('The registration name and code you entered are incorrect.'
      + chr(13) + 'Please retype them and try again.'
      + chr(13) + chr(13) + 'If you are sure they are correct'
      + chr(13) + 'and you still get this message, contact us.',
      mtError, [mbOK], 0);
    end;
  end;


procedure TRegistrationForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  if not opened then exit; 
  CanClose := false;
  if (not domain.registered)
  	and ((trim(registrationNameEdit.text) <> '') or (trim(registrationCodeEdit.text) <> '')) then
      begin
      ShowMessage('You entered registration information but didn''t finish registering.' + chr(13)
        + 'You should click "Register Me!" or clear out the registration information.');
      exit;
      end;
  if (domain.registered) and (not printedDetails) then
    begin
    if MessageDlg('You registered but didn''t print the information.' + chr(13)
        + 'Do you want to leave WITHOUT printing your registration information?', mtConfirmation, mbYesNoCancel, 0)
        <> IDYES then exit;
    end;
  CanClose := true;
	end;

procedure TRegistrationForm.closeClick(Sender: TObject);
  begin
  if domain.registered then
    modalResult := mrOK
  else
    modalResult := mrCancel;
  end;

procedure TRegistrationForm.moreInfoClick(Sender: TObject);
  begin
  application.helpJump('Why_register?');
  end;

procedure TRegistrationForm.readLicenseClick(Sender: TObject);
  begin
  application.helpJump('License'); 
  end;

procedure TRegistrationForm.PrintButtonClick(Sender: TObject);
	begin
  if printDialog.execute then
    begin
    thankYou.print('StoryHarp version 1.x Registration Details');
    printedDetails := true;
    end;
	end;

procedure TRegistrationForm.FormCreate(Sender: TObject);
	begin
	thankYou.visible := true;
	thankYou.visible := false;
	end;


end.

