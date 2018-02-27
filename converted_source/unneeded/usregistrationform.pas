unit usregistrationform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, HttpProt;

type
  TRegistrationForm = class(TForm)
    UserNameEdit: TEdit;
    RegistrationCodeEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    DisplayMemo: TMemo;
    DateInstalledLabel: TLabel;
    DaysSinceInstallationLabel: TLabel;
    EnableEditorButton: TButton;
    OnlineRegistrationButton: TButton;
    HttpClient: THttpCli;
    AbortButton: TButton;
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure UserNameEditKeyPress(Sender: TObject; var Key: Char);
    procedure RegistrationCodeEditKeyPress(Sender: TObject; var Key: Char);
    procedure FormActivate(Sender: TObject);
    procedure EnableEditorButtonClick(Sender: TObject);
    procedure OnlineRegistrationButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
   end;

var
  RegistrationForm: TRegistrationForm;

implementation

uses USConsoleForm;

{$R *.DFM}

procedure TRegistrationForm.OKButtonClick(Sender: TObject);
	begin
  if length(trim(UserNameEdit.Text)) < 3 then
    begin
    ShowMessage('A name must be entered');
    exit;
    end;
  if (lowercase(trim(RegistrationCodeEdit.Text))) <> 'htya9513' then
    begin
    ShowMessage('Invalid registration code. Please see http://www.kurtz-fernhout.com for instructions on how to register.');
    exit;
    end;
  ShowMessage('Thank you for supporting our work.');
  ConsoleForm.registrationOK(trim(UserNameEdit.Text));
  self.modalResult := 1;
  end;

procedure TRegistrationForm.CancelButtonClick(Sender: TObject);
	begin
  self.modalResult := mrCancel;
	end;

procedure TRegistrationForm.UserNameEditKeyPress(Sender: TObject;
  var Key: Char);
begin
//	OkButton.enabled := (UserNameEdit.Text <> '') and (RegistrationCodeEdit.Text <> '');
end;

procedure TRegistrationForm.RegistrationCodeEditKeyPress(Sender: TObject;
  var Key: Char);
begin
//	OkButton.enabled := (UserNameEdit.Text <> '') and (RegistrationCodeEdit.Text <> '');
end;

procedure TRegistrationForm.FormActivate(Sender: TObject);
	begin
  DateInstalledLabel.caption := 'Date editor enabled: ' + DateToStr(Date);
  DaysSinceInstallationLabel.caption := 'Days since editor enabled: ' + '1';
  if DateInstalledLabel.visible then
		EnableEditorButton.caption := 'Disable Editor'
  else
		EnableEditorButton.caption := 'Enable Editor'
	end;

procedure TRegistrationForm.EnableEditorButtonClick(Sender: TObject);
	begin
  if DateInstalledLabel.visible then
    begin
		DateInstalledLabel.visible := false;
		DaysSinceInstallationLabel.visible := false;
  	ConsoleForm.MenuEditStory.enabled := false;
  	ConsoleForm.MenuOptionsShowVariables.enabled := false;
  	ConsoleForm.MenuOptionsUpdateEditorSelections.enabled := false;
  	self.modalResult := mrCancel;
    end
  else
    begin
		DateInstalledLabel.visible := true;
		DaysSinceInstallationLabel.visible := true;
  	ConsoleForm.MenuEditStory.enabled := true;
  	ConsoleForm.MenuOptionsShowVariables.enabled := true;
  	ConsoleForm.MenuOptionsUpdateEditorSelections.enabled := true;
  	ShowMessage('The editor is now enabled.' + chr(13) +
    'If you find value in it, please register.' + chr(13) +
    'You must register to legally distribute or publically present' + chr(13) +
    'anything produced directly or indirectly through using the editor.' + chr(13) +
    'See the license for details.');
  	self.modalResult := mrCancel;
    end;
	end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Encode the data to be sent to the CGI                                     }
function Encode(const msg : String) : String;
  var
    I : Integer;
  begin
    Result := '';
    for I := 1 to Length(msg) do begin
        if msg[I] = ' ' then
            Result := Result + '+'
        else if msg[I] in ['a'..'z', 'A'..'Z'] then
            Result := Result + '%' + IntToHex(ord(msg[I]), 2)
        else
            Result := Result + msg[I];
    end;
  end;

procedure TRegistrationForm.OnlineRegistrationButtonClick(Sender: TObject);
  var
    DataIn, DataOut: TMemoryStream;
    Buf: string;
	begin

  if Trim(UserNameEdit.text) = '' then
    begin
    ShowMessage('You must enter your name first');
    exit;
    end;
  DisplayMemo.text := 'Connecting to http://www.kurtz-fernhout.com';
  DataIn := TMemoryStream.create;
  DataOut := TMemoryStream.create;
  try
  	Buf := 'N=' + Encode(UserNameEdit.text);
  	DataOut.Write(Buf[1], Length(Buf));
  	DataOut.seek(0, soFromBeginning);

  	HttpClient.SendStream := DataOut;
  	HttpClient.RcvdStream := DataIn;
  	HttpClient.Proxy := ''; // none for now - not sure what this is
  	HttpClient.ProxyPort := '80';
  	HttpClient.URL := 'http://www.kurtz-fernhout.com/qny147/register.cgi';

    OnlineRegistrationButton.enabled := false;
    AbortButton.enabled := true;
    try
  		try
  			HttpClient.post;
    		DataIn.seek(0, 0);
    		DisplayMemo.Lines.LoadFromStream(DataIn);
  		except
    		DisplayMemo.Lines.Add('Failed : ' + HttpClient.ReasonPhrase);
  		end;
    finally
    	OnlineRegistrationButton.enabled := true;
    	AbortButton.enabled := false;
    end;
  finally
  	DataOut.free;
  	DataIn.free;
  end;
	end;

end.
