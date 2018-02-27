unit usagentwarning;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TAgentWarningForm = class(TForm)
    Label1: TLabel;
    dontShowThisDialogAgain: TCheckBox;
    okButton: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure okButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses USDomain;

{$R *.DFM}

procedure TAgentWarningForm.okButtonClick(Sender: TObject);
	begin
  domain.options.suppressAgentNotPresentWarning := dontShowThisDialogAgain.checked;
  ModalResult := mrOK;
	end;

end.


