// initial comment

unit uabout;

// before interface and uses comment

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TUnregisteredAboutForm = class(TForm)
    close: TButton;
    registerIt: TButton;
    noDistributeLabel: TLabel;
    readLicense: TButton;
    TimePanel: TPanel;
    hoursLabel: TLabel;
    cancel: TButton;
    whyRegister: TButton;
    timeWarningLabel: TLabel;
    Label1: TLabel;
    Image1: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    versionLabel: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    registeredToLabel: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure closeClick(Sender: TObject);
    procedure readLicenseClick(Sender: TObject);
    procedure registerItClick(Sender: TObject);
    procedure hoursLabelClick(Sender: TObject);
    procedure whyRegisterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure initializeWithWhetherClosingProgram(closingProgram: boolean);
  end;

var
  UnregisteredAboutForm: TUnregisteredAboutForm;

implementation

{$R *.DFM}

uses USDomain, uregister, UFileSupport;

procedure TUnregisteredAboutForm.initializeWithWhetherClosingProgram(closingProgram: boolean);
  begin
  if domain.playerOnly then
    begin
  	self.clientHeight := TimePanel.top - 1;
    self.caption := 'Thank you for using the StoryHarp player!';
    self.registeredToLabel.caption := 'Player-only mode';
    whyRegister.visible := false;
    registerIt.visible := false;
    readLicense.top := registerIt.top;
		end;
  if closingProgram then
    close.caption := 'Quit'
  else
    close.caption := 'Close';
  cancel.visible := closingProgram;
  end;

procedure TUnregisteredAboutForm.FormActivate(Sender: TObject);
  var
    timeBetween: TDateTime;
    smallHours, minutes, seconds, milliseconds: Word;
    randomNumber: integer;
    hours: integer;
    i: integer;
  begin
  if domain.playerOnly then exit;
  activeControl := registerIt;
  randomize;
  for i := 0 to 100 do random;
  randomNumber := random(2);
  case randomNumber of
    0:
      begin
      close.top := 4;
      registerIt.top := close.top + close.height + 3;
      end;
    1:
      begin
      registerIt.top := 4;
      close.top := registerIt.top + registerIt.height + 3;
      end;
    end;
  hoursLabel.caption := 'You have been using StoryHarp for ';
  timeBetween := max((Now - domain.startTimeThisSession), 0) + domain.accumulatedUnregisteredTime;
  DecodeTime(timeBetween, smallHours, minutes, seconds, milliseconds);
  hours := smallHours;
  if timeBetween >= 1.0 then
    hours := hours + trunc(timeBetween) * 24;
    // hoursLabel.caption := hoursLabel.caption + 'more than 24 hours.'
  if (minutes < 1) and (hours < 1) then
    hoursLabel.caption := hoursLabel.caption + 'less than one minute.'
  else if (minutes = 1) and (hours < 1) then
    hoursLabel.caption := hoursLabel.caption + 'one minute.'
  else if hours < 1 then
    hoursLabel.caption := hoursLabel.caption + intToStr(minutes) + ' minutes.'
  else if hours = 1 then
    hoursLabel.caption := hoursLabel.caption + intToStr(hours) + ' hour and ' + intToStr(minutes) + ' minutes.'
  else
    hoursLabel.caption := hoursLabel.caption + intToStr(hours) + ' hours and ' + intToStr(minutes) + ' minutes.';
  if hours >= 24.0 then
    begin
  	timeWarningLabel.font.color := clGreen;
  	timeWarningLabel.font.style := [fsBold];
    end;
  end;

procedure TUnregisteredAboutForm.closeClick(Sender: TObject);
  begin
  modalResult := mrOK;
  end;

procedure TUnregisteredAboutForm.registerItClick(Sender: TObject);
  begin
  if RegistrationForm.showModal = mrOK then
    modalResult := mrCancel;
  end;

procedure TUnregisteredAboutForm.hoursLabelClick(Sender: TObject);
  //var
  //  hours, minutes: smallint;
  //  hourString: string;
  begin
  // only for cfk testing -- remove later
  (*
  if inputQuery('Testing', 'Enter number of hours', hourString) then
    hours := strToInt(hourString);
  if inputQuery('Testing', 'Enter number of minutes', hourString) then
    minutes := strToInt(hourString);

  hoursLabel.caption := 'You have been evaluating StoryHarp for ';
  if hours >= 24 then
    hoursLabel.caption := hoursLabel.caption + 'more than 24 hours.'
  else if (minutes < 1) and (hours < 1) then
    hoursLabel.caption := hoursLabel.caption + 'less than one minute.'
  else if (minutes = 1) and (hours < 1) then
    hoursLabel.caption := hoursLabel.caption + 'one minute.'
  else if hours < 1 then
    hoursLabel.caption := hoursLabel.caption + intToStr(minutes) + ' minutes.'
  else if hours = 1 then
    hoursLabel.caption := hoursLabel.caption + intToStr(hours) + ' hour and ' + intToStr(minutes) + ' minutes.'
  else
    hoursLabel.caption := hoursLabel.caption + intToStr(hours) + ' hours and ' + intToStr(minutes) + ' minutes.';
  *)
  end;

procedure TUnregisteredAboutForm.readLicenseClick(Sender: TObject);
  begin
  application.helpJump('License');
  end;

procedure TUnregisteredAboutForm.whyRegisterClick(Sender: TObject);
  begin
  application.helpJump('Why_register?'); 
  end;

procedure TUnregisteredAboutForm.FormCreate(Sender: TObject);
begin
  versionLabel.caption := gVersionName;
end;

// almost final comment

end.

// very final comment
