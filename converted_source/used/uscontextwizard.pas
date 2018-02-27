unit uscontextwizard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TContextWizardForm = class(TForm)
    goBack: TButton;
    goNext: TButton;
    cancel: TButton;
    helpButton: TButton;
    notebook: TNotebook;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Image3: TImage;
    NewContextsMemo: TMemo;
    DescribeLabel: TLabel;
    DescribeLabelExtra: TLabel;
    Label4: TLabel;
    DescribeImage: TImage;
    DescribeEdit: TEdit;
    Label13: TLabel;
    Label14: TLabel;
    Label18: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    Image1: TImage;
    Label10: TLabel;
    Label26: TLabel;
    Label19: TLabel;
    Image2: TImage;
    Label8: TLabel;
    commandStartPageImage: TImage;
    contextStartPageImage: TImage;
    replyStartPageImage: TImage;
    commandImage: TImage;
    Label11: TLabel;
    Label12: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    procedure goBackClick(Sender: TObject);
    procedure goNextClick(Sender: TObject);
    procedure cancelClick(Sender: TObject);
    procedure notebookPageChanged(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    generated: boolean;
    function initialize: boolean;
		procedure generateRules;
  end;

var
  ContextWizardForm: TContextWizardForm;

implementation

uses USDomain, USWorld, USRuleEditorForm, USConsoleForm, USCommands,
  USChangeLog;

{$R *.DFM}

const
  kStartPage = 0;
  kContextsPage = 1;
  kDescriptionPage = 2;
  kFinishPage = 3;

procedure TContextWizardForm.FormActivate(Sender: TObject);
	begin
  contextStartPageImage.picture.bitmap := ConsoleForm.ContextButton.glyph;
  commandStartPageImage.picture.bitmap := ConsoleForm.CommandButton.glyph;
  replyStartPageImage.picture.bitmap := RuleEditorForm.ReplyPicture.picture.bitmap;
  commandImage.picture.bitmap := ConsoleForm.CommandButton.glyph;
	end;

function TContextWizardForm.initialize: boolean;
  begin
  generated := false;
  notebook.pageIndex := kStartPage;
  NewContextsMemo.Text := '';
  result := true;
  end;

procedure TContextWizardForm.goBackClick(Sender: TObject);
	begin
  with notebook do
  	if pageIndex > kStartPage then
    	pageIndex := pageIndex - 1;
	end;

procedure TContextWizardForm.goNextClick(Sender: TObject);
	begin
  if notebook.pageIndex = kFinishPage then
    self.generateRules
  else if notebook.pageIndex = kContextsPage then
    begin
    if (Trim(NewContextsMemo.text) = '') then
     	begin
      ShowMessage('You must enter one or more contexts to proceed.');
      exit;
     	end;
    end
  else if notebook.pageIndex = kDescriptionPage then
    begin
    if (Trim(DescribeEdit.text) = '') then
     	begin
      ShowMessage('You must enter a command to be used to describe these contexts.');
      exit;
     	end;
    end;
  with notebook do
  	if pageIndex < kFinishPage then
    	pageIndex := pageIndex + 1;
	end;

procedure TContextWizardForm.notebookPageChanged(Sender: TObject);
	begin
  goBack.enabled := notebook.pageIndex > kStartPage;
  if noteBook.pageIndex <> kFinishPage then
    goNext.caption := '&Next >>'
  else
    goNext.caption := '&Finish';
	end;

procedure TContextWizardForm.cancelClick(Sender: TObject);
	begin
  modalResult := mrCancel;
	end;

procedure TContextWizardForm.GenerateRules;
	var
  	stream: TStringStream;
  	line, previousContext, contextName, contextDescription, character: string;
  	pipeRead: boolean;
  	newRule: TSRule;
  	position: TPoint;
    newRulesCommand: TSNewRulesCommand;
	begin
  ChangeLogForm.addToLog(NewContextsMemo.Text);
  position := RuleEditorForm.goodPosition;
	stream := TStringStream.create(NewContextsMemo.Text);
  previousContext := '';
  newRulesCommand := TSNewRulesCommand.create;
  newRulesCommand.creator := 'new context wizard';
  try
  character := stream.ReadString(1);
  while character <> '' do
    begin
    line := '';
    contextName := '';
    contextDescription := '';
    pipeRead := false;
    while not((character = #13) or (character = #10) or (character = '')) do
      begin
      line := line + character;
      if pipeRead then
        contextDescription := contextDescription + character
      else if character = '|' then
      	pipeRead := true
      else
        contextName := contextName + character;
    	character := stream.ReadString(1);
      end;
 		//ShowMessage('name: ' + contextName);
  	//ShowMessage('description: ' + contextDescription);
    if Trim(contextName) <> '' then
      begin
      if (Trim(DescribeEdit.text) <> '') then
        begin
    		newRule := domain.world.newRule;
       	newRulesCommand.addRule(newRule);
        RuleEditorForm.lastChoice := newRule;
        newRule.position := position;
    		newRule.setContext(Trim(contextName));
      	newRule.setCommand(Trim(DescribeEdit.text));
   	  	if Trim(contextDescription) <> '' then
        	newRule.setReply(Trim(contextDescription))
      	else
        	newRule.setReply('There is nothing of interest here.');
        end;
      previousContext := Trim(contextName);
      position.y := position.y + 60;
   	  end;
		while (character = #13) or (character = #10) do
    	character := stream.ReadString(1);
    end;
  finally
  	stream.free;
  end;
  if newRulesCommand.rules.count > 0 then
  	domain.worldCommandList.doCommand(newRulesCommand)
  else
    newRulesCommand.free;
  RuleEditorForm.updateForRuleChange;
  RuleEditorForm.adjustScrollBars;
  self.modalResult := mrOK;
  generated := true;
	end;

{
cave|You are in a big cave.
forest|You are in a lively forest.
spring|You are standing near a burbling spring.
}

(*
well house|You are in a well house for a small spring.
grate|You are standing above a grate.
forest|You are wandering around in dense forest.
glade|You are in a forest glade.
stream|You are walking along a dry stream bed.
*)

procedure TContextWizardForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
  var
  	result: word;
	begin
  if generated then exit;
  if Trim(NewContextsMemo.text) <> '' then
    begin
  	result := MessageDlg('You are about to close this wizard' + chr(13) + 'and lose any work done with it.'
      + chr(13) + chr(13) + 'Is this OK?',
    	mtConfirmation, [mbYes, mbNo], 0);
  	if result <> mrYes	then
    	begin
      CanClose := false;
      exit;
      end;
  	ChangeLogForm.addToLog(NewContextsMemo.Text);
    end;
	end;

procedure TContextWizardForm.helpButtonClick(Sender: TObject);
	begin
	Application.helpJump('Making_new_rules_using_the_new_contexts_wizard');
	end;

end.

  //with forwardEdit do
  //  if enabled then color := clWindow else color := clBtnFace;

