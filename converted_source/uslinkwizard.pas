unit uslinkwizard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, USWorld;

type
  TLinkWizardForm = class(TForm)
    notebook: TNotebook;
    Label10: TLabel;
    ForwardLabel: TLabel;
    Label13: TLabel;
    ForwardEdit: TEdit;
    forwardReplyLabel: TLabel;
    ForwardMemo: TMemo;
    forwardReplyNote: TLabel;
    helpButton: TButton;
    goBack: TButton;
    goNext: TButton;
    cancel: TButton;
    Label11: TLabel;
    BackwardLabel: TLabel;
    Label12: TLabel;
    BackwardEdit: TEdit;
    backwardReplyLabel: TLabel;
    BackwardMemo: TMemo;
    Image1: TImage;
    Label4: TLabel;
    Label17: TLabel;
    Label26: TLabel;
    Image3: TImage;
    Image2: TImage;
    Image4: TImage;
    forwardReplyArrow: TImage;
    Image6: TImage;
    backwardReplyArrow: TImage;
    Label1: TLabel;
    backwardReplyNote: TLabel;
    Label18: TLabel;
    Label14: TLabel;
    Label19: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Image8: TImage;
    FirstContextBox: TListBox;
    SecondContextBox: TListBox;
    commandStartPageImage: TImage;
    Label2: TLabel;
    contextStartPageImage: TImage;
    Label5: TLabel;
    replyStartPageImage: TImage;
    Label6: TLabel;
    Label7: TLabel;
    firstContextImage: TImage;
    secondContextImage: TImage;
    forwardCommandImage: TImage;
    forwardReplyImage: TImage;
    backwardCommandImage: TImage;
    backwardReplyImage: TImage;
    Label15: TLabel;
    Label16: TLabel;
    forwardSummary: TLabel;
    backwardSummary: TLabel;
    procedure cancelClick(Sender: TObject);
    procedure goBackClick(Sender: TObject);
    procedure goNextClick(Sender: TObject);
    procedure notebookPageChanged(Sender: TObject);
    procedure ForwardEditChange(Sender: TObject);
    procedure BackwardEditChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    generated: boolean;
    function initialize: boolean;
		function firstContext: string;
		function secondContext: string;
    procedure generateRules;
		function makeLink(const firstContext: string; secondContext: string; const command: string; const reply: string): TSRule;
  end;

var
  LinkWizardForm: TLinkWizardForm;

const
  kStartPage = 0;
  kContextsPage = 1;
  kForwardPage = 2;
  kBackwardPage = 3;
  kFinishPage = 4;

implementation

uses USDomain, USRuleEditorForm, USCommands, USChangeLog, USConsoleForm;

{$R *.DFM}

procedure TLinkWizardForm.FormActivate(Sender: TObject);
	begin
  contextStartPageImage.picture.bitmap := ConsoleForm.ContextButton.glyph;
  commandStartPageImage.picture.bitmap := ConsoleForm.CommandButton.glyph;
  replyStartPageImage.picture.bitmap := RuleEditorForm.ReplyPicture.picture.bitmap;
  firstContextImage.picture.bitmap := ConsoleForm.ContextButton.glyph;
  secondContextImage.picture.bitmap := ConsoleForm.ContextButton.glyph;
  forwardCommandImage.picture.bitmap := ConsoleForm.CommandButton.glyph;
  forwardReplyImage.picture.bitmap := RuleEditorForm.ReplyPicture.picture.bitmap;
  backwardCommandImage.picture.bitmap := ConsoleForm.CommandButton.glyph;
  backwardReplyImage.picture.bitmap := RuleEditorForm.ReplyPicture.picture.bitmap;
	end;

function TLinkWizardForm.initialize: boolean;
  var
    variable: TSVariable;
    i, j: integer;
    contextCount: integer;
  begin
  result := false;
  generated := false;
  notebook.pageIndex := kStartPage;
  domain.world.addContextsToListBox(FirstContextBox);
  if FirstContextBox.items.count < 2 then
    begin
    ShowMessage('You must create at least two contexts before using the link wizard.');
    exit;
    end;
  domain.world.addContextsToListBox(SecondContextBox);
  contextCount := 0;
  self.ForwardEditChange(self);
  self.BackwardEditChange(self);
  ForwardEdit.text := '';
  ForwardMemo.text := '';
  BackwardEdit.text := '';
  BackwardMemo.text := '';
  result := true;
  // must be done last because of exit
  for i := 0 to domain.world.variables.count - 1 do
    begin
    variable := TSVariable(domain.world.variables[i]);
    if variable.selected then
      begin
      if contextCount = 0 then
        begin
        for j := 0 to FirstContextBox.items.count - 1 do
          if FirstContextBox.items[j] = variable.phrase then
            begin
            FirstContextBox.itemIndex := j;
        		inc(contextCount);
            break;
            end;
        end
      else
        begin
        for j := 0 to SecondContextBox.items.count - 1 do
          if SecondContextBox.items[j] = variable.phrase then
            begin
            SecondContextBox.itemIndex := j;
        		exit;
            end;
        end;
      end;
    end;
  end;

procedure TLinkWizardForm.goBackClick(Sender: TObject);
	begin
  with notebook do
  	if pageIndex > kStartPage then
    	pageIndex := pageIndex - 1;
	end;

procedure TLinkWizardForm.goNextClick(Sender: TObject);
  var
    first, second: string;
	begin
  first := trim(self.firstContext);
  second := trim(self.secondContext);
  if notebook.pageIndex = kFinishPage then
    self.generateRules
  else if notebook.pageIndex = kContextsPage then
    begin
    if (first = '') or (second = '') then
     	begin
      ShowMessage('Both contexts must be entered to proceed.');
      exit;
     	end;
    if first = second then
     	begin
      ShowMessage('The two contexts must have different names.');
      exit;
     	end;
  	ForwardLabel.caption := first + ' --> ' + second;
  	BackwardLabel.caption := second + ' --> ' + first;
    end;
  if notebook.pageIndex = kForwardPage then
    begin
    if (Trim(ForwardEdit.text) = '') and (Trim(ForwardMemo.text) <> '') then
     	begin
      ShowMessage('You must enter a command phrase if you enter a reply.');
      exit;
     	end;
    end;
  if notebook.pageIndex = kBackwardPage then
    begin
    if (Trim(BackwardEdit.text) = '') and (Trim(BackwardMemo.text) <> '') then
     	begin
      ShowMessage('You must enter a command phrase if you enter a reply.');
      exit;
     	end;
    if (Trim(BackwardEdit.text) = '') and (Trim(ForwardEdit.text) = '') then
     	begin
      ShowMessage('You must enter a command on this page or the previous page to generate a link.');
      exit;
     	end;
    end;
  with notebook do
  	if pageIndex < kFinishPage then
    	pageIndex := pageIndex + 1;
	end;

procedure TLinkWizardForm.notebookPageChanged(Sender: TObject);
	begin
  goBack.enabled := notebook.pageIndex > kStartPage;
  if noteBook.pageIndex <> kFinishPage then
    goNext.caption := '&Next >>'
  else
    begin
    goNext.caption := '&Finish';
    if Trim(ForwardEdit.text) = '' then
      forwardSummary.caption := ''
    else
      forwardSummary.caption := firstContext + '  ---  '  + Trim(ForwardEdit.text) + '  -->  ' + secondContext;
    if Trim(BackwardEdit.text) = '' then
      backwardSummary.caption := ''
    else
      backwardSummary.caption := secondContext + '  ---  '  + Trim(BackwardEdit.text) + '  -->  ' + firstContext;
    end;
	end;

procedure TLinkWizardForm.cancelClick(Sender: TObject);
	begin
  modalResult := mrCancel;
	end;

function TLinkWizardForm.makeLink(const firstContext: string; secondContext: string; const command: string; const reply: string): TSRule;
	var
    dx, dy: integer;
  begin
  result := nil;
  if firstContext = '' then exit;
  result := domain.world.newRule;
  RuleEditorForm.lastChoice := result;
  result.setContext(firstContext);
  result.setCommand(command);
  if Trim(reply) <> '' then
    result.setReply(reply)
  else
    result.setReply('You ' + command + '.');  
  result.setMove(secondContext);
  result.position.x := (result.context.position.x + result.move.position.x) div 2;
  result.position.y := (result.context.position.y + result.move.position.y) div 2;
  dx := result.context.position.x - result.move.position.x;
  dy := result.context.position.y - result.move.position.y;
  //now determine offset
  if abs(dy) >= abs(dx) then
    begin
    if dy >= 0 then
    	result.position.x := result.position.x - 100
   	else
    	result.position.x := result.position.x + 100;
    end
  else
    begin
    if dx < 0 then
    	result.position.y := result.position.y - 30
   	else
    	result.position.y := result.position.y + 30;
    end;
  end;

function TLinkWizardForm.firstContext: string;
  begin
  result := '';
  with FirstContextBox do
    if itemIndex >= 0 then
      result := items[itemIndex];
  end;

function TLinkWizardForm.secondContext: string;
  begin
  result := '';
  with SecondContextBox do
    if itemIndex >= 0 then
      result := items[itemIndex];
  end;

procedure TLinkWizardForm.generateRules;
	var
    newRulesCommand: TSNewRulesCommand;
    newRule: TSRule;
	begin
  ChangeLogForm.addToLog(ForwardMemo.text);
  ChangeLogForm.addToLog(BackwardMemo.text);
  newRulesCommand := TSNewRulesCommand.create;
  newRulesCommand.creator := 'link wizard';
  newRule := self.makeLink(Trim(firstContext), Trim(secondContext),
  	Trim(ForwardEdit.text), Trim(ForwardMemo.text));
	if newRule <> nil then
		newRulesCommand.addRule(newRule);
  newRule := self.makeLink(Trim(secondContext), Trim(firstContext),
  	Trim(BackwardEdit.text), Trim(BackwardMemo.text));
	if newRule <> nil then
		newRulesCommand.addRule(newRule);
  if newRulesCommand.rules.count > 0 then
  	domain.worldCommandList.doCommand(newRulesCommand)
  else
    newRulesCommand.free;
  RuleEditorForm.updateForRuleChange;
  RuleEditorForm.adjustScrollBars;
  self.modalResult := 1;
  generated := true;
	end;

procedure TLinkWizardForm.ForwardEditChange(Sender: TObject);
  var
    haveText: boolean;
	begin
  haveText := ForwardEdit.text <> '';
  forwardReplyArrow.visible := haveText;
  forwardReplyLabel.enabled := haveText;
  forwardReplyImage.visible := haveText;
  forwardMemo.visible := haveText;
  forwardReplyNote.enabled := haveText;
	end;

procedure TLinkWizardForm.BackwardEditChange(Sender: TObject);
  var
    haveText: boolean;
	begin
  haveText := BackwardEdit.text <> '';
  backwardReplyArrow.visible := haveText;
  backwardReplyLabel.enabled := haveText;
  backwardReplyImage.visible := haveText;
  backwardMemo.visible := haveText;
  backwardReplyNote.enabled := haveText;
	end;

procedure TLinkWizardForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
  var
  	result: word;
	begin
  if generated then exit;
  if (Trim(ForwardEdit.text) <> '') or
  	(Trim(ForwardMemo.text) <> '') or
  	(Trim(BackwardEdit.text) <> '') or
  	(Trim(BackwardMemo.text) <> '') then
    begin
  	result := MessageDlg('You are about to close this wizard' + chr(13) + 'and lose any work done with it.'
      + chr(13) + chr(13) + 'Is this OK?',
    	mtConfirmation, [mbYes, mbNo], 0);
  	if result <> mrYes	then
      begin
      CanClose := false;
    	exit;
      end;
  	ChangeLogForm.addToLog(ForwardMemo.text);
  	ChangeLogForm.addToLog(BackwardMemo.text);
    end;
	end;

procedure TLinkWizardForm.helpButtonClick(Sender: TObject);
	begin
	Application.helpJump('Making_new_rules_using_the_new_moves_wizard');
	end;

end.
