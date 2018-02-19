unit uscommandwizard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  TCommandWizardForm = class(TForm)
    notebook: TNotebook;
    Label1: TLabel;
    Label2: TLabel;
    NewCommandsMemo: TMemo;
    GenerateSequence: TCheckBox;
    PrefixEdit: TEdit;
    PrefixLabel: TLabel;
    sequenceEndPanel: TPanel;
    endSequenceLabel: TLabel;
    endSequenceLoopToFirst: TRadioButton;
    endSequenceRemoveTheLastCommand: TRadioButton;
    endSequenceLeaveLastCommand: TRadioButton;
    helpButton: TButton;
    goBack: TButton;
    goNext: TButton;
    cancel: TButton;
    Image1: TImage;
    Label7: TLabel;
    Label17: TLabel;
    Label26: TLabel;
    Image3: TImage;
    Label8: TLabel;
    Label11: TLabel;
    Image2: TImage;
    Label9: TLabel;
    Label12: TLabel;
    Image4: TImage;
    Label13: TLabel;
    prefixArrow: TImage;
    sequenceEndArrow: TImage;
    prefixNote: TLabel;
    Label18: TLabel;
    Label16: TLabel;
    Label20: TLabel;
    Label3: TLabel;
    Image8: TImage;
    ContextBox: TListBox;
    commandStartPageImage: TImage;
    Label5: TLabel;
    contextStartPageImage: TImage;
    Label6: TLabel;
    replyStartPageImage: TImage;
    Label10: TLabel;
    contextListImage: TImage;
    Label4: TLabel;
    Label14: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label19: TLabel;
    Label15: TLabel;
    requirementsStartPageImage: TImage;
    changesStartPageImage: TImage;
    Label24: TLabel;
    Label25: TLabel;
    Label27: TLabel;
    newCommandsForContextLabel: TLabel;
    procedure GenerateSequenceClick(Sender: TObject);
    procedure goNextClick(Sender: TObject);
    procedure goBackClick(Sender: TObject);
    procedure cancelClick(Sender: TObject);
    procedure notebookPageChanged(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    lastFloodedContextPrefix: string;
    generated: boolean;
    function initialize: boolean;
    procedure generateRules;
  end;

var
  CommandWizardForm: TCommandWizardForm;

const
  kStartPage = 0;
  kContextPage = 1;
  kCommandsPage = 2;
  kSequencePage = 3;
  kFinishPage = 4;

implementation

uses USDomain, USWorld, USRuleEditorForm, USConsoleForm, USCommands,
  USChangeLog;

{$R *.DFM}

procedure TCommandWizardForm.FormActivate(Sender: TObject);
	begin
  contextStartPageImage.picture.bitmap := ConsoleForm.ContextButton.glyph;
  commandStartPageImage.picture.bitmap := ConsoleForm.CommandButton.glyph;
  replyStartPageImage.picture.bitmap := RuleEditorForm.ReplyPicture.picture.bitmap;
  requirementsStartPageImage.picture.bitmap := ConsoleForm.RequirementsButton.glyph;
  changesStartPageImage.picture.bitmap := ConsoleForm.ChangesButton.glyph;
  contextListImage.picture.bitmap := ConsoleForm.ContextButton.glyph;
	end;

function TCommandWizardForm.initialize: boolean;
  var
  	variable: TSVariable;
  	i, j: integer;
  begin
  generated := false;
  notebook.pageIndex := kStartPage;
  domain.world.addContextsToListBox(ContextBox);
  if ContextBox.items.count < 1 then
    begin
  	result := false;
    ShowMessage('You must create at least one context before using the command wizard.');
    exit;
    end;
  //self.GenerateSequenceClick(self);
  NewCommandsMemo.text := '';
  GenerateSequence.checked := false;
  PrefixEdit.text := '';
  endSequenceLoopToFirst.checked := true;
  lastFloodedContextPrefix := '';
  for i := 0 to domain.world.variables.count - 1 do
    begin
    variable := TSVariable(domain.world.variables[i]);
    if variable.selected then
      begin
      for j := 0 to ContextBox.items.count - 1 do
      if ContextBox.items[j] = variable.phrase then
        begin
        ContextBox.itemIndex := j;
        break;
        end;
      end;
    end;
  result := true;
  end;

procedure TCommandWizardForm.goNextClick(Sender: TObject);
	begin
  if notebook.pageIndex = kFinishPage then
    self.generateRules
  else if notebook.pageIndex = kContextPage then
    begin
    if (ContextBox.itemIndex < 0) then
     	begin
      ShowMessage('You must select a context to proceed.');
      exit;
     	end;
    if (Trim(PrefixEdit.text) = '') or (Trim(PrefixEdit.text) = lastFloodedContextPrefix) then
      begin
     	PrefixEdit.text := ContextBox.items[ContextBox.itemIndex];
      lastFloodedContextPrefix := ContextBox.items[ContextBox.itemIndex];
      end;
    newCommandsForContextLabel.caption := 'New commands for: ' + ContextBox.items[ContextBox.itemIndex];
    end
  else if notebook.pageIndex = kCommandsPage then
    begin
    if (Trim(NewCommandsMemo.text) = '') then
     	begin
      ShowMessage('You must enter one or more commands to proceed.');
      exit;
     	end;
    end
  else if notebook.pageIndex = kSequencePage then
    begin
    if GenerateSequence.checked then
      begin
    	if (Trim(PrefixEdit.text) = '') then
     		begin
      	ShowMessage('You must enter a prefix to proceed.');
      	exit;
     		end;
    	end;
    end;
  with notebook do
  	if pageIndex < kFinishPage then
    	pageIndex := pageIndex + 1;
	end;

procedure TCommandWizardForm.goBackClick(Sender: TObject);
	begin
  with notebook do
  	if pageIndex > kStartPage then
    	pageIndex := pageIndex - 1;
	end;

procedure TCommandWizardForm.notebookPageChanged(Sender: TObject);
	begin
  goBack.enabled := notebook.pageIndex > kStartPage;
  if noteBook.pageIndex <> kFinishPage then
    goNext.caption := '&Next >>'
  else
    goNext.caption := '&Finish';
	end;

procedure TCommandWizardForm.cancelClick(Sender: TObject);
	begin
  modalResult := mrCancel;
	end;

procedure TCommandWizardForm.generateRules;
	var
  	stream: TStringStream;
  	line: string;
  	previousContext: string;
  	commandPhrase, commandResponse: string;
  	character: string;
  	pipeRead: boolean;
  	newRule: TSRule;
  	position: TPoint;
  	index: integer;
    newRulesCommand: TSNewRulesCommand;
    context: string;
    requirements, changes: string;
    prefix: string;
	begin
  ChangeLogForm.addToLog(NewCommandsMemo.Text);
  position := RuleEditorForm.goodPosition;
	stream := TStringStream.create(NewCommandsMemo.Text);
  previousContext := '';
  index := 1;
  newRulesCommand := TSNewRulesCommand.create;
  newRulesCommand.creator := 'command sequence wizard';
  newRule := nil;
  try
  character := stream.ReadString(1);
  while character <> '' do
    begin
    line := '';
    commandPhrase := '';
    commandResponse := '';
    pipeRead := false;
    prefix := Trim(PrefixEdit.text);
    while not((character = #13) or (character = #10) or (character = '')) do
      begin
      line := line + character;
      if pipeRead then
        commandResponse := commandResponse + character
      else if character = '|' then
      	pipeRead := true
      else
        commandPhrase := commandPhrase + character;
    	character := stream.ReadString(1);
      end;
 //  	ShowMessage('name: ' + contextName);
  //  ShowMessage('description: ' + contextDescription);
    context := '';
    if ContextBox.itemIndex >= 0 then
      context := ContextBox.items[ContextBox.itemIndex];
    if Trim(commandResponse) = '' then
      commandResponse := 'Nothing happens.'; 
    if (Trim(commandPhrase) <> '') and (Trim(context) <> '') then
      begin
    	newRule := domain.world.newRule;
      newRulesCommand.addRule(newRule);
      RuleEditorForm.lastChoice := newRule;
      newRule.position := position;
    	newRule.setContext(Trim(context));
    	newRule.setCommand(Trim(commandPhrase));
      newRule.setReply(Trim(commandResponse));
      if GenerateSequence.checked and (prefix <> '') then
        begin
        if index = 1 then
          begin
          requirements :=
          		'~' + prefix + ' started';
          changes :=
        						prefix + ' started & ' +
        	  				prefix + ' ' + IntToStr(index) + '0';
          end
        else
          begin
        	requirements :=
          					prefix + ' ' + IntToStr(index - 1) + '0';
        	changes :=
        			'~' + prefix + ' ' + IntToStr(index - 1) + '0 & ' +
        	    			prefix + ' ' + IntToStr(index    ) + '0';
          end;
        newRule.setRequirements(requirements);
        newRule.setChanges(changes);
        end;
      position.y := position.y + 60;
      inc(index);
   	  end;
		while (character = #13) or (character = #10) do
    	character := stream.ReadString(1);
    end;
    // cleanup for last rule
    if GenerateSequence.checked and (prefix <> '') and
    		(newRule <> nil) and (index > 2) then
      begin
      changes := newRule.changesString;
      if endSequenceLoopToFirst.checked then
        begin
        changes :=
        			'~' + prefix + ' ' + IntToStr(index - 2) + '0 & ' +
        	    '~' +	prefix + ' started';
        end
      else if endSequenceLeaveLastCommand.checked then
        begin
        changes := '';
        end
      else if endSequenceRemoveTheLastCommand.checked then
        begin
        changes :=
        			'~' + prefix + ' ' + IntToStr(index - 2) + '0';
        end;
     newRule.setChanges(changes);
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
  self.modalResult := 1;
  generated := true;
	end;

procedure TCommandWizardForm.GenerateSequenceClick(Sender: TObject);
  var
    doSequence: boolean;
	begin
  doSequence := GenerateSequence.checked;
  prefixArrow.visible := doSequence;
	PrefixLabel.enabled := doSequence;
	PrefixEdit.visible := doSequence;
  prefixNote.enabled := doSequence;
  sequenceEndArrow.visible := doSequence;
  endSequenceLabel.enabled := doSequence;
  endSequenceLoopToFirst.enabled := doSequence;
  endSequenceLeaveLastCommand.enabled := doSequence;
  endSequenceRemoveTheLastCommand.enabled := doSequence;
	end;

{
talk to the grue|The grue won't listen.
talk to the grue|The grue seems to be getting very agitated.
talk to the grue|The grue seems about to fly into a rage.
talk to the grue|The grue devours you (except your bones of course).
grue pit
}

procedure TCommandWizardForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
  var
  	result: word;
	begin
  if generated then exit;
  if Trim(NewCommandsMemo.text) <> '' then
    begin
  	result := MessageDlg('You are about to close this wizard' + chr(13) + 'and lose any work done with it.'
      + chr(13) + chr(13) + 'Is this OK?',
    	mtConfirmation, [mbYes, mbNo], 0);
  	if result <> mrYes	then exit;
  	ChangeLogForm.addToLog(NewCommandsMemo.Text);
    end;
  modalResult := mrCancel;
	end;

procedure TCommandWizardForm.helpButtonClick(Sender: TObject);
	begin
	Application.helpJump('Making_new_rules_using_the_new_commands_wizard');
	end;

end.
