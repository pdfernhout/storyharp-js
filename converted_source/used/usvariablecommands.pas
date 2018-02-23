unit usvariablecommands;

interface

uses Windows, SysUtils, Graphics, Classes, Controls, StdCtrls, UCommand, USWorld;

type

  TSToggleVariableCommand = class(KfCommand)
    variable: TSVariable;
    oldState: TSVariableState;
    newState: TSVariableState;
    constructor createWithVariable(variable: TSVariable);
		procedure setVariableStateWithUpdate(state: TSVariableState);
  	procedure doCommand; override;
  	procedure undoCommand; override;
  	function description: string; override;
    end;

  // need to have abstract base so TSDoCommandPhrase can defer updating till after changes
  TSAbstractMoveFocusCommand = class(KfCommand)
    oldFocus: TSVariable;
    oldFocusOldState: TSVariableState;
    newFocus: TSVariable;
    newFocusOldState: TsVariableState;
    constructor createWithNewFocus(newFocus: TSVariable);
		procedure updateForChanges;
    function shiftsFocus: boolean;
    end;

  TSMoveFocusCommand = class(TSAbstractMoveFocusCommand)
  	procedure doCommand; override;
  	procedure undoCommand; override;
  	function description: string; override;
    end;

  TSDoCommandPhrase = class(TSAbstractMoveFocusCommand)
    commandPhrase: string;
    changedVariables: TList;
    oldLastSaidTextWithMacros: string;
    newLastSaidTextWithMacros: string;
    oldFirstCommandDoneForLastCommandPhrase: integer;
    newFirstCommandDoneForLastCommandPhrase: integer;
    constructor createWithCommandPhrase(const commandPhrase: string);
		destructor destroy; override;
  	procedure doCommand; override;
  	procedure undoCommand; override;
 		procedure redoCommand; override;
  	function description: string; override;
    end;

implementation

uses USConsoleForm, USDomain, USRuleEditorForm;

{ ----------------------------- TSToggleVariableCommand --------------------- }

constructor TSToggleVariableCommand.createWithVariable(variable: TSVariable);
  begin
  self.create;
  self.variable := variable;
  self.oldState := variable.getState;
  if oldState = kPresent then
  	newState := kAbsent
  else
  	newState := kPresent;
  end;

procedure TSToggleVariableCommand.setVariableStateWithUpdate(state: TSVariableState);
  begin
  variable.setState(state);
  if ConsoleForm.ShowOnlyTrueVariablesButton.down then
    begin
    ConsoleForm.updateVariables;
    ConsoleForm.VariablesListBox.itemIndex := ConsoleForm.VariablesListBox.items.indexOfObject(variable);
  	ConsoleForm.VariablesListBox.invalidate;
    end
  else
    ConsoleForm.VariablesListBox.invalidate;
  domain.world.updateAvailable;
  ConsoleForm.speechSystem.listenForAvailableCommands;
	end;

procedure TSToggleVariableCommand.doCommand;
  begin
  self.setVariableStateWithUpdate(newState);
	inherited doCommand;
  end;

procedure TSToggleVariableCommand.undoCommand;
  begin
  self.setVariableStateWithUpdate(oldState);
  inherited undoCommand;
  end;

function TSToggleVariableCommand.description: string;
  begin
  if newState = kPresent then
  	result := 'toggle "' + variable.phrase + '" to true'
  else
    result := 'toggle "' + variable.phrase + '" to false';
  end;

{ ----------------------------- TSAbstractMoveFocusCommand --------------------- }

constructor TSAbstractMoveFocusCommand.createWithNewFocus(newFocus: TSVariable);
  begin
  self.create;
  // the old states are stored for undo in case author has been toggling them individually
  self.newFocus := newFocus;
  self.newFocusOldState := newFocus.getState;
  if domain.world.focus <> nil then
    begin
  	self.oldFocus := domain.world.focus;
  	self.oldFocusOldState := self.oldFocus.getState;
    end
  else
    begin
  	self.oldFocus := newFocus;
  	self.oldFocusOldState := newFocus.getState;
    end;
  end;

procedure TSAbstractMoveFocusCommand.updateForChanges;
  begin
  domain.world.updateAvailable;
  ConsoleForm.speechSystem.listenForAvailableCommands;
  ConsoleForm.updateVariables;
  ConsoleForm.VariablesListBox.invalidate;
	end;

function TSAbstractMoveFocusCommand.shiftsFocus: boolean;
  begin
  result := (newFocus <> domain.world.emptyEntry) and (newFocus <> oldFocus);
  end;

{ ----------------------------- TSMoveFocusCommand --------------------- }

procedure TSMoveFocusCommand.doCommand;
  begin
  oldFocus.setState(kAbsent);
  domain.world.focus := newFocus;
  newFocus.setState(kPresent);
  self.updateForChanges;
	inherited doCommand;
  end;

procedure TSMoveFocusCommand.undoCommand;
  begin
  newFocus.setState(newFocusOldState);
  domain.world.focus := oldFocus;
  oldFocus.setState(oldFocusOldState);
  self.updateForChanges;
  inherited undoCommand;
  end;

function TSMoveFocusCommand.description: string;
  begin
  result := 'move focus to ' + newFocus.phrase;
  end;

{ ----------------------------- TSDoCommandPhrase -------------------------------}

constructor TSDoCommandPhrase.createWithCommandPhrase(const commandPhrase: string);
  var
    i: integer;
    rule: TSRule;
  begin
  self.commandPhrase := commandPhrase;
  changedVariables := TList.create;
  // determine what would need to change - including new focus and all variables
  newFocus := domain.world.emptyEntry;
  oldLastSaidTextWithMacros := ConsoleForm.speechSystem.lastSaidTextWithMacros;
  newLastSaidTextWithMacros := '';
  oldFirstCommandDoneForLastCommandPhrase := domain.world.firstCommandDoneForLastCommandPhrase;

  newFirstCommandDoneForLastCommandPhrase := -1;
  if domain.world.rules.count > 0 then
    for i := 0 to domain.world.rules.count - 1 do
      begin
      rule := TSRule(domain.world.rules.items[i]);
      if rule.available and (AnsiCompareText(rule.command.phrase, commandPhrase) = 0) then
        begin
        RuleEditorForm.lastCommand := rule;
        if newFirstCommandDoneForLastCommandPhrase = -1 then
        	newFirstCommandDoneForLastCommandPhrase := i;
        rule.recordReplyMoveChanges(changedVariables, newLastSaidTextWithMacros, newFocus);
       	end;
      end;

  // elimitate leading $
  if (length(self.commandPhrase) > 1) and (self.commandPhrase[1] = '$') then
    self.commandPhrase := copy(self.commandPhrase, 2, length(self.commandPhrase));

  self.createWithNewFocus(newFocus);
  end;

destructor TSDoCommandPhrase.destroy;
  var
  	i: integer;
  begin
  for i := 0 to changedVariables.count - 1 do
    TSChangedVariableWrapper(changedVariables[i]).free;
  changedVariables.free;
  changedVariables := nil;
  inherited destroy;
  end;

procedure TSDoCommandPhrase.doCommand;
  var
  	i: integer;
  begin
  ConsoleForm.addLineToTranscript('> ' + commandPhrase, clRed);
  ConsoleForm.addLineToTranscript(ConsoleForm.speechSystem.stripMacros(newLastSaidTextWithMacros), clBlue);
  ConsoleForm.scrollTranscriptEndIntoView;
  ConsoleForm.speechSystem.sayTextWithMacros(newLastSaidTextWithMacros);

  {common with undo}
  ConsoleForm.speechSystem.lastSaidTextWithMacros := newLastSaidTextWithMacros;
  domain.world.firstCommandDoneForLastCommandPhrase := newFirstCommandDoneForLastCommandPhrase;
  if newFocus <> domain.world.emptyEntry then
    begin
  	oldFocus.setState(kAbsent);
  	domain.world.focus := newFocus;
  	newFocus.setState(kPresent);
    end;
  for i := 0 to changedVariables.count - 1 do
    TSChangedVariableWrapper(changedVariables[i]).doChange;
  self.updateForChanges;
  ConsoleForm.speechSystem.checkForSayOptionsMacro;
  inherited doCommand;
 end;

procedure TSDoCommandPhrase.undoCommand;
  var
  	i: integer;
    undoPhrase: string;
  begin
  ConsoleForm.addLineToTranscript('> undo', clRed);
//  undoPhrase := 'It is as if "' + commandPhrase + '" had never been said.';
  undoPhrase := '(You decide not to say "' + commandPhrase + '")';
  ConsoleForm.addLineToTranscript(undoPhrase, clBlue);
  ConsoleForm.scrollTranscriptEndIntoView;
  ConsoleForm.speechSystem.speakText(undoPhrase);
  
  ConsoleForm.speechSystem.lastSaidTextWithMacros := oldLastSaidTextWithMacros;
  domain.world.firstCommandDoneForLastCommandPhrase := oldFirstCommandDoneForLastCommandPhrase;
  for i := changedVariables.count - 1 downto 0 do
    TSChangedVariableWrapper(changedVariables[i]).undoChange;
  if newFocus <> domain.world.emptyEntry then
    begin
  	newFocus.setState(newFocusOldState);
  	domain.world.focus := oldFocus;
  	oldFocus.setState(oldFocusOldState);
    end;
  self.updateForChanges;
  inherited undoCommand;
  end;

procedure TSDoCommandPhrase.redoCommand;
  var
  	i: integer;
    redoPhrase: string;
  begin
  ConsoleForm.addLineToTranscript('> redo', clRed);
  redoPhrase := '(You decide to say "' + commandPhrase + '" anyway)';
  ConsoleForm.addLineToTranscript(redoPhrase, clBlue);
  ConsoleForm.scrollTranscriptEndIntoView;
  ConsoleForm.speechSystem.speakText(redoPhrase);

  {common with do}
  ConsoleForm.speechSystem.lastSaidTextWithMacros := newLastSaidTextWithMacros;
  domain.world.firstCommandDoneForLastCommandPhrase := newFirstCommandDoneForLastCommandPhrase;
  if newFocus <> domain.world.emptyEntry then
    begin
  	oldFocus.setState(kAbsent);
  	domain.world.focus := newFocus;
  	newFocus.setState(kPresent);
    end;
  for i := 0 to changedVariables.count - 1 do
    TSChangedVariableWrapper(changedVariables[i]).doChange;
  self.updateForChanges;
  inherited doCommand;
 end;

function TSDoCommandPhrase.description: string;
  begin
  result := 'command: ' + commandPhrase;
  end;

end.
