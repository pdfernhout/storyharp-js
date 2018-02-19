unit usworld;

interface

  uses Windows, Classes, Dialogs, stdctrls;

const
  kSaveAllRules = false;
	kSaveOnlySelectedRules = true;

type

  TWorld = class;

  TSVariableState = (kPresent, kAbsent);

  TSVariableDisplayOptions = array [0..5] of boolean;

  TSDraggableObject = class(TObject)
    position: TPoint;
    extent: TPoint;
    selected: boolean;
    function displayName: string; virtual;
    procedure setPosition(value: string); virtual;
    function bounds: TRect;
		function center: TPoint;
    end;

  TSDragRecord = class(TObject)
    draggedNode: TSDraggableObject;
    originalLocation: TPoint;
    newLocation: TPoint;
    constructor createWithNode(node: TSDraggableObject);
    procedure doDrag;
    procedure undoDrag;
    procedure offset(delta: TPoint);
    end;

	TSVariable = class(TSDraggableObject)
    world: TWorld;
    phrase: string;
    state: TSVariableState;
    contextUseages: integer;
    requirementsUseages: integer;
    commandUseages: integer;
    moveUseages: integer;
    changesUseages: integer;
    indexInVariables: integer;  // for java creation
    function displayName: string; override;
		procedure setPhrase(const aPhrase: string);
		procedure setState(newState: TSVariableState);
		function getState: TSVariableState;
    function hasUseagesForField(col: integer): boolean;
		function meetsDisplayOptions(const displayOptions: TSVariableDisplayOptions): boolean;
    end;

  TSDesiredStateVariableWrapper = class(TObject)
    variable: TSVariable;
    desiredState: TSVariableState;
   	function leader: string;
   	function displayLeader: string;
    procedure invertDesiredState;
		function displayString: string;
    end;

  TSChangedVariableWrapper = class(TObject)
    variable: TSVariable;
    oldState: TSVariableState;
    newState: TSVariableState;
    constructor createWithVariableNewState(variable: TSVariable; newState: TSVariableState);
    procedure doChange;
    procedure undoChange;
    end;

  TSRule = class(TSDraggableObject)
  	world: TWorld;
    context: TSVariable;
    requirements: TList;
    command: TSVariable;
    reply: string;
    move: TSVariable;
    changes: TList;
    available: boolean;
    requirementsString: string;
    changesString: string;
    useagesRemoved: boolean;
    function displayName: string; override;
		constructor create;
		destructor destroy; override;
		procedure setContext(const aString: string);
		procedure setRequirements(const aString: string);
		procedure setCommand(const aString: string);
		procedure setReply(const aString: string);
		procedure setMove(const aString: string);
		procedure setChanges(const aString: string);
    procedure addUseages;
    procedure removeUseages;
		procedure compile(const aString: string; list: TList);
		function decompile(list: TList): string;
    function decompileRequirements: string;
    function decompileChanges: string;
		procedure updateAvailable;
		procedure recordReplyMoveChanges(changedVariablesList: TList; var totalReply: string; var contextToFocusTo: TSVariable);
		procedure setTextForField(col: integer; const text: string);
 		function getTextForField(col: integer): string;
		class function headerForField(col: integer): string;
		function usesVariableInList(variable: TSVariable; list: TList): boolean;
		function usesVariableFor(variable: TSVariable; col: integer): boolean;
		function variableInList(n: integer; list: TList): TSVariable;
		function variableForFieldWithSelections(col: integer; requirementsIndex, changesIndex: integer): TSVariable;
		function variableForField(col: integer): TSVariable;
    procedure setPosition(value: string); override;
    end;

  TSIndexChangeRuleWrapper = class(TObject)
    rule: TSRule;
    oldIndex: integer;
    newIndex: integer;
    constructor createWithRuleNewIndex(rule: TSRule; newIndex: integer);
    procedure doChange;
    procedure undoChange;
    end;

	TWorld = class(TObject)
  	emptyEntry: TSVariable;
    variables: TList;
    rules: TList;
    focus: TSVariable;
    previousFocus: TSVariable;
    firstCommandDoneForLastCommandPhrase: integer;
    lastVariableCreated: string;
		constructor Create;
		procedure resetVariableValues;
		procedure resetVariablesAndRules;
		destructor Destroy; override;
		function newRule: TSRule;
		function findVariable(const aString: string): TSVariable;
		function findOrCreateVariable(const aString: string; madeByMacro: boolean): TSVariable;
    procedure setInitialFocus;
		procedure newWorld;
		function loadWorldFromFile(const name: string): boolean;
		procedure saveWorldToFile(const name: string; saveOnlySelectedRules: boolean);
		procedure newSession;
		function loadSessionFromFile(const name: string; const worldFileName: string): boolean;
		procedure saveSessionToFile(const name: string; const worldFileName: string);
		procedure updateAvailable;
		procedure setFocusTo(contextToFocusOn: TSVariable);
		function deselectAllExcept(exceptObject: TSDraggableObject): boolean;
		procedure addDragRecordsToList(dragRecords: TList);
		procedure deleteSelectedRules;
		procedure raiseSelectedRules;
		procedure lowerSelectedRules;
		procedure selectAvailable;
    function firstAvailable: TSRule;
    function boundsRect: TRect;
		procedure selectInRectangle(rect: TRect);
		function firstSelectedVariable: TSVariable;
		function firstSelectedObject: TSDraggableObject;
		procedure addContextsToCombBox(comboBox: TComboBox);
		procedure addContextsToListBox(listBox: TListBox);
		procedure updateVariablesForIndexInVariables;
    end;

  // need to be sequence from zero
	const
  	kRuleContext = 0;
		kRuleCommand = 1;
		kRuleReply = 2;
		kRuleMove = 3;
		kRuleRequirements = 4;
		kRuleChanges = 5;
    kLastRuleField = 5;

implementation

uses USMain, SysUtils, USConsoleForm, USRuleEditorForm, UFileSupport, USCommands, USDomain;

//////////////////// TObjectWithPosition ///////////////////////

function TSDraggableObject.displayName: string;
  begin
  result := 'Error - override needed';
  end;

procedure TSDraggableObject.setPosition(value: string);
  var
  	firstNumber, secondNumber: string;
  begin
  firstNumber := Copy(value, 1, pos(',', value) - 1);
  secondNumber := Copy(value, length(firstNumber) + 2, length(value));
  try
  	position.x :=  StrToInt(firstNumber);
  	position.y :=  StrToInt(secondNumber);
  except
  	//position.x := 0;
  	//position.y := 0;
  end;
  end;

function TSDraggableObject.bounds: TRect;
  var
    topLeft: TPoint;
  begin
  topLeft := Point(position.x - extent.x div 2, position.y - extent.y div 2);
  with topLeft do
  	result := Rect(x, y, x + extent.x, y + extent.y);
  end;

function TSDraggableObject.center: TPoint;
  begin
  result := position;
  end;

//////////////////// TSDragRecord ///////////////////////

constructor TSDragRecord.createWithNode(node: TSDraggableObject);
  begin
  self.create;
  draggedNode := node;
  originalLocation := draggedNode.position;
  newLocation := originalLocation;
  end;

procedure TSDragRecord.doDrag;
  begin
  draggedNode.position := newLocation;
  end;

procedure TSDragRecord.undoDrag;
  begin
  draggedNode.position := originalLocation;
  end;

procedure TSDragRecord.offset(delta: TPoint);
	begin
	newLocation := Point(newLocation.x + delta.x, newLocation.y + delta.y);
  draggedNode.position := newLocation;
	end;

//////////////////// TSVariable ///////////////////////

function TSVariable.displayName: string;
  begin
  result := self.phrase;
  end;

procedure TSVariable.setPhrase(const aPhrase: string);
  begin
  phrase := aPhrase;
  end;

procedure TSVariable.setState(newState: TSVariableState);
  begin
  state := newState;
  end;

function TSVariable.getState: TSVariableState;
  begin
  result := state;
  end;

function TSVariable.hasUseagesForField(col: integer): boolean;
  begin
  result := false;
  case col of
    kRuleContext: result := (self.contextUseages > 0) or (self.moveUseages > 0);
    kRuleCommand: result := self.commandUseages > 0;
    kRuleReply: result := false;
    kRuleMove: result := self.moveUseages > 0;
    kRuleRequirements: result := self.requirementsUseages > 0;
    kRuleChanges: result := self.changesUseages > 0;
  end;
  end;

function TSVariable.meetsDisplayOptions(const displayOptions: TSVariableDisplayOptions): boolean;
  var
  	i: integer;
  begin
  result := false;
  for i := 0 to 5 do
    begin
    if i = kRuleCommand then continue; {don't display commands for now - used to display rules}
    if self.hasUseagesForField(i) and displayOptions[i] then
      begin
      result := true;
      exit;
      end;
    end;
  end;

/////////////////////////// TSDesiredStateVariableWrapperleader /////////////////////////////////////

function TSDesiredStateVariableWrapper.leader: string;
  begin
  if desiredState = kAbsent then
  	result := '~'
  else
  	result := '';
  end;

function TSDesiredStateVariableWrapper.displayLeader: string;
  begin
  if desiredState = kAbsent then
  	result := '~'
  else
  	result := '  ';
  end;

procedure TSDesiredStateVariableWrapper.invertDesiredState;
  begin
  if desiredState = kAbsent then
  	desiredState := kPresent
  else
  	desiredState := kAbsent;
  end;

function TSDesiredStateVariableWrapper.displayString: string;
  begin
  result := self.displayLeader + variable.phrase;
	end;
/////////////////////////// TSChangedVariableWrapper /////////////////////////////////////

constructor TSChangedVariableWrapper.createWithVariableNewState(variable: TSVariable; newState: TSVariableState);
	begin
  self.create;
  self.variable := variable;
  self.newState := newState;
  self.oldState := variable.getState;
  end;

procedure TSChangedVariableWrapper.doChange;
	begin
  variable.setState(newState);
  end;

procedure TSChangedVariableWrapper.undoChange;
	begin
  variable.setState(oldState);
  end;

//////////////////// TSRule ////////////////////////

function TSRule.displayName: string;
  begin
  result := '';
  if domain.options.showCommandPrefixInMap then
  	result := result + '> ';
  result := result + self.command.phrase;
  end;

constructor TSRule.create;
	begin
	requirements := TList.create;
	changes := TList.create;
	end;

procedure TSRule.addUseages;
  var i: integer;
  begin
  inc(context.contextUseages);
  for i := 0 to requirements.count - 1 do
		inc(TSDesiredStateVariableWrapper(requirements[i]).variable.requirementsUseages);
	inc(command.commandUseages);
	inc(move.moveUseages);
  for i := 0 to changes.count - 1 do
		inc(TSDesiredStateVariableWrapper(changes[i]).variable.changesUseages);
  useagesRemoved := false;
  end;

procedure TSRule.removeUseages;
  var i: integer;
  begin
  dec(context.contextUseages);
  for i := 0 to requirements.count - 1 do
		dec(TSDesiredStateVariableWrapper(requirements[i]).variable.requirementsUseages);
	dec(command.commandUseages);
	dec(move.moveUseages);
  for i := 0 to changes.count - 1 do
		dec(TSDesiredStateVariableWrapper(changes[i]).variable.changesUseages);
  useagesRemoved := true;
  end;

destructor TSRule.destroy;
  var i: integer;
	begin
  if not useagesRemoved then
    begin
  	if context <> nil then dec(context.contextUseages);
  	if requirements <> nil then
  		for i := 0 to requirements.count - 1 do
    		begin
				dec(TSDesiredStateVariableWrapper(requirements[i]).variable.requirementsUseages);
    		TSDesiredStateVariableWrapper(requirements[i]).free;
    		end;
		if command <> nil then dec(command.commandUseages);
		if move <> nil then dec(move.moveUseages);
  	if changes <> nil then
  		for i := 0 to changes.count - 1 do
    		begin
				dec(TSDesiredStateVariableWrapper(changes[i]).variable.changesUseages);
    		TSDesiredStateVariableWrapper(changes[i]).free;
    		end;
    end
  else
    begin
  	if requirements <> nil then
  		for i := 0 to requirements.count - 1 do
    		TSDesiredStateVariableWrapper(requirements[i]).free;
  	if changes <> nil then
  		for i := 0 to changes.count - 1 do
    		TSDesiredStateVariableWrapper(changes[i]).free;
    end;
  requirements.free;
	requirements := nil;
	changes.free;
  changes := nil;
  inherited destroy;
	end;

procedure TSRule.setContext(const aString: string);
  begin
  if context <> nil then dec(context.contextUseages);
  context := world.findOrCreateVariable(aString, false);
  inc(context.contextUseages);
  end;

procedure TSRule.setRequirements(const aString: string);
	var i: integer;
  begin
  requirementsString := aString;
  if requirements.count > 0 then for i := 0 to requirements.count - 1 do
    begin
		dec(TSDesiredStateVariableWrapper(requirements[i]).variable.requirementsUseages);
    TSDesiredStateVariableWrapper(requirements[i]).free;
    end;
  requirements.clear;
	self.compile(aString, requirements);
	if requirements.count > 0 then for i := 0 to requirements.count - 1 do
		inc(TSDesiredStateVariableWrapper(requirements[i]).variable.requirementsUseages);
	end;

procedure TSRule.setCommand(const aString: string);
	begin
	if command <> nil then dec(command.commandUseages);
	command := world.findOrCreateVariable(aString, false);
	inc(command.commandUseages);
	end;

procedure TSRule.setReply(const aString: string);
  var
  	i: integer;
    safeString: string;
	begin
  safeString := aString;
  for i := 1 to length(safeString) do
    begin
  	if safeString[i] = chr(13) then
    	safeString[i] := ' ';
  	if safeString[i] = chr(10) then
    	safeString[i] := ' ';
    end;
	reply := safeString;
	end;

procedure TSRule.setMove(const aString: string);
	begin
	if move <> nil then dec(move.moveUseages);
	move := world.findOrCreateVariable(aString, false);
	inc(move.moveUseages);
	end;

procedure TSRule.setChanges(const aString: string);
	var i: integer;
  begin
  changesString := aString;
  if changes.count > 0 then for i := 0 to changes.count - 1 do
    begin
		dec(TSDesiredStateVariableWrapper(changes[i]).variable.changesUseages);
    TSDesiredStateVariableWrapper(changes[i]).free;
    end;
  changes.clear;
	self.compile(aString, changes);
	if changes.count > 0 then for i := 0 to changes.count - 1 do
		inc(TSDesiredStateVariableWrapper(changes[i]).variable.changesUseages);
	end;

procedure TSRule.compile(const aString: string; list: TList);
 var
     phrase: string;
     position: integer;
     remaining: string;
     variable: TSVariable;
     desiredState: TSVariableState;
     wrapper: TSDesiredStateVariableWrapper;
  begin
  remaining := Trim(aString);
  while (Length(Trim(remaining)) > 0) do
    begin
    position := pos('&', remaining);
    if position > 0 then
      phrase := Copy(remaining, 1, position - 1)
    else
      phrase := remaining;
    phrase := trim(Phrase);
    if pos('~', phrase) = 1 then
    	desiredState := kAbsent
    else
    	desiredState := kPresent;
    if desiredState = kAbsent then
    	phrase := Trim(Copy(phrase, 2, Length(phrase)));
    variable := world.findOrCreateVariable(phrase, false);
    // need to distinguish for context list
    if list = requirements then
    	wrapper := TSDesiredStateVariableWrapper.create
    else
    	wrapper := TSDesiredStateVariableWrapper.create;
    wrapper.variable := variable;
    wrapper.desiredState := desiredState;
    list.add(wrapper);
    if position > 0 then
       remaining := Copy(remaining, position+1, Length(remaining))
    else
      remaining := '';
    end;
  end;

function TSRule.decompile(list: TList): string;
  var
    i: integer;
     wrapper: TSDesiredStateVariableWrapper;
    item: string;
  begin
  result := '';
  for i := 0 to list.count - 1 do
    begin
    // OK to cast requirements as this
    wrapper := TSDesiredStateVariableWrapper(list[i]);
    item := wrapper.leader + wrapper.variable.phrase;
    if result <> '' then
      result := result + ' & ' +  item
    else
      result := item;
    end;
  end;

function TSRule.decompileRequirements: string;
  begin
  result := self.decompile(requirements);
  end;

function TSRule.decompileChanges: string;
  begin
  result := self.decompile(changes);
  end;

procedure TSRule.updateAvailable;
   var
     i: integer;
      wrapper: TSDesiredStateVariableWrapper;
   begin
   // assuming all field set up correctly and not nil
   available := false;
   if context = world.emptyEntry then exit; {for now - really should do anyway - assuming unfinished}
   if context.state <> kPresent then exit;
   i := 0;
   while i < requirements.Count do
     begin
     wrapper := TSDesiredStateVariableWrapper(requirements.items[i]);
     if wrapper.variable.getState <> wrapper.desiredState then exit;
     i := i + 1;
     end;
   available := true;
   end;

procedure TSRule.recordReplyMoveChanges(changedVariablesList: TList; var totalReply: string; var contextToFocusTo: TSVariable);
	var
		i: integer;
		desiredStateWrapper: TSDesiredStateVariableWrapper;
		changedVariableWrapper: TSChangedVariableWrapper;
	begin
	if (totalReply <> '') and (self.reply <> '') then
		totalReply := totalReply + ' ';
	totalReply := totalReply + self.reply;
	if move <> world.emptyEntry then
		contextToFocusTo := move;
	i := 0;
	while i < changes.Count do
		begin
		desiredStateWrapper := TSDesiredStateVariableWrapper(changes[i]);
		changedVariableWrapper :=
				TSChangedVariableWrapper.createWithVariableNewState(desiredStateWrapper.variable, desiredStateWrapper.desiredState);
		changedVariablesList.add(changedVariableWrapper);
		i := i + 1;
		end;
	end;

procedure TSRule.setTextForField(col: integer; const text: string);
  begin
  case col of
    kRuleContext: self.setContext(text);
    kRuleCommand: self.setCommand(text);
    kRuleReply: self.setReply(text);
    kRuleMove: self.setMove(text);
    kRuleRequirements: self.setRequirements(text);
    kRuleChanges: self.setChanges(text);
  end;
  end;

function TSRule.getTextForField(col: integer): string;
  begin
  case col of
    kRuleContext: result := self.context.phrase;
    kRuleCommand: result := self.command.phrase;
    kRuleReply: result := self.reply;
    kRuleMove: result := self.move.phrase;
    kRuleRequirements: result := self.requirementsString;
    kRuleChanges: result := self.changesString;
  end;
  end;

class function TSRule.headerForField(col: integer): string;
  begin
  case col of
    kRuleContext: result := 'Context';
    kRuleCommand: result := 'Command';
    kRuleReply: result := 'Reply';
    kRuleMove: result := 'Move';
    kRuleRequirements: result := 'Requirements';
    kRuleChanges: result := 'Changes';
    end;
  end;

function TSRule.usesVariableInList(variable: TSVariable; list: TList): boolean;
  var
     i: integer;
     wrapper: TSDesiredStateVariableWrapper;
  begin
  result := true;
  for i := 0 to list.Count - 1 do
    begin
    wrapper := TSDesiredStateVariableWrapper(list.items[i]);
    if wrapper.variable = variable then exit;
    end;
  result := false;
  end;

function TSRule.usesVariableFor(variable: TSVariable; col: integer): boolean;
  begin
  result := false;
  case col of
    kRuleContext: result := self.context = variable;
    kRuleCommand: result := self.command = variable;
    kRuleReply: result := false;  // error
    kRuleMove: result := self.move = variable;
    kRuleRequirements: result := self.usesVariableInList(variable, requirements);
    kRuleChanges: result := self.usesVariableInList(variable, changes);
    end;
  end;

function TSRule.variableInList(n: integer; list: TList): TSVariable;
  var
    wrapper: TSDesiredStateVariableWrapper;
  begin
  if n < 0 then n := 0;
  if list.Count > n then
    begin
    wrapper := TSDesiredStateVariableWrapper(list.items[n]);
    result := wrapper.variable;
    end
  else
    result := world.emptyEntry;
  end;

function TSRule.variableForFieldWithSelections(col: integer; requirementsIndex, changesIndex: integer): TSVariable;
  begin
  result := world.emptyEntry;
  case col of
    kRuleContext: result := self.context;
    kRuleCommand: result := self.command;
    kRuleReply: result := world.emptyEntry;  // error
    kRuleMove: result := self.move;
    kRuleRequirements: result := self.variableInList(requirementsIndex, requirements);
    kRuleChanges: result := self.variableInList(changesIndex, changes);
    end;
  end;

function TSRule.variableForField(col: integer): TSVariable;
  begin
  result := variableForFieldWithSelections(col, 0, 0);
	end;

procedure TSRule.setPosition(value: string);
  var
  	firstPart, secondPart, thirdPart, rest: string;
  begin
  firstPart := Copy(value, 1, pos('|', value) - 1);
  rest := Copy(value, length(firstPart) + 2, length(value));
  secondPart := Copy(rest, 1, pos('|', rest) - 1);
  thirdPart := Copy(rest, length(secondPart) + 2, length(rest));
  inherited setPosition(firstPart);
  self.context.setPosition(secondPart);
  self.move.setPosition(thirdPart);
  end;


//////////////////// TSIndexChangeRuleWrapper ///////////////////////////

constructor TSIndexChangeRuleWrapper.createWithRuleNewIndex(rule: TSRule; newIndex: integer);
  begin
  self.rule := rule;
  self.oldIndex := rule.world.rules.indexOf(rule);
  self.newIndex := newIndex;
  end;

procedure TSIndexChangeRuleWrapper.doChange;
  begin
  if oldIndex = newIndex then exit;
  if newIndex >= 0 then
  	rule.world.rules.move(oldIndex, newIndex)
  else
  	rule.world.rules.delete(oldIndex);
  end;

procedure TSIndexChangeRuleWrapper.undoChange;
  begin
  if oldIndex = newIndex then exit;
  if newIndex >= 0 then
  	rule.world.rules.move(newIndex, oldIndex)
  else
  	rule.world.rules.insert(oldIndex, rule);
  end;


//////////////////// TWorld ///////////////////////////

constructor TWorld.Create;
  begin
  variables := TList.create;
  rules := TList.create;
  emptyEntry := TSVariable.create;
  end;

procedure TWorld.resetVariableValues;
 	var i: integer;
  begin
	if variables <> nil then
  	begin
  	if variables.Count > 0 then
  		for i := 0 to variables.Count - 1 do
  			TSVariable(variables.items[i]).state := kAbsent;
  	end;
  end;

procedure TWorld.resetVariablesAndRules;
 	var i: integer;
  begin
	if rules <> nil then
  	begin
  	if rules.Count > 0 then
  		for i := 0 to rules.Count - 1 do
  			TSRule(rules.items[i]).free;
    rules.clear;
  	end;
	if variables <> nil then
  	begin
  	if variables.Count > 0 then
  		for i := 0 to variables.Count - 1 do
  			TSVariable(variables.items[i]).free;
    variables.clear;
  	end;
  end;

destructor TWorld.Destroy;
	begin
  self.resetVariablesAndRules;
  rules.free;
  rules := nil;
  variables.free;
  variables := nil;
  emptyEntry.free;
  emptyEntry := nil;
  inherited destroy;
 	end;

function TWorld.newRule: TSRule;
  begin
  result := TSRule.create;
  result.world := self;
  result.context := self.emptyEntry;
  result.command := self.emptyEntry;
  result.move := self.emptyEntry;
  if RuleEditorForm <> nil then
  	result.position := RuleEditorForm.goodPosition;
  rules.add(result);
  end;

function TWorld.findVariable(const aString: string): TSVariable;
  var i: integer;
  begin
  result := nil;
  if aString = '' then begin result := self.emptyEntry; exit; end;
  if variables.count > 0 then
    for i := 0 to variables.Count - 1 do
      if AnsiCompareText(TSVariable(variables.items[i]).phrase, aString) = 0 then
        begin
        result := TSVariable(variables.items[i]);
        // take on the case of the last choice if not the same
        if TSVariable(variables.items[i]).phrase <> aString then
        	TSVariable(variables.items[i]).setPhrase(aString);
        exit;
        end;
  end;

function TWorld.findOrCreateVariable(const aString: string; madeByMacro: boolean): TSVariable;
  begin
  result := self.findVariable(trim(aString));
  if result <> nil then exit;
  result := TSVariable.create;
  result.world := self;
  result.setPhrase(trim(aString));
  result.state := kAbsent;   // directly set for now - otherwise circular error on startup...
  if RuleEditorForm <> nil then
  	result.position := RuleEditorForm.goodPosition;
  variables.add(result);
  lastVariableCreated := aString;
  end;

procedure TWorld.setInitialFocus;
  begin
  if self.rules.count > 0 then
    begin
  	self.focus := TSRule(self.rules[0]).context;
  	self.previousFocus := TSRule(self.rules[0]).context;
    self.focus.state := kPresent;
		self.updateAvailable;
    end
  else
    begin
  	self.focus := nil;
  	self.previousFocus := nil;
    end;
  end;

procedure TWorld.newWorld;
  begin
  self.resetVariablesAndRules;
  self.focus := nil;
  self.previousFocus := nil;
  end;

function TWorld.loadWorldFromFile(const name: string): boolean;
	var
  	WorldFile: TextFile;
  	value: string;
  	rule: TSRule;
  	count: integer;
    header: string;
	begin
  result := false;
  consoleForm.reportMode('Loading');
	AssignFile(WorldFile, name);
	Reset(WorldFile);
  try
  // done by caller to allow merges
  //self.resetVariablesAndRules;
  count := 0;
   // unfinished - need better error checking
  readln(WorldFile, header);
  if (header <> '; world file version 1.0') and
     (header <> '; StoryHarp world file version 1.3') then
    begin
  	ShowMessage('File header for world file is not correct');
    exit;
    end;
	while not eof(WorldFile) do
  	begin
  	readln(WorldFile, value);
    if (value <> '') and (value[1] = ';') then
    	continue;
  	if value <> '====================' then exit;
    if count = 0 then
      begin
  		readln(WorldFile, value); // context
  		readln(WorldFile, value); // command
  		readln(WorldFile, value); // reply
  		readln(WorldFile, value); // move to
  		readln(WorldFile, value); // extra changes
  		readln(WorldFile, value); // extra requirements
  		readln(WorldFile, value); // map positions
      end
    else
      begin
    	rule := self.newRule;
  		readln(WorldFile, value);
  		rule.setContext(Trim(value));
  		readln(WorldFile, value);
  		rule.setCommand(Trim(value));
  		readln(WorldFile, value);
  		rule.setReply(Trim(value));
  		readln(WorldFile, value);
  		rule.setMove(Trim(value));
  		readln(WorldFile, value);
  		rule.setChanges(Trim(value));
  		readln(WorldFile, value);
  		rule.setRequirements(Trim(value));
  		readln(WorldFile, value);
  		rule.setPosition(Trim(value));
      end;
    count := count + 1;
 	end;
  finally
	CloseFile(WorldFile);
  consoleForm.reportMode('Running');
  end;
  result := true;
  end;

procedure TWorld.saveWorldToFile(const name: string; saveOnlySelectedRules: boolean);
	var
  	i: integer;
  	WorldFile: TextFile;
    rule: TSRule;
	begin
	AssignFile(WorldFile, name);
  Rewrite(WorldFile);
  consoleForm.reportMode('Saving');
  try
    // 1.0 had all lower case
    // 1.3 supports mixed case
  	writeln(WorldFile, '; StoryHarp world file version 1.3');
  	writeln(WorldFile, '====================');
  	for i := 0 to 5 do
    	writeln(WorldFile, TSRule.headerForField(i));
  	writeln(WorldFile, 'map positions');
  	for i := 0 to rules.count - 1 do
    	begin
    	rule := TSRule(rules.items[i]);
    	if saveOnlySelectedRules and not rule.selected then continue;
    	writeln(WorldFile, '====================');
    	writeln(WorldFile, rule.context.phrase);
    	writeln(WorldFile, rule.command.phrase);
    	writeln(WorldFile, rule.reply);
    	writeln(WorldFile, rule.move.phrase);
    	writeln(WorldFile, rule.changesString);
    	writeln(WorldFile, rule.requirementsString);
    	writeln(WorldFile, rule.position.x, ',', rule.position.y, '|', rule.context.position.x, ',', rule.context.position.y, '|', rule.move.position.x, ',', rule.move.position.y);
    	end;
  	Flush(WorldFile);
  finally
		CloseFile(WorldFile);
  	consoleForm.reportMode('Running');
  end;
  end;

procedure TWorld.newSession;
  begin
  self.resetVariableValues;
  self.setInitialFocus;
  end;

function findCompleteWorldFileName(const worldFileNameRead: string): string;
  begin
  if not FileExists(worldFileNameRead) then
		result := getFileOpenInfo(kFileTypeWorld, worldFileNameRead,
    	'Search for world file: ' + worldFileNameRead, kOtherExtNotOk)
  else
    result := worldFileNameRead;
  end;

function TWorld.loadSessionFromFile(const name: string; const worldFileName: string): boolean;
	var
  	SessionFile: TextFile;
    variable: TSVariable;
    header: string;
  	worldFileNameRead, variableNameRead, focusNameRead, previousFocusNameRead: string;
    completeWorldFileName: string;
  begin
  result := false;
  AssignFile(SessionFile, name);
  Reset(SessionFile);
  try
  self.resetVariableValues;
  // unfinished - need better error checking
  readln(SessionFile, header);
  if header <> '; session file version 1.0' then
    begin
  	ShowMessage('File header for session file is not correct');
    exit;
    end;
  readln(SessionFile, header);
  if header <> '============ Variables for world =================' then exit;
  readln(SessionFile, worldFileNameRead);
  if worldFileNameRead <> worldFileName then
    begin
    completeWorldFileName := findCompleteWorldFileName(worldFileNameRead);
    if completeWorldFileName <> '' then
      begin
    	RuleEditorForm.openWorldFile(completeWorldFileName);
  		domain.sessionFileName := name; // to counteract resetting session when load world
      end
    else
    	exit;
    end;
  readln(SessionFile, header);
  if header <> '============ Focus ===============================' then exit;
  readln(SessionFile, focusNameRead);
  readln(SessionFile, previousFocusNameRead);
  readln(SessionFile, header);
  if header <> '============ Variables ===========================' then exit;
	while not eof(SessionFile) do
  	begin
  	readln(SessionFile, variableNameRead);
    variableNameRead := trim(variableNameRead);
    variable := self.findOrCreateVariable(variableNameRead, false);
    variable.state := kPresent;
 		end;
  finally
	CloseFile(SessionFile);
  end;
  self.focus := self.findOrCreateVariable(focusNameRead, false);
  self.previousFocus := self.findOrCreateVariable(previousFocusNameRead, false);
	self.updateAvailable;
  result := true;
  end;

procedure TWorld.saveSessionToFile(const name: string; const worldFileName: string);
	var
  	i: integer;
  	SessionFile: TextFile;
    variable: TSVariable;
  begin
  AssignFile(SessionFile, name);
  Rewrite(SessionFile);
   try
  	writeln(SessionFile, '; session file version 1.0');
  	writeln(SessionFile, '============ Variables for world =================');
  	writeln(SessionFile, worldFileName);
  	writeln(SessionFile, '============ Focus ===============================');
  	writeln(SessionFile, focus.phrase);
  	writeln(SessionFile, previousFocus.phrase);
  	writeln(SessionFile, '============ Variables ===========================');
  	for i := 0 to variables.count - 1 do
    	begin
      variable := TSVariable(variables.items[i]);
      if variable.state = kPresent then
    		writeln(SessionFile, variable.phrase);
    	end;
  	Flush(SessionFile);
   finally
		CloseFile(SessionFile);
   end;
  end;

procedure TWorld.updateAvailable;
  var
  	rule: TSRule;
    i: integer;
  begin
  if rules.count > 0 then
    for i := 0 to rules.count - 1 do
      begin
      rule := TSRule(rules.items[i]);
      rule.updateAvailable;
      end;
  end;

procedure TWorld.setFocusTo(contextToFocusOn: TSVariable);
  begin
  if contextToFocusOn <> nil then
    begin
    previousFocus := focus;
    previousFocus.setState(kAbsent);
    focus := contextToFocusOn;
    focus.setState(kPresent);
    end;
  end;

// retruns whether should redraw grid
function TWorld.deselectAllExcept(exceptObject: TSDraggableObject): boolean;
  var
  	rule: TSRule;
    variable: TSVariable;
    i: integer;
  begin
  result := false;
  for i := 0 to rules.count - 1 do
  	begin
    rule := TSRule(rules[i]);
    if (rule.selected) and (rule <> exceptObject) then
    	begin
    	rule.selected := false;
    	result := true;
    	end;
    end;
  for i := 0 to variables.count - 1 do
  	begin
    variable := TSVariable(variables[i]);
    if (variable.selected) and (variable <> exceptObject) then
    	begin
    	variable.selected := false;
    	result := true;
    	end;
    end;
  end;

procedure TWorld.addDragRecordsToList(dragRecords: TList);
  var
  	rule: TSRule;
    variable: TSVariable;
    i: integer;
  begin
  for i := 0 to rules.count - 1 do
  	begin
    rule := TSRule(rules[i]);
    if (rule.selected) then
      dragRecords.add(TSDragRecord.createWithNode(rule));
    end;
  for i := 0 to variables.count - 1 do
  	begin
    variable := TSVariable(variables[i]);
    if (variable.selected) then
      dragRecords.add(TSDragRecord.createWithNode(variable));
    end;
  end;

procedure TWorld.deleteSelectedRules;
  var
  	rule: TSRule;
    i: integer;
    command: TSDeleteRulesCommand;
  begin
  command := TSDeleteRulesCommand.create;
  for i := 0 to rules.count - 1 do
    begin
    rule := TSRule(rules[i]);
    if (rule.selected) then
      command.addRule(rule, -1);
    end;
  if command.ruleWrappers.count > 0 then
    domain.worldCommandList.doCommand(command)
  else
    command.free;
  end;

procedure TWorld.raiseSelectedRules;
  var
  	rule: TSRule;
  	higherRule: TSRule;
    i: integer;
    command: TSMoveRulesCommand;
    moving: boolean;
  begin
  command := TSMoveRulesCommand.create;
  command.action := 'raise';
  moving := false;
  {skip first}
  for i := 1 to rules.count - 1 do
    begin
    rule := TSRule(rules[i]);
    if rule.selected then
      begin
      if not moving then
        begin
    		higherRule := TSRule(rules[i - 1]);
    		if not higherRule.selected then
          moving := true;
        end;
      if moving then
        command.addRule(rule, i - 1);
      end
    else moving := true;
    end;
  if command.ruleWrappers.count > 0 then
    domain.worldCommandList.doCommand(command)
  else
    command.free;
  end;

procedure TWorld.lowerSelectedRules;
  var
  	rule: TSRule;
  	lowerRule: TSRule;
    i: integer;
    command: TSMoveRulesCommand;
    moving: boolean;
  begin
  command := TSMoveRulesCommand.create;
  command.action := 'lower';
  moving := false;
  {skip first}
  for i := rules.count - 2 downto 0 do
    begin
    rule := TSRule(rules[i]);
    if rule.selected then
      begin
      if not moving then
        begin
    		lowerRule := TSRule(rules[i + 1]);
    		if not lowerRule.selected then
          moving := true;
        end;
      if moving then
        command.addRule(rule, i + 1);
      end
    else moving := true;
    end;
  if command.ruleWrappers.count > 0 then
    domain.worldCommandList.doCommand(command)
  else
    command.free;
  end;

procedure TWorld.selectAvailable;
  var
  	rule: TSRule;
    i: integer;
  begin
  for i := 0 to rules.count - 1 do
  	begin
    rule := TSRule(rules[i]);
    rule.selected := rule.available;
    end;
  end;

function TWorld.firstAvailable: TSRule;
  var
  	rule: TSRule;
    i: integer;
  begin
  for i := 0 to rules.count - 1 do
  	begin
    rule := TSRule(rules[i]);
    if rule.available then
      begin
      result := rule;
      exit;
      end;
    end;
  result := nil;
  end;

procedure swapIntegers(var a: integer; var b: integer);
  var temp: integer;
  begin
  temp := a;
  a := b;
  b := temp;
  end;

procedure TWorld.selectInRectangle(rect: TRect);
  var
    intersection: TRect;
    i: integer;
  	rule: TSRule;
    variable: TSVariable;
  begin
  if rect.right < rect.left then
  	swapIntegers(rect.left, rect.right);
  if rect.bottom < rect.top then
  	swapIntegers(rect.top, rect.bottom);
  for i := 0 to rules.count - 1 do
  	begin
    rule := TSRule(rules[i]);
    IntersectRect(intersection, rule.bounds, rect);
    if not IsRectEmpty(intersection) then
    	begin
    	rule.selected := true;
    	end;
    end;
  for i := 0 to variables.count - 1 do
  	begin
    variable := TSVariable(variables[i]);
    IntersectRect(intersection, variable.bounds, rect);
    if not IsRectEmpty(intersection) then
    	begin
    	variable.selected := true;
    	end;
    end;
  end;

function TWorld.firstSelectedVariable: TSVariable;
  var
    i: integer;
    variable: TSVariable;
  begin
  result := nil;
  for i := 0 to variables.count - 1 do
  	begin
    variable := TSVariable(variables[i]);
    if variable.selected then
      begin
      result := variable;
      exit;
      end;
    end;
  end;

function TWorld.firstSelectedObject: TSDraggableObject;
  var
    i: integer;
    variable: TSVariable;
    rule: TSRule;
  begin
  result := nil;
  for i := 0 to variables.count - 1 do
  	begin
    variable := TSVariable(variables[i]);
    if variable.selected then
      begin
      result := variable;
      exit;
      end;
    end;
  for i := 0 to rules.count - 1 do
  	begin
    rule := TSRule(rules[i]);
    if rule.selected then
      begin
      result := rule;
      exit;
      end;
    end;
  end;

procedure TWorld.addContextsToCombBox(comboBox: TComboBox);
  var
    i: integer;
    variable: TSVariable;
  begin
  comboBox.clear;
  for i := 0 to variables.count - 1 do
  	begin
    variable := TSVariable(variables[i]);
    if variable.contextUseages > 0 then
      begin
      comboBox.items.addObject(variable.phrase, variable);
      end;
    end;
  end;

procedure TWorld.addContextsToListBox(listBox: TListBox);
  var
    i: integer;
    variable: TSVariable;
  begin
  listBox.clear;
  for i := 0 to variables.count - 1 do
  	begin
    variable := TSVariable(variables[i]);
    if variable.contextUseages > 0 then
      begin
      listBox.items.addObject(variable.phrase, variable);
      end;
    end;
  end;

function TWorld.boundsRect: TRect;
  var
    node: TSDraggableObject;
    i: integer;
  begin
  with result do
    begin
  	top := 0;
  	bottom := 0;
  	left := 0;
  	right := 0;
    for i := 0 to variables.count - 1 do
      begin
      node := variables[i];
      if left > node.position.x then
      	left := node.position.x;
      if right < node.position.x + node.extent.x then
      	right := node.position.x + node.extent.x;
      if top > node.position.y then
      	top := node.position.y;
      if bottom < node.position.y + node.extent.y then
      	bottom := node.position.y + node.extent.y;
      end;
    for i := 0 to rules.count - 1 do
      begin
      node := rules[i];
      if left > node.position.x then
      	left := node.position.x;
      if right < node.position.x + node.extent.x then
      	right := node.position.x + node.extent.x;
      if top > node.position.y then
      	top := node.position.y;
      if bottom < node.position.y + node.extent.y then
      	bottom := node.position.y + node.extent.y;
      end;
    end;
  end;

procedure TWorld.updateVariablesForIndexInVariables;
  var
    i: integer;
    variable: TSVariable;
  begin
  for i := 0 to variables.count - 1 do
  	begin
    variable := TSVariable(variables[i]);
    variable.indexInVariables := i;
    end;
  end;

end.
