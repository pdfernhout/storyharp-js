unit uscommands;

interface

uses Windows, Graphics, Classes, Controls, SysUtils, StdCtrls, UCommand, USWorld, 
		USFocusCommands, USVariableCommands;

type

{TEditChangeCommand = class(KfCommand)
  	entryField: TObject;
    oldSelectionInformation: TSelectionInformation;
    newSelectionInformation: TSelectionInformation;
		closed: boolean;
		procedure recordChangesMadeToEdit;
		constructor createWithEdit(Sender: TObject;  const lastSelectionInformation: TSelectionInformation);
 		procedure doCommand; override;
		procedure undoCommand; override;
		procedure redoCommand; override;
		function description: string; override;
		end;

  	entryField: TObject;
    newSelectionInformation: TSelectionInformation;
		constructor createWithRule(Sender: TObject;  const lastSelectionInformation: TSelectionInformation);
   }

	TSRuleFieldChange = class(KfCommand)
    rule: TSRule;
    field: integer;
    oldValue: string;
    newValue: string;
		constructor createWithRule(rule: TSRule; field: integer; const newValue: string);
    procedure updateEditorForChange;
 		procedure doCommand; override;
		procedure undoCommand; override;
		procedure redoCommand; override;
		function description: string; override;
    end;

	TSNewRulesCommand = class(KfCommand)
    rules: TList;
    creator: string;
    constructor create;
    procedure addRule(rule: TSRule);
    destructor destroy; override;
  	procedure doCommand; override;
  	procedure undoCommand; override;
  	procedure redoCommand; override;
  	function description: string; override;
    end;

	TSDeleteRulesCommand = class(KfCommand)
    ruleWrappers: TList;
    constructor create;
    procedure addRule(rule: TSRule; newIndex: integer);
    destructor destroy; override;
  	procedure doCommand; override;
  	procedure undoCommand; override;
  	procedure redoCommand; override;
  	function description: string; override;
    end;

	TSMoveRulesCommand = class(KfCommand)
    ruleWrappers: TList;
    action: string;
    constructor create;
    procedure addRule(rule: TSRule; newIndex: integer);
    destructor destroy; override;
  	procedure doCommand; override;
  	procedure undoCommand; override;
  	procedure redoCommand; override;
  	function description: string; override;
    end;

	TSMapDragCommand = class(KfCommand)
    dragRecords: TList;
  	notifyProcedure: TCommandEvent;
    constructor create;
    destructor destroy; override;
  	procedure doCommand; override;
  	procedure undoCommand; override;
  	procedure redoCommand; override;
  	function description: string; override;
  	function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  		mouseDidMove, rightButtonDown: boolean): KfCommand; override;
    end;

	TSCommandList = class(KfCommandList)
    function toggleVariable(variable: TSVariable): TSToggleVariableCommand;
    function moveFocus(newFocus: TSVariable): TSMoveFocusCommand;
		function doCommandPhrase(const commandPhrase: string): TSDoCommandPhrase;
		function ruleFieldChange(rule: TSRule; field: integer; const newValue: string): TSRuleFieldChange;
    end;

implementation

uses USDomain, USConsoleForm, USSpeech, USRuleEditorForm, USChangeLog;

{ TSRuleFieldChange ------------------------------------------}
constructor TSRuleFieldChange.createWithRule(rule: TSRule; field: integer; const newValue: string);
  begin
  create;
  self.rule := rule;
  self.field := field;
  self.oldValue := rule.getTextForField(field);
  self.newValue := newValue;
  end;

procedure TSRuleFieldChange.updateEditorForChange;
  begin
  RuleEditorForm.rule := rule;
  RuleEditorForm.loadAllRuleFields;
  
  if (field = kRuleContext) or (field = kRuleMove) then
    ConsoleForm.locationCacheValid := false;
    
 	{wrapper entries will get freed if list box - so reset them}
 	if field = kRuleRequirements then
  	RuleEditorForm.fillListBox(RuleEditorForm.requirementsListBox, rule.requirements)
  else if field = kRuleChanges then
		RuleEditorForm.fillListBox(RuleEditorForm.changesListBox, rule.changes);

 	RuleEditorForm.RuleGrid.invalidate;
 	RuleEditorForm.MapPaintBoxChanged;
 	RuleEditorForm.SecondListBox.invalidate; // could optimize to only do in certain cases
  if RuleEditorForm.organizeByField = field then   // could optimize to only do when browser visible
    RuleEditorForm.setOrganizeByField(RuleEditorForm.organizeByField);
  end;

procedure TSRuleFieldChange.doCommand;
  begin
  Domain.world.lastVariableCreated := '';
  rule.setTextForField(field, newValue);
  // log changes
  if field <> kRuleReply then
  	ChangeLogForm.addToLog(Domain.world.lastVariableCreated)
  else
    ChangeLogForm.addToLog(newValue);
  self.updateEditorForChange;
  inherited doCommand;
  end;

procedure TSRuleFieldChange.undoCommand;
  begin
  rule.setTextForField(field, oldValue);
  self.updateEditorForChange;
  RuleEditorForm.selectEditorField(field);
  inherited undoCommand;
  end;

procedure TSRuleFieldChange.redoCommand;
  begin
  rule.setTextForField(field, newValue);
  self.updateEditorForChange;
  RuleEditorForm.selectEditorField(field);
  inherited doCommand;
  end;

function TSRuleFieldChange.description: string;
  begin
//  result := 'rule ' + IntToStr(domain.world.rules.indexOf(rule) + 1) + ' change of ' + TSRule.headerForField(field);
  result := 'Change ' + TSRule.headerForField(field) + ' For Rule ' + IntToStr(domain.world.rules.indexOf(rule) + 1);
  end;


{ TSNewRulesCommand ------------------------------------------}

constructor TSNewRulesCommand.create;
  begin
  rules := TList.create;
  end;

destructor TSNewRulesCommand.destroy;
  var
  	i: integer;
  begin
  if not self.done then
  	if rules <> nil then
    	for i := 0 to rules.count - 1 do
      	TObject(rules[i]).free;
  rules.free;
  rules := nil;
  end;

procedure TSNewRulesCommand.addRule(rule: TSRule);
  begin
  rules.add(rule);
  end;

procedure TSNewRulesCommand.doCommand;
  begin
  {already added at start}
	inherited doCommand;
  RuleEditorForm.updateForRuleChange;
  RuleEditorForm.scrollGridSelectionsIntoView(kFromBottom);
  end;

procedure TSNewRulesCommand.undoCommand;
  var
  	i: integer;
    rule: TSRule;
  begin
  for i := 0 to rules.count - 1 do
    begin
    rule := TSRule(rules[i]);
    domain.world.rules.remove(rule);
    rule.selected := false;
    rule.removeUseages;
    end;
  inherited undoCommand;
  if rules.IndexOf(RuleEditorForm.rule) >= 0 then
    RuleEditorForm.editRule(nil); 
  RuleEditorForm.updateForRuleChange;
  end;

procedure TSNewRulesCommand.redoCommand;
  var
  	i: integer;
    rule: TSRule;
  begin
  domain.world.deselectAllExcept(nil);
  for i := 0 to rules.count - 1 do
    begin
    rule := TSRule(rules[i]);
    rule.selected := true;
    domain.world.rules.add(rule);
    rule.addUseages;
    end;
	inherited doCommand;
  RuleEditorForm.updateForRuleChange;
  RuleEditorForm.scrollGridSelectionsIntoView(kFromBottom);
  //if rules.count > 0 then
  //  RuleEditorForm.editRule(rules[rules.count - 1]);
  end;

function TSNewRulesCommand.description: string;
  begin
  if rules.count > 1 then
    result := 'new rules'
  else if rules.count = 1 then
    result := 'new rule'
  else
    result := 'new rule';
  if creator <> '' then
    result := result + ' from ' + creator;
  if creator = 'duplicating' then
    result := 'duplicate rule';
  end;

{ TSDeleteRulesCommand ------------------------------------------}

constructor TSDeleteRulesCommand.create;
  begin
  ruleWrappers := TList.create;
  end;

destructor TSDeleteRulesCommand.destroy;
  var
  	i: integer;
    rule: TSRule;
    wrapper: TSIndexChangeRuleWrapper;
  begin
  if ruleWrappers <> nil then
    for i := 0 to ruleWrappers.count - 1 do
      begin
      wrapper := TSIndexChangeRuleWrapper(ruleWrappers[i]);
      rule := wrapper.rule;
      if self.done then
        rule.free;
      wrapper.free;
      end;
  ruleWrappers.free;
  ruleWrappers := nil;
  end;

procedure TSDeleteRulesCommand.addRule(rule: TSRule; newIndex: integer);
  var
    wrapper: TSIndexChangeRuleWrapper;
  begin
  wrapper := TSIndexChangeRuleWrapper.createWithRuleNewIndex(rule, newIndex);
  ruleWrappers.add(wrapper);
  end;

procedure TSDeleteRulesCommand.doCommand;
  var
  	i: integer;
    wrapper: TSIndexChangeRuleWrapper;
  begin
  for i := ruleWrappers.count - 1 downto 0 do
    begin
    wrapper := TSIndexChangeRuleWrapper(ruleWrappers[i]);
    if (wrapper.rule = RuleEditorForm.rule) then
    	RuleEditorForm.editRule(nil);
    wrapper.rule.removeUseages;
    wrapper.doChange;
    end;
	inherited doCommand;
  RuleEditorForm.updateForRuleChange;
  end;

procedure TSDeleteRulesCommand.undoCommand;
  var
  	i: integer;
    wrapper: TSIndexChangeRuleWrapper;
  begin
  domain.world.deselectAllExcept(nil);
  for i := 0 to ruleWrappers.count - 1 do
    begin
    wrapper := TSIndexChangeRuleWrapper(ruleWrappers[i]);
    wrapper.rule.addUseages;
    wrapper.undoChange;
    wrapper.rule.selected := true;
    end;
  if ruleWrappers.count > 0 then
    	RuleEditorForm.editRule(TSIndexChangeRuleWrapper(ruleWrappers[0]).rule);
  inherited undoCommand;
  RuleEditorForm.updateForRuleChange;
  RuleEditorForm.scrollGridSelectionsIntoView(kFromTop)
  end;

procedure TSDeleteRulesCommand.redoCommand;
  var
  	i: integer;
    wrapper: TSIndexChangeRuleWrapper;
  begin
  domain.world.deselectAllExcept(nil);
  for i := ruleWrappers.count - 1 downto 0 do
    begin
    wrapper := TSIndexChangeRuleWrapper(ruleWrappers[i]);
    if (wrapper.rule = RuleEditorForm.rule) then
    	RuleEditorForm.editRule(nil);
    wrapper.rule.removeUseages;
    wrapper.doChange;
    end;
	inherited doCommand;
  RuleEditorForm.updateForRuleChange;
  end;

function TSDeleteRulesCommand.description: string;
  begin
  if ruleWrappers.count > 1 then
    result := 'delete rules'
  else if ruleWrappers.count = 1 then
    result := 'delete rule'
  else
    result := 'delete rule';
  end;

{ TSMoveRulesCommand ------------------------------------------}

constructor TSMoveRulesCommand.create;
  begin
  ruleWrappers := TList.create;
  end;

destructor TSMoveRulesCommand.destroy;
  var
  	i: integer;
    wrapper: TSIndexChangeRuleWrapper;
  begin
  if ruleWrappers <> nil then
    for i := 0 to ruleWrappers.count - 1 do
      begin
      wrapper := TSIndexChangeRuleWrapper(ruleWrappers[i]);
      wrapper.free;
      end;
  ruleWrappers.free;
  ruleWrappers := nil;
  end;

procedure TSMoveRulesCommand.addRule(rule: TSRule; newIndex: integer);
  var
    wrapper: TSIndexChangeRuleWrapper;
  begin
  wrapper := TSIndexChangeRuleWrapper.createWithRuleNewIndex(rule, newIndex);
  ruleWrappers.add(wrapper);
  end;

procedure TSMoveRulesCommand.doCommand;
  var
  	i: integer;
    wrapper: TSIndexChangeRuleWrapper;
  begin
  for i := 0 to ruleWrappers.count - 1 do
    begin
    wrapper := TSIndexChangeRuleWrapper(ruleWrappers[i]);
    wrapper.doChange;
    end;
	inherited doCommand;
  RuleEditorForm.RuleGrid.invalidate;
  if action = 'raise' then
  	RuleEditorForm.scrollGridSelectionsIntoView(kFromTop)
  else
  	RuleEditorForm.scrollGridSelectionsIntoView(kFromBottom);
  RuleEditorForm.updateRuleNumberLabel;
  end;

procedure TSMoveRulesCommand.undoCommand;
  var
  	i: integer;
    wrapper: TSIndexChangeRuleWrapper;
  begin
  domain.world.deselectAllExcept(nil);
  for i := ruleWrappers.count - 1 downto 0 do
    begin
    wrapper := TSIndexChangeRuleWrapper(ruleWrappers[i]);
    wrapper.rule.selected := true;
    wrapper.undoChange;
    end;
  inherited undoCommand;
  RuleEditorForm.RuleGrid.invalidate;
  if action = 'raise' then
  	RuleEditorForm.scrollGridSelectionsIntoView(kFromBottom)
  else
  	RuleEditorForm.scrollGridSelectionsIntoView(kFromTop);
  RuleEditorForm.updateRuleNumberLabel;
  end;

procedure TSMoveRulesCommand.redoCommand;
  var
  	i: integer;
    wrapper: TSIndexChangeRuleWrapper;
  begin
  domain.world.deselectAllExcept(nil);
  for i := 0 to ruleWrappers.count - 1 do
    begin
    wrapper := TSIndexChangeRuleWrapper(ruleWrappers[i]);
    wrapper.rule.selected := true;
    wrapper.doChange;
    end;
	inherited doCommand;
  RuleEditorForm.RuleGrid.invalidate;
  if action = 'raise' then
  	RuleEditorForm.scrollGridSelectionsIntoView(kFromTop)
  else
  	RuleEditorForm.scrollGridSelectionsIntoView(kFromBottom);
  RuleEditorForm.updateRuleNumberLabel;
  end;

function TSMoveRulesCommand.description: string;
  begin
  if ruleWrappers.count > 1 then
    result := 'rules'
  else if ruleWrappers.count = 1 then
    result := 'rule'
  else
    result := 'rule';
  if action <> '' then
    result := action + ' ' + result
  else
    result := 'move ' + result;
  end;

{ TSMapDragCommand ------------------------------------------}

constructor TSMapDragCommand.create;
  begin
  dragRecords := TList.create;
  domain.world.addDragRecordsToList(dragRecords);
  end;

destructor TSMapDragCommand.destroy;
  var
  	i: integer;
  begin
  if dragRecords <> nil then
    for i := 0 to dragRecords.count - 1 do
      TObject(dragRecords[i]).free;
  dragRecords.free;
  dragRecords := nil;
  end;

procedure TSMapDragCommand.doCommand;
  var
  	i: integer;
  begin
  for i := 0 to dragRecords.count - 1 do
    TSDragRecord(dragRecords[i]).doDrag;
	if assigned(notifyProcedure) then
  	notifyProcedure(self, commandDone);
	inherited doCommand;
  end;

procedure TSMapDragCommand.undoCommand;
  var
  	i: integer;
  begin
  domain.world.deselectAllExcept(nil);
  for i := 0 to dragRecords.count - 1 do
    begin
    TSDragRecord(dragRecords[i]).draggedNode.selected := true;
    TSDragRecord(dragRecords[i]).undoDrag;
    end;
	if assigned(notifyProcedure) then
  	notifyProcedure(self, commandUndone);
  inherited undoCommand;
  end;

procedure TSMapDragCommand.redoCommand;
  var
  	i: integer;
  begin
  domain.world.deselectAllExcept(nil);
  for i := 0 to dragRecords.count - 1 do
    begin
    TSDragRecord(dragRecords[i]).draggedNode.selected := true;
    TSDragRecord(dragRecords[i]).doDrag;
    end;
	if assigned(notifyProcedure) then
  	notifyProcedure(self, commandDone);
	inherited doCommand;
  end;

function TSMapDragCommand.description: string;
  begin
  if dragRecords.count > 1 then
    result := 'Drag nodes'
  else if dragRecords.count = 1 then
    result := 'Drag ' + TSDragRecord(dragRecords[0]).draggedNode.displayName
  else
    result := 'Drag';
  end;

function TSMapDragCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
		mouseDidMove, rightButtonDown: boolean): KfCommand;
  var
  	i: integer;
    delta: TPoint;
  begin
  result := self;
  case aTrackPhase of
  	trackPress:
      begin
      if dragRecords.count = 0 then
        begin
        result := nil;
        self.free;
        exit;
        end;
      end;
    trackMove:
      begin
      if mouseDidMove then
        begin
      	delta :=
      		Point(nextPoint.x - previousPoint.x,
      					nextPoint.y - previousPoint.y);
  			for i := 0 to dragRecords.count - 1 do
    			TSDragRecord(dragRecords[i]).offset(delta);
				if assigned(notifyProcedure) then
  				notifyProcedure(self, commandDone);
        end;
      end;
  	trackRelease:
  		begin
     	if not mouseDidMove then
      	begin
   			if (TSDragRecord(dragRecords[0]).draggedNode.position.x <>
        			TSDragRecord(dragRecords[0]).originalLocation.x) or
        	 (TSDragRecord(dragRecords[0]).draggedNode.position.y <>
           		TSDragRecord(dragRecords[0]).originalLocation.y) then
          begin
  				for i := 0 to dragRecords.count - 1 do
    				TSDragRecord(dragRecords[i]).undoDrag;
					if assigned(notifyProcedure) then
  					notifyProcedure(self, commandDone);
          end;
      	result := nil;
      	self.free;
      	end
      else
        begin
       	delta :=
      		Point(nextPoint.x - previousPoint.x,
      					nextPoint.y - previousPoint.y);
   			if (delta.x <> 0) or (delta.y <> 0) then
          begin
  			  for i := 0 to dragRecords.count - 1 do
    			  TSDragRecord(dragRecords[i]).offset(delta);
					if assigned(notifyProcedure) then
  					notifyProcedure(self, commandDone);
          end;
        end;
  		end;
    end;
  end;

{ ----------------------------- TSCommandList -------------------------------}

function TSCommandList.toggleVariable(variable: TSVariable): TSToggleVariableCommand;
  begin
  result := TSToggleVariableCommand.createWithVariable(variable);
  self.doCommand(result);
  end;

function TSCommandList.moveFocus(newFocus: TSVariable): TSMoveFocusCommand;
  begin
  result := TSMoveFocusCommand.createWithNewFocus(newFocus);
  self.doCommand(result);
  end;

function TSCommandList.doCommandPhrase(const commandPhrase: string): TSDoCommandPhrase;
  begin
  result := TSDoCommandPhrase.createWithCommandPhrase(commandPhrase);
  self.doCommand(result);
  end;

function TSCommandList.ruleFieldChange(rule: TSRule; field: integer; const newValue: string): TSRuleFieldChange;
  var
    newContextOrMove: TSVariable;
  begin
  if (field = kRuleContext) or (field = kRuleMove) then
    if Pos('new context ', rule.getTextForField(field)) = 1 then
      begin
      if domain.world.findVariable(newValue) = nil then
        begin
   			newContextOrMove := domain.world.findOrCreateVariable(newValue, false);
    		newContextOrMove.position := rule.context.position;
        end;
      end;
  result := TSRuleFieldChange.createWithRule(rule, field, newValue);
  self.doCommand(result);
  end;

end.
