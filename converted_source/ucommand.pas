unit Ucommand;

interface

uses Classes, WinTypes;

type
TrackPhase = (trackPress, trackMove, trackRelease);

KfCommand = class;

KfCommandChangeType = (commandDone, commandUndone);

TCommandEvent = procedure(command: KfCommand; state: KfCommandChangeType) of object;

KfCommand = class(TObject)
  public
  notifyProcedure: TCommandEvent;
  canUndo: boolean;
  done: boolean;
  commandChangesFile: boolean;
  constructor create;
  destructor destroy; override;
  procedure doCommand; virtual;
  procedure undoCommand; virtual;
  procedure redoCommand; virtual;
  function description: string; virtual;
  function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove, rightButtonDown: boolean): KfCommand; virtual;
  procedure doNotify;
  end;

KfCommandList = class(TObject)
	public
  notifyProcedure: TCommandEvent;
	commandList: TList;
	lastDoneCommandIndex: longint;
  undoLimit: longint;
  mouseCommand: KfCommand;
  anchorPoint: TPoint;
  previousPoint: TPoint;
  rightButtonDown: boolean;
  constructor create;
  destructor destroy; override;
	procedure clear;
  function command(index: longint): KfCommand;
  procedure setNewUndoLimit(newLimit: longint);
	procedure freeCommandsAboveLimit(theLimit: longint);
  procedure doCommand(newCommand: KfCommand);
  function mouseDown(newCommand: KfCommand; point: TPoint): boolean;
  procedure mouseMove(point: TPoint);
  procedure mouseUp(point: TPoint);
  function isUndoEnabled: boolean;
  function isRedoEnabled: boolean;
  function undoDescription: string;
  function redoDescription: string;
  procedure undoLast;
  procedure removeCommand(aCommand: KfCommand);
  procedure redoLast;
  function lastCommand: KfCommand;
	procedure clearRedoableCommands;
  procedure lastCommandChanged; {call if change last command}
  end;

implementation

uses SysUtils;

{KfCommand}
constructor KfCommand.create;
	begin
  inherited create;
  canUndo := true;
  done := false;
  { default commandChangesPlantFile to true, since most commands change file,
    if command does not change file, set to false after call to inherited create }
  commandChangesFile := true;
  end;

destructor KfCommand.destroy;
	begin
  {sublass could override}
  inherited destroy;
  end;

procedure KfCommand.doCommand;
	begin
  self.done := true;
  {subclass should override and call inherited}
  end;

procedure KfCommand.undoCommand;
	begin
  self.done := false;
  {sublass should override and call inherited}
  end;

procedure KfCommand.redoCommand;
  begin
  self.doCommand;
  {sublass may override and call inherited doCommand}
  {could call inherited redo, but then watch out that do will be done too!}
  end;

function KfCommand.description: string;
  begin
  result := '*command description*';
  end;

function KfCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove, rightButtonDown: boolean): KfCommand;
  begin
  {sublasses should override if needed}
  result := self;
  end;

// notify cannot be done within the do, undo, redo of command because
// command list will not have finished updating itself

procedure KfCommand.doNotify;
  begin
  // commandChangesFile and
  if Assigned(notifyProcedure) then
    begin
    if self.done then
    	self.notifyProcedure(self, commandDone)
    else
      self.notifyProcedure(self, commandUndone);
    end;
  end;

{KfCommandList}
constructor KfCommandList.create;
  begin
  inherited create;
  commandList := TList.create;
	lastDoneCommandIndex := -1;
  undoLimit := 100;
  end;

procedure KfCommandList.clear;
  var i: integer;
  begin
  if commandList.count > 0 then
    for i := 0 to commandList.count - 1 do
      TObject(commandList[i]).free;
  commandList.clear;
	lastDoneCommandIndex := -1;
  end;

destructor KfCommandList.destroy;
	begin
  self.clear;
  commandList.free;
  {if mouseCommand <> nil then error condition - ignoring for now - not released}
  {could only happend if quitting somehow in middle of action}
  inherited destroy;
  end;

function KfCommandList.command(index: longint): KfCommand;
	begin
  result := KfCommand(commandList.items[index]);
  end;

procedure KfCommandList.setNewUndoLimit(newLimit: longint);
  begin
  undoLimit := newLimit;
  self.freeCommandsAboveLimit(undoLimit);
  end;

{free any command more than the number passed in}
procedure KfCommandList.freeCommandsAboveLimit(theLimit: longint);
  var
    theCommand: KfCommand;
  begin
  while (commandList.count > theLimit) and (commandList.count > 0) do
  	begin
    theCommand := command(0);
    commandList.delete(0);
    theCommand.free;
    dec(lastDoneCommandIndex);
    if lastDoneCommandIndex < -1 then lastDoneCommandIndex := -1;
    end;
  end;

procedure KfCommandList.doCommand(newCommand: KfCommand);
	begin
  {remove any extra commands after the current}
  {do this first to free memory for command}
  self.clearRedoableCommands;
  {see if too many commands are stored and if so, scroll them}
  self.freeCommandsAboveLimit(undoLimit - 1);
  {now do this command}
  if not Assigned(newCommand.notifyProcedure) then
      newCommand.notifyProcedure := notifyProcedure;
  newCommand.doCommand; {may fail in which case won't add}
  inc(lastDoneCommandIndex);
  commandList.add(newCommand);
  newCommand.doNotify;
  end;

{added nextMouseCommand in these three functions to deal with unhandled exceptions occurring
during mouse commands.  This way, the command will not be further processed.
This may occasionally leak - the mouse command should be the one responsible for freeing
itself and returning nil if a problem occurs}
{returns whether the command finished tracking without freeing itself}
function KfCommandList.mouseDown(newCommand: KfCommand; point: Tpoint): boolean;
  var
    nextMouseCommand: KfCommand;
	begin
  result := false;
  {check if need to clear mouse command}
  if mouseCommand <> nil then
    self.mouseUp(point);
  mouseCommand := nil;
  {save mouse command and start it}
  if newCommand <> nil then
    begin
    anchorPoint := point;
    previousPoint := point;
  	nextMouseCommand := newCommand;
  	if not Assigned(newCommand.notifyProcedure) then
      newCommand.notifyProcedure := notifyProcedure;
    mouseCommand := nextMouseCommand.trackMouse(TrackPress, anchorPoint, previousPoint, point, false, rightButtonDown);
    result := (mouseCommand <> nil);
    end;
  end;

const kMinMouseMoveDistance = 2;

procedure KfCommandList.mouseMove(point: TPoint);
  var
  	mouseDidMove: boolean;
    nextMouseCommand: KfCommand;
    pointMovedTo: TPoint;
	begin
  nextMouseCommand := mouseCommand;
  mouseCommand := nil;
  if nextMouseCommand <> nil then
    begin
    mouseDidMove := (abs(previousPoint.x - point.x) > kMinMouseMoveDistance)
        or (abs(previousPoint.y - point.y) > kMinMouseMoveDistance);
    if mouseDidMove then
      pointMovedTo := point
    else
      pointMovedTo := previousPoint;
  	mouseCommand := nextMouseCommand.trackMouse(
        trackMove, anchorPoint, previousPoint, pointMovedTo, mouseDidMove, rightButtonDown);
    end;
  previousPoint := pointMovedTo;
  end;

procedure KfCommandList.mouseUp(point: TPoint);
  var
  	mouseDidMove: boolean;
    nextMouseCommand: KfCommand;
    pointMovedTo: TPoint;
	begin
  nextMouseCommand := mouseCommand;
  mouseCommand := nil;
  if nextMouseCommand <> nil then
    begin
    mouseDidMove := (abs(point.x - anchorPoint.x) > kMinMouseMoveDistance)
        or (abs(point.y - anchorPoint.y) > kMinMouseMoveDistance);
    if mouseDidMove then
      pointMovedTo := point
    else
      pointMovedTo := anchorPoint;
  	nextMouseCommand := nextMouseCommand.trackMouse(
        trackRelease, anchorPoint, previousPoint, point, mouseDidMove, rightButtonDown);
  	if nextMouseCommand <> nil then
    	doCommand(nextMouseCommand);
    end;
  end;

function KfCommandList.isUndoEnabled: boolean;
	begin
  result := lastDoneCommandIndex >= 0;
  end;

function KfCommandList.isRedoEnabled: boolean;
	begin
  result := lastDoneCommandIndex < (commandList.count - 1);
  end;

function KfCommandList.undoDescription: string;
	begin
  if lastDoneCommandIndex >= 0 then
  	result := command(lastDoneCommandIndex).description
  else
    result := '';
  end;

function KfCommandList.redoDescription: string;
	begin
  if lastDoneCommandIndex < (commandList.count - 1) then
  	result := command(lastDoneCommandIndex+1).description
  else
    result := '';
  end;

procedure KfCommandList.undoLast;
  var aCommand: KfCommand;
	begin
  if lastDoneCommandIndex >= 0 then
  	begin
 	  aCommand := command(lastDoneCommandIndex);
    aCommand.undoCommand;
    dec(lastDoneCommandIndex);
    aCommand.doNotify;
    end;
  end;

procedure KfCommandList.redoLast;
  var aCommand: KfCommand;
	begin
  if lastDoneCommandIndex < (commandList.count - 1) then
  	begin
  	aCommand := command(lastDoneCommandIndex+1);
    aCommand.redoCommand;
    inc(lastDoneCommandIndex);
    aCommand.doNotify;
    end;
  end;

procedure KfCommandList.removeCommand(aCommand: KfCommand);
	begin
  { assume this command has been undone previously }
  if aCommand.done then
    raise Exception.create('KfCommandList.removeCommand: command not undone');
  commandList.remove(aCommand);
  end;

function KfCommandList.lastCommand: KfCommand;
	begin
  result := nil;
  if lastDoneCommandIndex >= 0 then
  	begin
  	result := command(lastDoneCommandIndex);
    end;
  end;

procedure KfCommandList.clearRedoableCommands;
	var
  	i: longint;
    theCommand: KfCommand;
  begin
  if isRedoEnabled then
  	for i := commandList.count - 1 downto lastDoneCommandIndex + 1 do
    	begin
      theCommand := command(i);
      commandList.delete(i);
      theCommand.free;
      end;
  end;

procedure KfCommandList.lastCommandChanged;
  begin
  self.clearRedoableCommands;
  end;

end.
