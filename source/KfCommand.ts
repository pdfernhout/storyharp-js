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

end.
