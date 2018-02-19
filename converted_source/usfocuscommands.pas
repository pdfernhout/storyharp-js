unit usfocuscommands;

interface

uses Windows, Graphics, Classes, Forms, Controls, StdCtrls, UCommand, USWorld, 
	USModelChanges;

type

  TSelectionInformation = record
 		text: string;
		selStart, selLength: integer;
    end;

  procedure RecordSelectionInformation(var selectionInformation: TSelectionInformation; control: TControl);
  procedure RestoreSelectionInformation(const selectionInformation: TSelectionInformation; control: TControl);

type

  TFocusShiftAction = class(TObject)
    control: TWinControl;
    form: TForm;
    modelChange: TModelChange;
    constructor createWithFormControlModelChange(form: TForm; control: TWinControl; modelChange: TModelChange); virtual;
    destructor destroy; override;
    procedure recordState(const selectionInformation: TSelectionInformation); virtual;  // param ignored
    procedure restoreStateWithFocus; virtual;
    procedure doShift; virtual;
    procedure undoShift; virtual;
    procedure redoShift; virtual;
    end;

	TFocusExitAction = class(TFocusShiftAction)
    procedure doShift; override;
    procedure undoShift; override;
    procedure redoShift; override;
    end;

	TFocusEnterAction = class(TFocusShiftAction)
    procedure doShift; override;
    procedure undoShift; override;
    procedure redoShift; override;
    end;

  TTextFocusExitAction = class(TFocusExitAction)
 		selectionInformation: TSelectionInformation;
    procedure recordState(const selectionInformation: TSelectionInformation); override;
    procedure restoreStateWithFocus; override;
    end;

  TTextFocusEnterAction = class(TFocusEnterAction)
    end;

	TFocusShiftCommand = class(KfCommand)
    focusExitAction: TFocusExitAction;
    focusEnterAction: TFocusEnterAction;
    constructor createWithFocusExitAction(focusExitAction: TFocusExitAction);
    procedure setFocusEnterAction(focusEnterAction: TFocusEnterAction);
    destructor destroy; override;
 		procedure doCommand; override;
		procedure undoCommand; override;
		procedure redoCommand; override;
 		function description: string; override;
    end;

implementation

uses SysUtils, QuickFillComboBox, USDomain;

procedure RecordSelectionInformation(var selectionInformation: TSelectionInformation; control: TControl);
	begin
  if control is TCustomEdit then
    begin
    selectionInformation.text := (control as TCustomEdit).text;
  	selectionInformation.selStart := (control as TCustomEdit).selStart;
  	selectionInformation.selLength := (control as TCustomEdit).selLength;
    end
  else if control is TQuickFillComboBox then
    begin
    selectionInformation.text := (control as TQuickFillComboBox).text;
  	selectionInformation.selStart := (control as TQuickFillComboBox).selStart;
  	selectionInformation.selLength := (control as TQuickFillComboBox).selLength;
    end
  else
    begin
    // error
    raise Exception.create('RecordSelectionInformation: unsupported control class');
    end;
  end;

procedure RestoreSelectionInformation(const selectionInformation: TSelectionInformation; control: TControl);
	begin
  if control is TCustomEdit then
    begin
    (control as TCustomEdit).text := selectionInformation.text;
  	(control as TCustomEdit).selStart := selectionInformation.selStart;
  	(control as TCustomEdit).selLength := selectionInformation.selLength;
    end
  else if control is TQuickFillComboBox then
    begin
    (control as TQuickFillComboBox).text := selectionInformation.text;
  	(control as TQuickFillComboBox).selStart := selectionInformation.selStart;
  	(control as TQuickFillComboBox).selLength := selectionInformation.selLength;
    end
  else
    begin
    // error
    raise Exception.create('RestoreSelectionInformation: unsupported control class');
    end;
  end;

///////////////////////////// TFocusShiftAction //////////////////////////

constructor TFocusShiftAction.createWithFormControlModelChange(form: TForm; control: TWinControl; modelChange: TModelChange);
	begin
  self.form := form;
  self.control := control;
  self.modelChange := modelChange;
  end;

destructor TFocusShiftAction.destroy;
  begin
  modelChange.free;
  inherited destroy;
  end;

procedure TFocusShiftAction.recordState(const selectionInformation: TSelectionInformation); // param ignored
	begin
  // subclasses should override
  end;

procedure TFocusShiftAction.restoreStateWithFocus;
	begin
  // form.Active := true;
  control.setFocus;
  // subclasses may override
  end;

// assume focus shifted is handled by system the first time
// assume do must be called before other component is focused to if it is off the form
procedure TFocusShiftAction.doShift;
	begin
  // subclasses may override
  if modelChange <> nil then
  	modelChange.doChange;
  end;

procedure TFocusShiftAction.undoShift;
	begin
  // subclasses may override
  if modelChange <> nil then
    modelChange.undoChange;
  end;

procedure TFocusShiftAction.redoShift;
	begin
 	if modelChange <> nil then
    modelChange.redoChange;
  end;

//////////////////////////// TFocusExitAction ////////////////////////////////

procedure TFocusExitAction.doShift;
	begin
  inherited doShift;
  end;

// exit moves focus on undo
procedure TFocusExitAction.undoShift;
	begin
  self.restoreStateWithFocus;
  // need to restore selectionInformation in form
 	inherited undoShift;
  end;

procedure TFocusExitAction.redoShift;
	begin
 	inherited redoShift;
  end;

//////////////////////////// TFocusEnterAction ////////////////////////////////

procedure TFocusEnterAction.doShift;
	begin
  inherited doShift;
  end;

procedure TFocusEnterAction.undoShift;
	begin
 	inherited undoShift;
  end;

// enter moves focus on redo
procedure TFocusEnterAction.redoShift;
	begin
  self.restoreStateWithFocus;
 	inherited redoShift;
  end;

//////////////////////////// TTextFocusExitAction ////////////////////////////////

procedure TTextFocusExitAction.recordState(const selectionInformation: TSelectionInformation);
  begin
  inherited recordState(selectionInformation);
  self.selectionInformation := selectionInformation;
  //RecordSelectionInformation(selectionInformation, control);
  end;

procedure TTextFocusExitAction.restoreStateWithFocus;
  begin
  inherited restoreStateWithFocus;
  RestoreSelectionInformation(selectionInformation, control);
  end;

//////////////////////////// TTextFocusEnterAction ////////////////////////////////

//////////////////////////// TFocusShiftCommand ////////////////////////////////

constructor TFocusShiftCommand.createWithFocusExitAction(focusExitAction: TFocusExitAction);
  begin
  self.create;
  self.focusExitAction := focusExitAction;
  //self.focusExitAction.recordState;
  // responsibility of component being focused to call setFocusEnterAction
  end;

procedure TFocusShiftCommand.setFocusEnterAction(focusEnterAction: TFocusEnterAction);
  var selectionInformation: TSelectionInformation;
  begin
  self.focusEnterAction := focusEnterAction;
  self.FocusEnterAction.recordState(selectionInformation); // param ignored
  end;

destructor TFocusShiftCommand.destroy;
  begin
  focusExitAction.free;
  focusEnterAction.free;
  inherited destroy;
  end;

procedure TFocusShiftCommand.doCommand;
  begin
  inherited doCommand;
  domain.beginUpdate;
  focusExitAction.doShift;
  if focusEnterAction <> nil then
  	focusEnterAction.doShift;
  domain.endUpdate;
  end;

procedure TFocusShiftCommand.undoCommand;
  begin
  inherited undoCommand;
  domain.beginUpdate;
  if focusEnterAction <> nil then
  	focusEnterAction.undoShift;
  focusExitAction.undoShift;
  domain.endUpdate;
  end;

procedure TFocusShiftCommand.redoCommand;
  begin
  inherited doCommand;
  domain.beginUpdate;
  focusExitAction.redoShift;
  if focusEnterAction <> nil then
  	focusEnterAction.redoShift;
  domain.endUpdate;
  end;

function TFocusShiftCommand.description: string;
	begin
  result := 'editor focus shift';
  end;

end.
