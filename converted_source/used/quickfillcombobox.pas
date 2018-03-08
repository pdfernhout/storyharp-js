unit QuickFillComboBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TQuickFillComboBox = class(TCustomComboBox)
  private
    { Private declarations }
    // pdf hack test remove
    procedure CBNEditChange(var MSG: Tmessage);  message CBN_EDITUPDATE;
  protected
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer); override;
		procedure Change; override;
  public
    { Public declarations }
  	lastMatch: string;
  	FMustBeInList: boolean;
  	FEntryRequired: boolean;
		function findMatch(const match: string): integer;
		procedure quickFillComboBoxKeyPress(var Key: Word);
  published
    property Align;
    property Style; {Must be published before Items}
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property Items;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;  // pdf added
    property OnMouseMove;  // pdf added
    property OnMouseUp;    // pdf added
    property OnStartDrag;
    property MustBeInList: boolean read FMustBeInList write FMustBeInList default false;
    property EntryRequired: boolean read FEntryRequired write FEntryRequired default false;
    end;

// PDF PORT changed from "register" as there was a name conflict with grammar; won't be called anyway
procedure RegisterIt;

implementation

// pdf hack - test remove
procedure TQuickFillComboBox.CBNEditChange(var MSG: Tmessage);
  var i: integer;
  begin
  inherited;
  i := 0;
  end;

function TQuickFillComboBox.findMatch(const match: string): integer;
var
	i, j: integer;
  test: string;
  matches: boolean;
begin
  result := -1;
  if match = '' then exit;
  if self.items.count = 0 then exit;
  for i := 0 to self.items.count - 1 do
    begin
    matches := true;
    test := self.items[i];
    for j := 1 to length(match) do
      if test[j] <> match[j] then
      	begin
        matches := false;
        break;
        end;
    if matches = true then
    	begin
      result := i;
      exit;
      end;
    end;
end;

procedure TQuickFillComboBox.quickFillComboBoxKeyPress(var Key: Word);
  var
  	index: integer;
    startText: string;
    atEnd: boolean;
	begin
    {reset if empty}
    if self.Text = '' then begin lastMatch := ''; end;
    {compensate for selection about to replace}
    atEnd := (self.selStart + self.SelLength) = Length(self.text);
    if not atEnd then
      if (self.text <> lastMatch) or (self.SelLength <> 0) then
      	begin
        if FMustBeInList then
          begin
          if (key = 8) and (selStart > 0) then selStart := selStart - 1;
        	key := 0; {eat key}
          end
        else
        	lastMatch := '';
      	if (self.text = '') and FEntryRequired and (self.items.count > 0) then
        	begin
         	key := 0; {eat key}
       	  lastMatch := self.items[0];
        	self.Text := lastMatch;
        	end;
        exit;
        end;
    if key = 8 then
      begin
      if self.SelLength = 0 then
        begin
      	startText := Copy(self.Text, 1, self.selStart-1);
        if selStart > 0 then selStart := selStart - 1;
        end
      else
        startText := Copy(self.Text, 1, self.selStart);
      end
    else if key < 32 then {don't process control keys - low asciii values}
    	begin
      lastMatch := '';
      if (self.text = '') and FEntryRequired and (self.items.count > 0) then
        begin
        lastMatch := self.items[0];
        self.Text := lastMatch;
        end;
      exit;
      end
    else
      startText := Copy(self.Text, 1, self.selStart) + char(key);
  	Key := 0;
    if startText = '' then
      begin
      if FEntryRequired and (self.items.count > 0) then
        begin
        lastMatch := self.items[0];
        self.Text := lastMatch;
        end
      else
        begin
      	self.Text := '';
      	lastMatch := '';
        end;
      exit;
      end;
    index := self.findMatch(startText);
 //   if (index < 0) and EntryRequired and (self.items.count > 0) then
 //     begin
 //     index := 0;
 //     startText := Copy(;
 //     end;
    if index >= 0 then
      begin
    	lastMatch := self.items[index];
    	self.Text := lastMatch;
    	self.SelStart := length(startText);
    	end
    else
      begin
      if not FMustBeInList then
        begin
   			lastMatch := '';
    		self.Text := startText;
   			self.SelStart := length(startText);
        end;
      end;
  	end;

// PDF PORT changed Message to TheMessage as used in with and gramamr did not like that
procedure TQuickFillComboBox.ComboWndProc(var TheMessage: TMessage; ComboWnd: HWnd; ComboProc: Pointer);
begin
  try
    with TheMessage do
    begin
      case Msg of
        WM_CHAR:
          begin
          if self.style <> csDropDownList	 then
						self.quickFillComboBoxKeyPress(TWMKey(TheMessage).charCode);
          end;
        CBN_EDITUPDATE:
          begin
          Dispatch(TheMessage);
          end;
        WM_LBUTTONUP:
          begin
          Dispatch(TheMessage);
          end;
      end;
      inherited ComboWndProc(TheMessage, ComboWnd, ComboProc);
    end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TQuickFillComboBox.Change;
	begin
  if FMustBeInList and (self.text <> '') then
    begin
    if (self.items.count <= 0) then begin self.text := ''; exit; end;
    if self.findMatch(self.text) < 0 then
      begin
      lastMatch := self.items[0];
      self.text := lastMatch;
      self.selStart := 0;
      self.selLength := 0;
      end;
    end;
  if (self.text = '') and FEntryRequired and (self.items.count > 0) then
    begin
    lastMatch := self.items[0];
    self.Text := lastMatch;
    end;
  inherited Change;
	end;

procedure RegisterIt;
begin
  RegisterComponents('Speech', [TQuickFillComboBox]);
end;

end.
