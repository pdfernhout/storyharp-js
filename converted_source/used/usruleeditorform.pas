unit usruleeditorform;

interface

{INDEX (search for at symbol and name)
Local
Creation/destruction
File menu
Edit menu
Rule menu
Display menu
Tools menu
Help menu
Button bar
Updating
Events
Commands
Table
Map
Browser
Resizing}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, QuickFillComboBox, ExtCtrls,
  USDomain, USWorld, USCommands, USFocusCommands, Menus, Grids, ComCtrls,
  UCommand, USMapView, MPlayer;

type
  TRuleEditorForm = class(TForm)
    PanelEditor: TPanel;
    WaveFileOpenDialog: TOpenDialog;
    ImageList: TImageList;
    SplitterEdit: TSplitter;
    PanelRequirementsChanges: TPanel;
    RequirementsSpeedButton: TSpeedButton;
    ChangesSpeedButton: TSpeedButton;
    RequirementsListBox: TListBox;
    ChangesListBox: TListBox;
    RequirementsEdit: TEdit;
    ChangesEdit: TEdit;
    PanelRest: TPanel;
    Label5: TLabel;
    CommandSpeedButton: TSpeedButton;
    MoveSpeedButton: TSpeedButton;
    ContextSpeedButton: TSpeedButton;
    ContextEdit: TEdit;
    CommandEdit: TEdit;
    ReplyMemo: TMemo;
    MoveEdit: TEdit;
    SplitterRequirementsChanges: TSplitter;
    MainMenu1: TMainMenu;
    MenuEdit: TMenuItem;
    MenuEditCut: TMenuItem;
    MenuEditCopy: TMenuItem;
    MenuEditPaste: TMenuItem;
    MenuEditUndo: TMenuItem;
    MenuEditRedo: TMenuItem;
    N1: TMenuItem;
    MenuRule: TMenuItem;
    MenuRuleNew: TMenuItem;
    MenuRuleDelete: TMenuItem;
    MenuRuleDuplicate: TMenuItem;
    MenuBrowseByContext: TMenuItem;
    MenuBrowseByCommand: TMenuItem;
    MenuBrowseByMove: TMenuItem;
    MenuBrowseByRequirements: TMenuItem;
    MenuBrowseByChanges: TMenuItem;
    MenuFile: TMenuItem;
    MenuFileNewWorld: TMenuItem;
    MenuFileSaveWorld: TMenuItem;
    MenuFileSaveWorldAs: TMenuItem;
    MenuFileMergeWithWorld: TMenuItem;
    N3: TMenuItem;
    MenuRuleRaise: TMenuItem;
    MenuRuleLower: TMenuItem;
    MenuMaps: TMenuItem;
    MenuHelp: TMenuItem;
    MenuMapsQuickContexts: TMenuItem;
    MenuFileExit: TMenuItem;
    MenuFileOpenWorld: TMenuItem;
    MenuHelpContents: TMenuItem;
    MenuHelpEditingWorlds: TMenuItem;
    N6: TMenuItem;
    MenuHelpAbout: TMenuItem;
    FontDialog: TFontDialog;
    MenuMapsShowCommands: TMenuItem;
    MenuHelpRegister: TMenuItem;
    EditPopupMenu: TPopupMenu;
    PopupCut: TMenuItem;
    PopupCopy: TMenuItem;
    PopupPaste: TMenuItem;
    MenuMapQuickCommands: TMenuItem;
    MenuMapLinkWizard: TMenuItem;
    MenuEditLogFile: TMenuItem;
    Wizards1: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    MenuHelpTutorial: TMenuItem;
    AfterRegisterMenuSeparator: TMenuItem;
    N10: TMenuItem;
    MenuFileExport: TMenuItem;
    N11: TMenuItem;
    PanelTop: TPanel;
    ListPages: TPageControl;
    TabSheetTable: TTabSheet;
    RuleGrid: TDrawGrid;
    TabSheetBrowse: TTabSheet;
    PanelLists: TPanel;
    SplitterLists: TSplitter;
    PanelFirstList: TPanel;
    firstListBoxImage: TImage;
    firstListBoxLabel: TLabel;
    FirstListBox: TListBox;
    PanelSecondList: TPanel;
    SecondListBoxImage: TImage;
    SecondListBoxLabel: TLabel;
    SecondListBox: TListBox;
    TabSheetMap: TTabSheet;
    PanelMap: TPanel;
    MapImage: TImage;
    MapScrollBarHorizontal: TScrollBar;
    MapScrollBarVertical: TScrollBar;
    PanelButtonBar: TPanel;
    NewRuleButton: TSpeedButton;
    DuplicateRuleButton: TSpeedButton;
    DeleteRuleButton: TSpeedButton;
    MoveUpButton: TSpeedButton;
    MoveDownButton: TSpeedButton;
    RuleNumberLabel: TLabel;
    MenuDisplayShowButtonBar: TMenuItem;
    MenuToolsSearch: TMenuItem;
    N2: TMenuItem;
    FindDialog: TFindDialog;
    N12: TMenuItem;
    MenuEditInsertSound: TMenuItem;
    insertSound: TSpeedButton;
    MediaPlayer: TMediaPlayer;
    MenuOptionsShowRuleEditor: TMenuItem;
    InsertMusicButton: TSpeedButton;
    MenuEditInsertMusic: TMenuItem;
    N13: TMenuItem;
    MenuEditPreferences: TMenuItem;
    replyPicture: TImage;
    N5: TMenuItem;
    MenuRuleTestReply: TMenuItem;
    MenuHelpBasicConcepts: TMenuItem;
    N4: TMenuItem;
    MenuWorldSwitchToPlayer: TMenuItem;
    MapPopupMenu: TPopupMenu;
    PopupNewContext: TMenuItem;
    PopupNewLink: TMenuItem;
    PopupNewCommand: TMenuItem;
    MenuToolsGenerateJava: TMenuItem;
    N14: TMenuItem;
    MenuEditInsertPicture: TMenuItem;
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure SpeedButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PanelFirstListResize(Sender: TObject);
    procedure PanelSecondListResize(Sender: TObject);
    procedure PanelRestResize(Sender: TObject);
    procedure PanelRequirementsChangesResize(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PanelListsResize(Sender: TObject);
    procedure SplitterRequirementsChangesMoved(Sender: TObject);
    procedure SplitterEditMoved(Sender: TObject);
    procedure EditDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure EditDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListBoxMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ListBoxDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure ListBoxDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TabSheetMapDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TabSheetMapDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListPagesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure RuleGridDrawCell(Sender: TObject; Col, Row: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure RuleGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RuleGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListPagesChange(Sender: TObject);
    procedure MenuFileNewWorldClick(Sender: TObject);
    procedure MenuFileOpenWorldClick(Sender: TObject);
    procedure MenuFileSaveWorldClick(Sender: TObject);
    procedure MenuFileSaveWorldAsClick(Sender: TObject);
    procedure MenuFileMergeWithWorldClick(Sender: TObject);
    procedure MenuFileExitClick(Sender: TObject);
    procedure MenuEditUndoClick(Sender: TObject);
    procedure MenuEditRedoClick(Sender: TObject);
    procedure MenuEditCutClick(Sender: TObject);
    procedure MenuEditCopyClick(Sender: TObject);
    procedure MenuEditPasteClick(Sender: TObject);
    procedure MenuMapsQuickContextsClick(Sender: TObject);
    procedure MenuMapsShowCommandsClick(Sender: TObject);
    procedure MapListChange(Sender: TObject);
    procedure MapImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MapImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MapImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MapImageDblClick(Sender: TObject);
    procedure PanelMapResize(Sender: TObject);
    procedure MapScrollBarVerticalChange(Sender: TObject);
    procedure MapScrollBarHorizontalChange(Sender: TObject);
    procedure EditEnterCommit(Sender: TObject);
    procedure MenuRuleNewClick(Sender: TObject);
    procedure MenuRuleRaiseClick(Sender: TObject);
    procedure MenuRuleLowerClick(Sender: TObject);
    procedure MenuRuleDeleteClick(Sender: TObject);
    procedure MenuRuleDuplicateClick(Sender: TObject);
    procedure ListBoxKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListBoxDblClick(Sender: TObject);
    procedure ListBoxEditExit(Sender: TObject);
    procedure ListBoxEditKeyPress(Sender: TObject; var Key: Char);
    procedure ListBoxExit(Sender: TObject);
    procedure MenuMapFontClick(Sender: TObject);
    procedure MenuMapQuickCommandsClick(Sender: TObject);
    procedure MenuMapLinkWizardClick(Sender: TObject);
    procedure MenuEditLogFileClick(Sender: TObject);
    procedure MenuHelpAboutClick(Sender: TObject);
    procedure NewRuleButtonClick(Sender: TObject);
    procedure DuplicateRuleButtonClick(Sender: TObject);
    procedure DeleteRuleButtonClick(Sender: TObject);
    procedure MoveUpButtonClick(Sender: TObject);
    procedure MoveDownButtonClick(Sender: TObject);
    procedure MenuDisplayShowButtonBarClick(Sender: TObject);
    procedure FirstListBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure SecondListBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure MenuFileExportClick(Sender: TObject);
    procedure FirstListBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SecondListBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuToolsSearchClick(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure MenuEditInsertSoundClick(Sender: TObject);
    procedure insertSoundClick(Sender: TObject);
    procedure MenuOptionsShowRuleEditorClick(Sender: TObject);
    procedure InsertMusicButtonClick(Sender: TObject);
    procedure MenuEditInsertMusicClick(Sender: TObject);
    procedure MediaPlayerNotify(Sender: TObject);
    procedure MenuHelpRegisterClick(Sender: TObject);
    procedure MenuEditPreferencesClick(Sender: TObject);
    procedure firstListBoxImageClick(Sender: TObject);
    procedure MenuRuleTestReplyClick(Sender: TObject);
    procedure replyPictureMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuEditClick(Sender: TObject);
    procedure ReplyMemoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MenuHelpContentsClick(Sender: TObject);
    procedure MenuHelpEditingWorldsClick(Sender: TObject);
    procedure MenuHelpTutorialClick(Sender: TObject);
    procedure MenuHelpBasicConceptsClick(Sender: TObject);
    procedure MenuWorldSwitchToPlayerClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PopupNewContextClick(Sender: TObject);
    procedure PopupNewCommandClick(Sender: TObject);
    procedure PopupNewLinkClick(Sender: TObject);
    procedure MenuToolsGenerateJavaClick(Sender: TObject);
    procedure MenuEditInsertPictureClick(Sender: TObject);
  private
    procedure WMGetMinMaxInfo(var MSG: Tmessage);  message WM_GetMinMaxInfo;
    procedure WMEraseBkgnd(var m: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
		function GetPalette: HPALETTE; override;
		function PaletteChanged(Foreground: Boolean): Boolean; override;
  public
  	rule: TSRule;
		selectionInformation: TSelectionInformation;
    organizeByField: integer;
    wasLoaded: boolean;
    lastSaveProceeded: boolean;
    lastClickAtLeft: boolean;
    ignoreNextEnter: boolean; {kludge for popup edits...}
    indexEdited: integer;
    lastCommand: TSRule;
    lastSingleRuleIndex, lastBrowserSingleRuleIndex: integer;
    loopMusic: boolean;
    buttonSymbols: boolean;
    startingUp: boolean;
		procedure openWorldFile(const fileNameWithPath: string);
		procedure commitChangesToRule;
		procedure updateForFieldChange(fieldType: integer);
		procedure editRule(rule: TSRule);
		procedure loadAllRuleFields;
		procedure updateRuleNumberLabel;
		procedure fillListBox(listBox: TListBox; list: TList);
		function logicalStatementForListBox(listBox: TListBox): string;
		procedure loadFirstListBox;
		procedure loadSecondListBox;
		procedure drawBrowserListBoxItem(Control: TWinControl;
  		displayString: string; Index: Integer; Rect: TRect; selected, focused: boolean);
		procedure setOrganizeByField(newValue: integer);
		function askForSaveWorldAndProceed: boolean;
		procedure updateMenusForUndoRedo;
		procedure commandChangedNotification(command: KfCommand; state: KfCommandChangeType);
		procedure updateMenus;
		procedure updateViews;
		procedure listBoxNewStatement(listBox: TListBox; const newStatement: string);
		procedure positionEditForListBox(edit: TEdit; listBox: TListBox);
		procedure scrollGridSelectionsIntoView(direction: boolean);
 		procedure updateForRuleChange;
    procedure trackLastCommand;
		procedure setEnabledForControl(control: TWinControl; enable: boolean);
		procedure searchForAndSelectRule(aText: string; ignoreCase: boolean; goDown: boolean);
		procedure updateForChangeToDomainOptions;
		procedure setButtonGlyphs;
    procedure selectEditorField(field: integer);
		function switchToPage(newPage: TTabSheet): boolean;
 public {graphs}
    actionInProgress: boolean;
    previousChoice, lastChoice: TSDraggableObject;
    mapSelectionInProgress: boolean;
    mapSelectionRect: TRect;
    lastMapMouseDownPosition: TPoint;
    numNewContextsMadeByPopupMenuThisSession: integer;
    numNewCommandsMadeByPopupMenuThisSession: integer;
    function currentGraphView: TSMapView;
    function makeChoice(choice: TSDraggableObject; multiSelect: boolean): boolean;
    function lastChoiceText: string;
		procedure adjustScrollBars;
    procedure MapPaintBoxChanged;
		procedure mapChangedNotification(command: KfCommand; state: KfCommandChangeType);
    function goodPosition: TPoint;
		procedure XorRect(canvas: TCanvas; rect: TRect);
		procedure scrollMapSelectionIntoView;
   end;

var
  RuleEditorForm: TRuleEditorForm;

const
  kFromBottom = true;
  kFromTop = false;

	kPlaySoundMacroStart = 'sound ';
	kPlayMusicMacroStart = 'music ';
  kShowPictureMacroStart = 'picture ';


procedure setCanvasColorsForSelection(canvas: TCanvas; selected, focused, isCommandInMap: boolean);

implementation
                  
uses Clipbrd, ShellAPI, USModelChanges, USConsoleForm, UFileSupport, USContextWizard,
  USCommandWizard, USLinkWizard, USChangeLog, USAbout, uregister,
  USPreferences, USJavaWriter, USPictureForm;

{$R *.DFM}

const
	kMinListHeight = 115;
  kMinRestHeight = 24 * 4 + 4 * 5;
  kMinRequirementsChangesHeight = 60;
  kSplitterHeight = 3;

// ----------------------------------------------------------- @Local functions
function localIntMin(a, b: integer): integer;
  begin
  result := a;
  if b < a then
  	result := b;
  end;

function localIntMax(a, b: integer): integer;
  begin
  result := a;
  if b > a then
  	result := b;
  end;

// ------------------------------------------------------ @Creation/destruction
procedure TRuleEditorForm.FormCreate(Sender: TObject);
	begin
  numNewContextsMadeByPopupMenuThisSession := 1;
  numNewCommandsMadeByPopupMenuThisSession := 1;
  domain.setFormSize(self, domain.options.editorWindowRect);
  if domain.options.editorPanelEditorHeight > 0 then
    begin
    PanelEditor.height := domain.options.editorPanelEditorHeight;
    PanelEditor.top := self.clientHeight - PanelEditor.height;
    end;
  if domain.options.editorPanelRequirementsChangesHeight > 0 then
    begin
    PanelRequirementsChanges.height := domain.options.editorPanelRequirementsChangesHeight;
    PanelRequirementsChanges.top := PanelEditor.clientHeight - PanelRequirementsChanges.height;
    end;
  case domain.options.pageShowing of
    kPageTable: ListPages.ActivePage := TabSheetTable;
    kPageMap:   ListPages.ActivePage := TabSheetMap;
    kPageBrowser: ListPages.ActivePage := TabSheetBrowse;
    end;
  if domain.options.editorPanelFirstListWidth > 0 then
    PanelFirstList.width := domain.options.editorPanelFirstListWidth;
  domain.worldCommandList.notifyProcedure := self.commandChangedNotification;
  startingUp := true;
	self.updateForChangeToDomainOptions;
  startingUp := false;
	end;

function TRuleEditorForm.switchToPage(newPage: TTabSheet): boolean;
  begin
  result := false;
  if ListPages.ActivePage = newPage then exit;
  ListPages.ActivePage := newPage;
  self.ListPagesChange(self);
  result:= true;
  end;

procedure TRuleEditorForm.FormActivate(Sender: TObject);
	begin
  if application.terminated then exit;
	//  self.updateForRuleChange;
	//	self.loadFirstListBox;
	//	self.loadSecondListBox;
	if firstListBox.items.count = 0 then
    begin
  	self.setOrganizeByField(organizeByField);
    //if secondListBox.items.count > 0 then
    end;
  wasLoaded := true;
  if domain.world.rules.count + 1 >= 2 then
  	RuleGrid.rowCount := domain.world.rules.count + 1;
	end;

procedure TRuleEditorForm.commitChangesToRule;
  begin
  RequirementsEdit.hide;
  ChangesEdit.hide;
  if rule = nil then exit;
  if ContextEdit.Text <> rule.context.phrase then
    domain.worldCommandList.ruleFieldChange(rule, kRuleContext, ContextEdit.Text);
  if CommandEdit.Text <> rule.command.phrase then
    domain.worldCommandList.ruleFieldChange(rule, kRuleCommand, CommandEdit.Text);
  if ReplyMemo.Text <> rule.reply then
    domain.worldCommandList.ruleFieldChange(rule, kRuleReply, ReplyMemo.Text);
  if MoveEdit.Text <> rule.move.phrase then
    domain.worldCommandList.ruleFieldChange(rule, kRuleMove, MoveEdit.Text);
  if logicalStatementForListBox(requirementsListBox) <> rule.decompileRequirements then
    domain.worldCommandList.ruleFieldChange(rule, kRuleRequirements, logicalStatementForListBox(requirementsListBox));
  if logicalStatementForListBox(changesListBox) <> rule.decompileChanges then
    domain.worldCommandList.ruleFieldChange(rule, kRuleChanges, logicalStatementForListBox(changesListBox));
  end;

procedure TRuleEditorForm.setButtonGlyphs;
  begin
  if ConsoleForm = nil then exit;
  ContextSpeedButton.glyph := ConsoleForm.ContextButton.glyph;
  CommandSpeedButton.glyph := ConsoleForm.CommandButton.glyph;
  MoveSpeedButton.glyph := ConsoleForm.MoveButton.glyph;
  RequirementsSpeedButton.glyph := ConsoleForm.RequirementsButton.glyph;
  ChangesSpeedButton.glyph := ConsoleForm.ChangesButton.glyph;
  end;

procedure TRuleEditorForm.WMEraseBkgnd(var m : TWMEraseBkgnd);
	begin
	// since we're going to be painting the whole form, handling this
	// message will suppress the uneccessary repainting of the background
	// which can result in flicker.
  m.Result := LRESULT(False);
	end;

procedure TRuleEditorForm.FormDeactivate(Sender: TObject);
	begin
 	//	ShowMessage('deactivate');
 	// unfinished self.makeChangeCommandIfNeeded;
  self.commitChangesToRule;
	end;

// ----------------------------------------------------------------- @File menu
procedure TRuleEditorForm.MenuFileNewWorldClick(Sender: TObject);
	begin
  self.commitChangesToRule;
	if not self.askForSaveWorldAndProceed then exit;
	if not ConsoleForm.askForSaveSessionAndProceed then exit;
  rule := nil;
  domain.newWorld;
  ConsoleForm.locationCacheValid := false;
  ConsoleForm.clearTranscript;
	self.updateForRuleChange;
  self.updateViews;
  self.editRule(nil);
  previousChoice := nil;
  lastChoice := nil;
	ConsoleForm.speechSystem.haltSpeechAndSound;
  ConsoleForm.speechSystem.speakSound('music'); // turns it off
	end;

procedure TRuleEditorForm.MenuFileOpenWorldClick(Sender: TObject);
	var
    fileNameWithPath: string;
	begin
  self.commitChangesToRule;
	if not self.askForSaveWorldAndProceed then exit;
	if not ConsoleForm.askForSaveSessionAndProceed then exit;
  fileNameWithPath := getFileOpenInfo(kFileTypeWorld, domain.worldFileName, 'Choose a world file', kOtherExtNotOk);
  if fileNameWithPath = '' then exit;
  self.openWorldFile(fileNameWithPath);
  domain.world.setInitialFocus;
  domain.world.updateAvailable;
	self.updateViews;
  ConsoleForm.updateTitles;
	ConsoleForm.speechSystem.haltSpeechAndSound;
  ConsoleForm.speechSystem.speakSound('music'); // turns it off
  if domain.world.rules.count > 0 then
    ConsoleForm.speechSystem.doCommand(TSRule(domain.world.rules[0]).command.phrase);
  end;

procedure TRuleEditorForm.WMDropFiles(var Msg: TWMDropFiles);
  var
    CFileName: array[0..MAX_PATH] of Char;
  begin
  try
    if DragQueryFile(Msg.Drop, 0, CFileName, MAX_PATH) > 0 then
    begin
      if pos('.WLD', upperCase(CFileName)) <= 0 then exit;
  		self.commitChangesToRule;
			if not self.askForSaveWorldAndProceed then exit;
			if not ConsoleForm.askForSaveSessionAndProceed then exit;
  		self.openWorldFile(CFileName);
  		domain.world.setInitialFocus;
  		domain.world.updateAvailable;
			self.updateViews;
  		ConsoleForm.updateTitles;
			ConsoleForm.speechSystem.haltSpeechAndSound;
  		ConsoleForm.speechSystem.speakSound('music'); // turns it off
  		if domain.world.rules.count > 0 then
    		ConsoleForm.speechSystem.doCommand(TSRule(domain.world.rules[0]).command.phrase);
      Msg.Result := 0;
    end;
  finally
    DragFinish(Msg.Drop);
  end;
  end;

procedure TRuleEditorForm.openWorldFile(const fileNameWithPath: string);
  begin
  try
    startWaitMessage('Opening ' + extractFileName(fileNameWithPath));
  	Domain.loadWorld(fileNameWithPath);
  except
    on E: Exception do
      begin
      StopWaitMessage;
  		ShowMessage(E.message);
  		ShowMessage('Could not load file ' + fileNameWithPath);
   		domain.newWorld;
  		ConsoleForm.clearTranscript;
			self.updateViews;
   		exit;
      end;
	end;
  stopWaitMessage;
  {repeated for new}
  ConsoleForm.locationCacheValid := false;
  ConsoleForm.clearTranscript;
  rule := nil;
	self.updateForRuleChange;
  self.MapPaintBoxChanged;
  self.adjustScrollBars;
  self.updateViews;
  self.editRule(nil);
	end;

procedure TRuleEditorForm.MenuFileSaveWorldClick(Sender: TObject);
  var
    fileInfo: SaveFileNamesStructure;
  begin
  self.commitChangesToRule;
  if pos(upperCase(kUnsavedWorldFileName), upperCase(ExtractFileName(Domain.worldFileName))) > 0 then
    begin
    self.MenuFileSaveWorldAsClick(self);
    exit;
    end;
  lastSaveProceeded := getFileSaveInfo(kFileTypeWorld, kDontAskForFileName, Domain.worldFileName, fileInfo);
  if not lastSaveProceeded then exit;
  try
    startFileSave(fileInfo);
    Domain.saveWorld(fileInfo.tempFile);
    fileInfo.writingWasSuccessful := true;
  finally
    lastSaveProceeded := cleanUpAfterFileSave(fileInfo);
  end;
  domain.resetWorldChangeCount;
  domain.worldCommandList.clear;
  Domain.options.mostRecentWorld := fileInfo.newFile;
  self.updateMenus;
	end;

procedure TRuleEditorForm.MenuFileSaveWorldAsClick(Sender: TObject);
  var
    fileInfo: SaveFileNamesStructure;
	begin
  self.commitChangesToRule;
  lastSaveProceeded := getFileSaveInfo(kFileTypeWorld, kAskForFileName, Domain.worldFileName, fileInfo);
  if not lastSaveProceeded then exit;
  try
    startFileSave(fileInfo);
    Domain.saveWorld(fileInfo.tempFile);
    fileInfo.writingWasSuccessful := true;
  finally
    lastSaveProceeded := cleanUpAfterFileSave(fileInfo);
  end;
  Domain.worldFileName := fileInfo.newFile;
  Domain.options.mostRecentWorld := fileInfo.newFile;
  domain.resetWorldChangeCount;
  domain.worldCommandList.clear;
  self.updateMenus;
  ConsoleForm.updateTitles;
	end;

function TRuleEditorForm.askForSaveWorldAndProceed: boolean;
  var
    messageBoxResult: integer;
  begin
  result := true;
  if not domain.isWorldFileChanged then exit;
  messageBoxResult := MessageDlg('Save changes to world ' + extractFileName(domain.worldFileName) + '?',
      mtConfirmation, mbYesNoCancel, 0);
  case messageBoxResult of
    IDCANCEL: result := false;
    IDYES:
    	begin
      self.MenuFileSaveWorldClick(Self);
      result := self.lastSaveProceeded;
      end;
    IDNO: result := true;
    else
      begin
      ShowMessage('Error with save request dialog.');
      result := true;
      end;
    end;
  end;

procedure TRuleEditorForm.MenuFileMergeWithWorldClick(Sender: TObject);
	var
    fileNameWithPath: string;
    oldRuleCount: integer;
    oldVariablesCount: integer;
    i: integer;
    newRulesCommand: TSNewRulesCommand;
    rule: TSRule;
    variable: TSVariable;
	begin
  self.commitChangesToRule;
  fileNameWithPath := getFileOpenInfo(kFileTypeWorld, domain.worldFileName,
  		'Choose a world file to merge into this one', kOtherExtNotOk);
  if fileNameWithPath = '' then exit;
  oldRuleCount := Domain.world.rules.count;
  oldVariablesCount := Domain.world.variables.count;
  newRulesCommand := TSNewRulesCommand.create;
  newRulesCommand.creator := 'merging ' + extractFileName(fileNameWithPath);
  try
    startWaitMessage('Opening ' + extractFileName(fileNameWithPath));
  	Domain.mergeWorld(fileNameWithPath);
  except
    on E: Exception do
      begin
      StopWaitMessage;
  		ShowMessage(E.message);
  		ShowMessage('Could not correctly merge in file ' + fileNameWithPath);
      // clear out those new rules and variables
      for i := oldRuleCount to Domain.world.rules.count - 1 do
        begin
        rule := TSRule(Domain.world.rules[i]);
        Domain.world.rules.remove(rule);
        rule.free;
        end;
  		for i := oldVariablesCount to Domain.world.variables.count - 1 do
  			begin
    		variable := TSVariable(Domain.world.variables[i]);
        Domain.world.variables.remove(variable);
    		variable.free;
    		end;
      newRulesCommand.free;
			self.updateViews;
   		exit;
      end;
	end;
  stopWaitMessage;
  ConsoleForm.locationCacheValid := false;
  domain.world.deselectAllExcept(nil);
  // select new items
  for i := oldRuleCount to Domain.world.rules.count - 1 do
    begin
    rule := TSRule(Domain.world.rules[i]);
    newRulesCommand.addRule(rule);
    rule.selected := true;
    end;
  for i := oldVariablesCount to Domain.world.variables.count - 1 do
  	begin
    variable := TSVariable(Domain.world.variables[i]);
    variable.selected := true;
    end;
 	Domain.worldCommandList.doCommand(newRulesCommand);
  if Domain.world.rules.count > 0 then
    self.editRule(Domain.world.rules[Domain.world.rules.count - 1])
  else
  	self.editRule(nil);
  self.scrollGridSelectionsIntoView(kFromTop);
	self.updateForRuleChange;
  self.updateViews;
	end;

procedure TRuleEditorForm.MenuFileExitClick(Sender: TObject);
  begin
  self.commitChangesToRule;
	if not self.askForSaveWorldAndProceed then exit;
	if not ConsoleForm.askForSaveSessionAndProceed then exit;
  ConsoleForm.cleanUpBeforeExit;
  Application.terminate;
  end;

// ----------------------------------------------------------------- @Edit menu
procedure TRuleEditorForm.MenuEditUndoClick(Sender: TObject);
	begin
  self.commitChangesToRule;
  domain.worldCommandList.undoLast;
	end;

procedure TRuleEditorForm.MenuEditRedoClick(Sender: TObject);
	begin
  self.commitChangesToRule;
  domain.worldCommandList.redoLast;
	end;

procedure TRuleEditorForm.MenuEditCutClick(Sender: TObject);
	var
  	clip: string;
    key: word;
    Shift: TShiftState;
	begin
  // should not commit rule because may be in floating edit
  if rule = nil then exit;
	clip := '';
  key := vk_Delete;
  Shift := [];
	if self.ActiveControl = RequirementsListBox then
  	begin
  	if RequirementsListBox.itemIndex < RequirementsListBox.items.count then
      begin
      if RequirementsListBox.itemIndex < 0 then exit;
  		clip := Trim(RequirementsListBox.items[RequirementsListBox.itemIndex]);
      if pos('~', clip) = 1 then
      	clip := copy(clip, 2, length(clip));
			self.ListBoxKeyUp(RequirementsListBox, key, Shift);
      end;
  	end
	else if self.ActiveControl = ChangesListBox then
  	begin
  	if ChangesListBox.itemIndex < ChangesListBox.items.count then
      begin
      if ChangesListBox.itemIndex < 0 then exit;
  		clip := Trim(ChangesListBox.items[ChangesListBox.itemIndex]);
      if pos('~', clip) = 1 then
      	clip := copy(clip, 2, length(clip));
			self.ListBoxKeyUp(ChangesListBox, key, Shift);
      end;
  	end
	else if self.ActiveControl is TMemo then
    begin
  	clip := (self.ActiveControl as TMemo).selText;
    (self.ActiveControl as TMemo).selText := '';
    end
	else if self.ActiveControl is TEdit then
    begin
  	clip := (self.ActiveControl as TEdit).selText;
    (self.ActiveControl as TEdit).selText := '';
    end;
	Clipboard.setTextBuf(pchar(clip));
	end;

procedure TRuleEditorForm.MenuEditCopyClick(Sender: TObject);
	var
  	clip: string;
	begin
  // should not commit rule because may be in floating edit
  if rule = nil then exit;
	clip := '';
	if self.ActiveControl = RequirementsListBox then
  	begin
    if RequirementsListBox.itemIndex < 0 then exit;
  	if RequirementsListBox.itemIndex < RequirementsListBox.items.count then
  		clip := Trim(RequirementsListBox.items[RequirementsListBox.itemIndex]);
    if pos('~', clip) = 1 then
      clip := copy(clip, 2, length(clip));
  	end
	else if self.ActiveControl = ChangesListBox then
  	begin
    if ChangesListBox.itemIndex < 0 then exit;
  	if ChangesListBox.itemIndex < ChangesListBox.items.count then
  		clip := Trim(ChangesListBox.items[ChangesListBox.itemIndex]);
    if pos('~', clip) = 1 then
      clip := copy(clip, 2, length(clip));
  	end
	else if self.ActiveControl is TMemo then
  	clip := (self.ActiveControl as TMemo).selText
	else if self.ActiveControl is TEdit then
  	clip := (self.ActiveControl as TEdit).selText;
	Clipboard.setTextBuf(pchar(clip));
	end;

procedure TRuleEditorForm.MenuEditPasteClick(Sender: TObject);
	var
  	MyHandle: THandle;
  	TextPtr: PChar;
 		clip: string;
    edit: TEdit;
	begin
  // should not commit rule because may be in floating edit
  if rule = nil then exit;
	clip := '';
  ClipBoard.Open;
	try
  	MyHandle := Clipboard.GetAsHandle(CF_TEXT);
  	TextPtr := GlobalLock(MyHandle);
  	clip := StrPas(TextPtr);
  	GlobalUnlock(MyHandle);
	finally
  	Clipboard.Close;
	end;
	if self.ActiveControl = RequirementsListBox then
  	begin
    if RequirementsListBox.items.count < 1 then exit;
    indexEdited := RequirementsListBox.itemIndex;
    if indexEdited < 0 then
    	indexEdited := RequirementsListBox.items.count - 1;
    if indexEdited > RequirementsListBox.items.count - 1 then
    	indexEdited := RequirementsListBox.items.count - 1;
    edit := RequirementsEdit;
    edit.text := clip;
    self.ListBoxEditExit(edit);
  	end
	else if self.ActiveControl = ChangesListBox then
  	begin
    if ChangesListBox.items.count < 1 then exit;
    indexEdited := ChangesListBox.itemIndex;
    if indexEdited < 0 then
    	indexEdited := ChangesListBox.items.count - 1;
    if indexEdited > ChangesListBox.items.count - 1 then
    	indexEdited := ChangesListBox.items.count - 1;
    edit := ChangesEdit;
    edit.text := clip;
    self.ListBoxEditExit(edit);
  	end
	else if self.ActiveControl is TMemo then
  	(self.ActiveControl as TMemo).selText := clip
	else if self.ActiveControl is TEdit then
  	(self.ActiveControl as TEdit).selText := clip
  // PDF PORT - removed unneeded parens
  else Beep;
	end;

procedure TRuleEditorForm.MenuEditPreferencesClick(Sender: TObject);
	begin
  self.commitChangesToRule;
  if PreferencesForm = nil then exit;
  PreferencesForm.options := domain.options;
  if PreferencesForm.showModal = mrOK then with domain.options do
    begin
    domain.options := PreferencesForm.options;
    self.updateForChangeToDomainOptions;
    end;
	end;

// ----------------------------------------------------------------- @Rule menu
procedure TRuleEditorForm.MenuRuleNewClick(Sender: TObject);
  var
  	newRule: TSRule;
    variable: TSvariable;
    newRulesCommand: TSNewRulesCommand;
	begin
  self.commitChangesToRule;
  newRulesCommand := TSNewRulesCommand.create;
  newRule := domain.world.newRule;
  newRulesCommand.addRule(newRule);
	(*
  if ListPages.ActivePage = TabSheetTable then
    begin

    end
 	else if ListPages.ActivePage = TabSheetBrowse then
    begin
    end
	else if ListPages.ActivePage = TabSheetMap then
    begin
  	if lastChoice is TSVariable then
    	newRule.setContext(TSVariable(lastChoice).phrase)
  	else if lastChoice is TSRule then
    	newRule.setContext(TSRule(lastChoice).context.phrase);
    end;
    *)
  variable := domain.world.firstSelectedVariable;
  if variable <> nil then
    newRule.setContext(variable.phrase)
  else if rule <> nil then
    newRule.setContext(rule.context.phrase);
  domain.world.deselectAllExcept(newRule);
  newRule.selected := true;
  domain.worldCommandList.doCommand(newRulesCommand);
  self.editRule(newRule);
  if trim(newRule.context.phrase) <> '' then
  	self.activeControl := CommandEdit
  else
  	self.activeControl := ContextEdit;
	end;

procedure TRuleEditorForm.MenuRuleDuplicateClick(Sender: TObject);
  var
  	newRule: TSRule;
    newRulesCommand: TSNewRulesCommand;
	begin
  self.commitChangesToRule;
  if rule = nil then exit;
  newRulesCommand := TSNewRulesCommand.create;
  newRulesCommand.creator := 'duplicating';
  newRule := domain.world.newRule;
  newRulesCommand.addRule(newRule);
  newRule.setContext(rule.context.phrase);
  newRule.setCommand(rule.command.phrase);
  newRule.setReply(rule.reply);
  newRule.setMove(rule.move.phrase);
  newRule.setRequirements(rule.requirementsString);
  newRule.setChanges(rule.changesString);
  domain.world.deselectAllExcept(newRule);
  newRule.selected := true;
  domain.worldCommandList.doCommand(newRulesCommand);
  self.editRule(newRule);
  self.updateForRuleChange;
	end;

procedure TRuleEditorForm.MenuRuleDeleteClick(Sender: TObject);
	begin
  self.commitChangesToRule;
  if (rule <> nil) and (rule.selected) then
  	self.editRule(nil);
  domain.world.deleteSelectedRules;
  previousChoice := nil;
  lastChoice := nil;
  self.updateForRuleChange;
	end;

// -------------------------------------------------------------- @Display menu
procedure TRuleEditorForm.MenuMapsShowCommandsClick(Sender: TObject);
	begin
  self.commitChangesToRule;
  domain.options.showCommandsInMap := not domain.options.showCommandsInMap;
	MenuMapsShowCommands.checked := domain.options.showCommandsInMap;
  if not self.switchToPage(TabSheetMap) then
  	MapPaintBoxChanged;
	end;

procedure TRuleEditorForm.MenuMapFontClick(Sender: TObject);
	begin
  self.switchToPage(TabSheetMap);
  fontDialog.font := mapImage.canvas.font;
  if fontDialog.execute then
    begin
    mapImage.canvas.font := fontDialog.font;
    self.MapPaintBoxChanged;
    firstListBox.{canvas.}font := fontDialog.font;
    secondListBox.{canvas.}font := fontDialog.font;
    ruleGrid.{canvas.}font := fontDialog.font;
    end;
	end;

procedure TRuleEditorForm.MenuDisplayShowButtonBarClick(Sender: TObject);
	begin
  self.commitChangesToRule;
  domain.options.showButtonBar := not domain.options.showButtonBar;
  MenuDisplayShowButtonBar.checked := domain.options.showButtonBar;
  PanelButtonBar.Visible := domain.options.showButtonBar;
	end;

procedure TRuleEditorForm.MenuOptionsShowRuleEditorClick(Sender: TObject);
	begin
  self.commitChangesToRule;
  domain.options.showRuleEditor := not domain.options.showRuleEditor;
  MenuOptionsShowRuleEditor.checked := domain.options.showRuleEditor;
  if domain.options.showRuleEditor then
    begin
    PanelEditor.height := domain.options.editorPanelEditorHeight;
    if PanelEditor.height = 1 then
      PanelEditor.height := 2;
    PanelEditor.enabled := true;
    end
  else
    begin
    domain.options.editorPanelEditorHeight := PanelEditor.height;
    PanelEditor.height := 1;
    PanelEditor.enabled := false;
    end;
  self.resize;
	end;

// ---------------------------------------------------------------- @Tools menu
procedure TRuleEditorForm.MenuToolsSearchClick(Sender: TObject);
	begin
  self.commitChangesToRule;
  FindDialog.execute;
	end;

procedure TRuleEditorForm.FindDialogFind(Sender: TObject);
	begin
  if FindDialog.findText <> '' then
    self.searchForAndSelectRule(FindDialog.findText, not (frMatchCase in FindDialog.options), frDown in FindDialog.options);
	end;

procedure TRuleEditorForm.MenuMapsQuickContextsClick(Sender: TObject);
	begin
  self.commitChangesToRule;
  self.switchToPage(TabSheetMap);
  if ContextWizardForm.initialize then
		ContextWizardForm.ShowModal;
	end;

procedure TRuleEditorForm.MenuMapLinkWizardClick(Sender: TObject);
	begin
  self.commitChangesToRule;
  self.switchToPage(TabSheetMap);
  if LinkWizardForm.initialize then
		LinkWizardForm.ShowModal;
	end;

procedure TRuleEditorForm.MenuMapQuickCommandsClick(Sender: TObject);
	begin
  self.commitChangesToRule;
  self.switchToPage(TabSheetMap);
	if CommandWizardForm.initialize then
		CommandWizardForm.ShowModal;
	end;

procedure TRuleEditorForm.MenuEditLogFileClick(Sender: TObject);
	begin
  self.commitChangesToRule;
  ChangeLogForm.loadChangeLog;
	ChangeLogForm.Show;
	ChangeLogForm.invalidate;
	ChangeLogForm.refresh;
  ChangeLogForm.scrollLogEndIntoView;
	end;

// ----------------------------------------------------------------- @Help menu
procedure TRuleEditorForm.MenuHelpRegisterClick(Sender: TObject);
	begin
  self.commitChangesToRule;
	ConsoleForm.MenuHelpRegisterClick(sender);
	end;

procedure TRuleEditorForm.MenuHelpAboutClick(Sender: TObject);
	begin
  self.commitChangesToRule;
	ConsoleForm.MenuHelpAboutClick(sender);
	end;

// ---------------------------------------------------------------- @Button bar
procedure TRuleEditorForm.MenuRuleRaiseClick(Sender: TObject);
	begin
  self.commitChangesToRule;
  self.switchToPage(TabSheetTable);
  domain.world.raiseSelectedRules;
	end;

procedure TRuleEditorForm.MenuRuleLowerClick(Sender: TObject);
	begin
  self.commitChangesToRule;
  self.switchToPage(TabSheetTable);
  domain.world.lowerSelectedRules;
	end;

procedure TRuleEditorForm.NewRuleButtonClick(Sender: TObject);
	begin
  self.MenuRuleNewClick(self);
	end;

procedure TRuleEditorForm.DuplicateRuleButtonClick(Sender: TObject);
	begin
  self.MenuRuleDuplicateClick(self);
	end;

procedure TRuleEditorForm.DeleteRuleButtonClick(Sender: TObject);
	begin
  self.MenuRuleDeleteClick(self);
	end;

procedure TRuleEditorForm.MoveUpButtonClick(Sender: TObject);
	begin
  self.MenuRuleRaiseClick(self);
	end;

procedure TRuleEditorForm.MoveDownButtonClick(Sender: TObject);
	begin
  self.MenuRuleLowerClick(self);
	end;

// ------------------------------------------------------------------ @Updating
procedure TRuleEditorForm.updateForChangeToDomainOptions;
  begin
  with domain.options do
    begin
  	// table -- also set default row height for table
  	if tableFontName <> '' then ruleGrid.font.name := tableFontName;
  	if tableFontSize > 0 then ruleGrid.font.size := tableFontSize;
  	ruleGrid.canvas.font := ruleGrid.font;
  	ruleGrid.defaultRowHeight := ruleGrid.canvas.textheight('W') + 2;
  	// map
  	if mapFontName <> '' then mapImage.canvas.font.name := mapFontName;
  	if mapFontSize > 0 then mapImage.canvas.font.size := mapFontSize;
  	// browser
  	if browserFontName <> '' then firstListBox.font.name := browserFontName;
  	if browserFontSize > 0 then firstListBox.font.size := browserFontSize;
  	secondListBox.font := firstListBox.font;
    // options menu
  	MenuDisplayShowButtonBar.checked := domain.options.showButtonBar;
  	PanelButtonBar.Visible := domain.options.showButtonBar;
    if MenuOptionsShowRuleEditor.checked <> domain.options.showRuleEditor then
      begin
  		MenuOptionsShowRuleEditor.checked := domain.options.showRuleEditor;
  		if domain.options.showRuleEditor then
    		begin
    		PanelEditor.height := 2;
   			PanelEditor.enabled := true;
    		end
  		else
   	  	begin
    		PanelEditor.height := 1;
    		PanelEditor.enabled := false;
    		end;
  		self.resize;
      end;
	  MenuMapsShowCommands.checked := domain.options.showCommandsInMap;
    if (domain.options.buttonSymbols <> self.buttonSymbols) or (startingUp) then
      begin
      self.buttonSymbols := domain.options.buttonSymbols;
  		ConsoleForm.setButtonGlyphs(domain.options.buttonSymbols);
      self.setButtonGlyphs;
      self.invalidate;
  		// replyPicture.visible := not buttonSymbols;
      self.setOrganizeByField(domain.options.browseBy);
      end;
    if domain.options.browseBy <> self.organizeByField then
      self.setOrganizeByField(domain.options.browseBy);
 	  // updating
  	self.updateForRuleChange;
  	self.MapPaintBoxChanged;
  	self.adjustScrollBars;
  	self.updateViews;
    end;
  end;

procedure TRuleEditorForm.updateMenus;
  begin
  // unfinished - maybe want better save as testing
  MenuFileSaveWorld.enabled := domain.isWorldFileChanged;
  MenuFileSaveWorldAs.enabled := true;
  self.updateMenusForUndoRedo;
  end;

procedure TRuleEditorForm.updateViews;
  begin
  if Application.terminated then exit;
	self.updateMenus;
  ConsoleForm.updateViews;
  PictureForm.updateViews;
  end;

procedure TRuleEditorForm.updateForRuleChange;
  begin
  self.RuleGrid.rowCount := domain.world.rules.count + 1;
  if self.RuleGrid.rowCount > 1 then
  	self.RuleGrid.FixedRows := 1;
  self.RuleGrid.invalidate;
  ConsoleForm.updateVariables;
  ConsoleForm.VariablesListBox.invalidate;
  self.MapPaintBoxChanged;
  self.setOrganizeByField(organizeByField);
  //if (domain.world.focus = nil) or (domain.world.focus = domain.world.emptyEntry) then
  //  domain.world.setInitialFocus;
  domain.world.updateAvailable;
  ConsoleForm.updateViews;
  self.updateRuleNumberLabel;
  end;

procedure TRuleEditorForm.updateMenusForUndoRedo;
	begin
  if domain.worldCommandList.isUndoEnabled then
  	begin
    MenuEditUndo.enabled := true;
    MenuEditUndo.caption := '&Undo ' + domain.worldCommandList.undoDescription;
    end
  else
  	begin
    MenuEditUndo.enabled := false;
    MenuEditUndo.caption := 'Can''t undo';
    end;
  if domain.worldCommandList.isRedoEnabled then
  	begin
    MenuEditRedo.enabled := true;
    MenuEditRedo.caption := '&Redo ' + domain.worldCommandList.redoDescription;
    end
  else
  	begin
    MenuEditRedo.enabled := false;
    MenuEditRedo.caption := 'Can''t redo';
    end;
  end;

// -------------------------------------------------------------------- @Events
procedure TRuleEditorForm.ListBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
  	desiredStateWrapper: TSDesiredStateVariableWrapper;
    wrapperObject: TObject;
    listBox: TListBox;
  	oldIndex: integer;
    oldTopIndex: integer;
	begin
  lastClickAtLeft := false;
  listBox := sender as TListBox;
  if (listBox.itemIndex < 0) or (listBox.itemIndex >= listBox.items.count) then exit;
  if listBox.itemIndex = listBox.items.count - 1 then
    self.ListBoxDblClick(sender)
  else if x < 10 then
    begin
    oldIndex := listBox.itemIndex;
    oldTopIndex := listBox.TopIndex;
    lastClickAtLeft := true;
    wrapperObject := listBox.items.objects[listBox.itemIndex];
    desiredStateWrapper := wrapperObject as TSDesiredStateVariableWrapper;
    // temporarily invert to get new display string
    desiredStateWrapper.invertDesiredState;
    listBox.items[listBox.itemIndex] := desiredStateWrapper.displayString;
    listBox.items.objects[listBox.itemIndex] := desiredStateWrapper;
    // restore to current state
    desiredStateWrapper.invertDesiredState;
    // now create command for undo
    self.commitChangesToRule;
    listBox.itemIndex := oldIndex;
    listBox.TopIndex := oldTopIndex;
    end;
	end;

procedure TRuleEditorForm.ListBoxDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
	begin
	//
	end;

procedure TRuleEditorForm.EditDragDrop(Sender, Source: TObject; X, Y: Integer);
	begin
  if Source = MapImage then
    begin
    if lastChoice = nil then exit;
    if lastChoice is TSRule then
    	(Sender as TEdit).Text := TSRule(lastChoice).command.phrase
    else
    	(Sender as TEdit).Text := TSVariable(lastChoice).phrase
    end
 	else if Source = FirstListBox then
  	begin
    if FirstListBox.itemIndex < 0 then exit;
    (Sender as TEdit).Text := FirstListBox.items[FirstListBox.itemIndex];
  	end;
  self.commitChangesToRule;
	end;

procedure TRuleEditorForm.listBoxNewStatement(listBox: TListBox; const newStatement: string);
  begin
  if listBox = RequirementsListBox then
    begin
  	rule.setRequirements(newStatement);
  	self.fillListBox(listBox, rule.requirements);
    end
  else if listBox = ChangesListBox then
    begin
  	rule.setChanges(newStatement);
  	self.fillListBox(listBox, rule.changes);
    end;
  domain.worldChangeDone; {not undoable}
  end;

procedure TRuleEditorForm.ListBoxDragDrop(Sender,
  Source: TObject; X, Y: Integer);
  var
  	theText: string;
    listBox: TListBox;
	begin
  listBox := sender as TListBox;
 	if Source = MapImage then
 		begin
    if lastChoice = nil then exit;
    if lastChoice is TSRule then
    	theText := TSRule(lastChoice).command.phrase
    else
    	theText := TSVariable(lastChoice).phrase;
    listBox.items.insertObject(listBox.items.count - 1, theText, nil);
    self.commitChangesToRule;
  	end
 	else if Source = FirstListBox then
 		begin
    if FirstListBox.itemIndex < 0 then exit;
    theText := FirstListBox.items[FirstListBox.itemIndex];
    listBox.items.insertObject(listBox.items.count - 1, theText, nil);
    self.commitChangesToRule;
  	end
  else
    begin
    exit;
    end;
	end;

procedure TRuleEditorForm.ListBoxKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
  var
    listBox: TListBox;
	begin
  listBox := sender as TListBox;
	if (Key = vk_Delete) or (key = vk_back) then
    begin
    if listBox.itemIndex < 0 then exit;
    if listBox.itemIndex >= listBox.items.count - 1 then exit;
    listBox.items.delete(listBox.itemIndex);
 		self.commitChangesToRule;
 		Key := 0;
  	end
	else if (Key = vk_Return) then
  	begin
  	Key := 0;
    if not ignoreNextEnter then
    	self.ListBoxDblClick(sender);
    ignoreNextEnter := false;
  	end;
	end;

procedure TRuleEditorForm.positionEditForListBox(edit: TEdit; listBox: TListBox);
  var
    tildeWidth: integer;
  begin
  edit.top := listBox.top + 2 +
    	(listBox.itemIndex - listBox.TopIndex) * listBox.ItemHeight;
  tildeWidth := listbox.canvas.textWidth('~');
  edit.left := listBox.left + 2 + tildeWidth;
  edit.width := listBox.width - 4 - tildeWidth;
  edit.height := listBox.ItemHeight + 2;
  edit.show;
  self.FocusControl(edit);
  end;

procedure TRuleEditorForm.ListBoxDblClick(Sender: TObject);
  var
  	desiredStateWrapper: TSDesiredStateVariableWrapper;
    listBox: TListBox;
    edit: TEdit;
    wrapperObject: TObject;
	begin
  if lastClickAtLeft then exit;
  indexEdited := -1;
  listBox := sender as TListBox;
  if sender = RequirementsListBox then
    edit := RequirementsEdit
  else if sender = ChangesListBox then
    edit := ChangesEdit
  else
    exit;
  if (listBox.itemIndex < 0) or (listBox.itemIndex >= listBox.items.count) then exit;
  if listBox.itemIndex < listBox.items.count - 1 then
    begin
    wrapperObject := listBox.items.objects[listBox.itemIndex];
  	desiredStateWrapper := wrapperObject as TSDesiredStateVariableWrapper;
  	edit.Text := desiredStateWrapper.variable.phrase;
    end
  else
    edit.Text := '';
  indexEdited := listBox.itemIndex;
  self.positionEditForListBox(edit, listBox);
	end;

procedure TRuleEditorForm.ListBoxEditExit(Sender: TObject);
	var
  	oldIndex: integer;
  	desiredStateWrapper: TSDesiredStateVariableWrapper;
    edit: TEdit;
    listBox: TListBox;
    oldTopIndex: integer;
	begin
  edit := sender as TEdit;
  edit.hide;
  if edit = RequirementsEdit then
    listBox := RequirementsListBox
  else if edit = ChangesEdit then
    listBox := ChangesListBox
  else exit;
  if (indexEdited < 0) or (indexEdited > listbox.items.count - 1) then
    exit;
  if (indexEdited = listBox.items.count - 1) then
    begin
    if (trim(edit.Text) <> '') then
      begin
 		  listBox.items[listBox.items.count - 1] := edit.Text;
     	oldIndex := indexEdited;
   		oldTopIndex := listBox.TopIndex;
      listBox.items.add('');
      self.commitChangesToRule;
    	//listBox.itemIndex := index;
      //if index - oldTopIndex + 1 > (listBox.clientHeight + listBox.itemHeight - 1) div listBox.itemHeight then
      //	listBox.TopIndex := oldTopIndex + 1
      //else
    	listBox.itemIndex := oldIndex;
     	listBox.TopIndex := oldTopIndex;
      end
    end
  else
    begin
    oldIndex := indexEdited;
    oldTopIndex := listBox.TopIndex;
    desiredStateWrapper := listBox.items.objects[oldIndex] as TSDesiredStateVariableWrapper;
    if desiredStateWrapper <> nil then
    	listBox.items[oldIndex] := desiredStateWrapper.displayLeader + edit.Text;
    self.commitChangesToRule;
    listBox.itemIndex := oldIndex;
    listBox.TopIndex := oldTopIndex;
    end;
  indexEdited := -1;
  edit.Text := '';
	end;

procedure TRuleEditorForm.ListBoxEditKeyPress(Sender: TObject;
  var Key: Char);
  var
    edit: TEdit;
    listBox: TListBox;
  	desiredStateWrapper: TSDesiredStateVariableWrapper;
	begin
  edit := sender as TEdit;
  if edit = RequirementsEdit then
    listBox := RequirementsListBox
  else if edit = ChangesEdit then
    listBox := ChangesListBox
  else exit;

  if (key = char(13)) then {enter}
    begin
    Key := char(0);
  	self.FocusControl(listBox);
    ignoreNextEnter := true;
    end
  else if Key = char(27) then  {escape}
    begin
  	Key := char(0);
    desiredStateWrapper := nil;
    if (indexEdited >= 0) and (indexEdited < listBox.items.count) then
    	desiredStateWrapper := listBox.items.objects[indexEdited] as TSDesiredStateVariableWrapper;
    if desiredStateWrapper <> nil then
    	edit.Text := desiredStateWrapper.variable.phrase
    else
      edit.Text := '';
  	self.FocusControl(listBox);
    end;
(*
  if Key = Char(1) then
    begin
    if (ExtraChangesEntry.items.count = 0) or
    		(NewExtraChanges.Text = ExtraChangesEntry.items[0]) then
    	begin
    	NewExtraChanges.Text := '';
  		ResponseEditorForm.FocusControl(ExtraChangesEntry);
    	Key := Char(0);
    	end;
    end;
 *)
 end;

procedure TRuleEditorForm.ListBoxExit(Sender: TObject);
	begin
	(sender as TListBox).itemIndex := -1;
	end;

// ------------------------------------------------------------------ @Commands
procedure TRuleEditorForm.commandChangedNotification(command: KfCommand; state: KfCommandChangeType);
	begin
  case state of
    commandDone: domain.worldChangeDone;
    commandUndone: domain.worldChangeUndone;
  end;
	end;

procedure TRuleEditorForm.trackLastCommand;
  begin
  self.editRule(lastCommand);
  self.updateForRuleChange;
  end;

procedure TRuleEditorForm.EditEnterCommit(Sender: TObject);
	begin
  self.commitChangesToRule;
	end;

procedure TRuleEditorForm.selectEditorField(field: integer);
  begin
  if rule = nil then exit;

  case field of
		kRuleContext:
      begin
    	ContextEdit.SetFocus;
      ContextEdit.selStart := 0;
      ContextEdit.selLength := length(ContextEdit.text);
      end;
		kRuleCommand:
      begin
    	CommandEdit.SetFocus;
      CommandEdit.selStart := 0;
      CommandEdit.selLength := length(CommandEdit.text);
      end;
		kRuleReply:
      begin
    	ReplyMemo.SetFocus;
      ReplyMemo.selStart := 0;
      ReplyMemo.selLength := length(ReplyMemo.text);
      end;
		kRuleMove:
      begin
    	MoveEdit.SetFocus;
      MoveEdit.selStart := 0;
      MoveEdit.selLength := length(MoveEdit.text);
      end;
		kRuleRequirements:
      begin
    	requirementsListBox.SetFocus;
      if requirementsListBox.items.count > 0 then
      	requirementsListBox.itemIndex := 0;
      end;
		kRuleChanges:
      begin
    	changesListBox.SetFocus;
      if changesListBox.items.count > 0 then
      	changesListBox.itemIndex := 0;
      end;
    end;
  end;

procedure TRuleEditorForm.updateForFieldChange(fieldType: integer);
  begin
  if rule = nil then exit;
  case fieldType of
    kRuleContext: ContextEdit.Text := rule.context.phrase;
    kRuleCommand: CommandEdit.Text := rule.command.phrase;
    kRuleReply: ReplyMemo.Text := rule.reply;
    kRuleMove: MoveEdit.Text := rule.move.phrase;
    kRuleRequirements: self.fillListBox(requirementsListBox, rule.requirements);
    kRuleChanges: self.fillListBox(changesListBox, rule.changes);
  end;
  end;

procedure TRuleEditorForm.editRule(rule: TSRule);
  begin
  self.commitChangesToRule;
  self.rule := rule;
  self.loadAllRuleFields;
  end;

procedure TRuleEditorForm.loadAllRuleFields;
  var newIndex: integer;
  begin
  domain.beginUpdate;
  // if cacheInvalid then fillChoices;
  if rule <> nil then
    begin
		ContextEdit.Text := rule.context.phrase;
		CommandEdit.Text := rule.command.phrase;
		MoveEdit.Text := rule.move.phrase;
		ReplyMemo.Text := rule.reply;
		self.fillListBox(requirementsListBox, rule.requirements);
		self.fillListBox(changesListBox, rule.changes);
    end
  else
    begin
		ContextEdit.Text := '';
		CommandEdit.Text := '';
		MoveEdit.Text := '';
		ReplyMemo.Text := '';
  	requirementsListBox.clear;
  	changesListBox.clear;
    end;
  self.updateRuleNumberLabel;
  self.setEnabledForControl(ContextEdit, rule <> nil);
	self.setEnabledForControl(CommandEdit, rule <> nil);
	self.setEnabledForControl(MoveEdit, rule <> nil);
	self.setEnabledForControl(ReplyMemo, rule <> nil);
  self.setEnabledForControl(requirementsListBox, rule <> nil);
  self.setEnabledForControl(changesListBox, rule <> nil);
  //RecordSelectionInformation(selectionInformation, self.ActiveControl as TWinControl);
  domain.endUpdate;
  newIndex := secondListBox.items.indexOfObject(rule);
  if secondListBox.itemIndex <> newIndex then
  	secondListBox.itemIndex := newIndex;
  end;

procedure TRuleEditorForm.updateRuleNumberLabel;
  begin
  if rule <> nil then
    RuleNumberLabel.caption := '#' + intToStr(domain.world.rules.IndexOf(rule) + 1)
  else
    RuleNumberLabel.caption := '';
  end;

procedure TRuleEditorForm.setEnabledForControl(control: TWinControl; enable: boolean);
  var
    aColor: TColor;
  begin
  if control = nil then exit;
  if enable then
    aColor := clWindow
  else
  	aColor := clBtnFace;
  if control is TEdit then
    (control as TEdit).color := aColor
  else if control is TMemo then
    (control as TMemo).color := aColor
  else if control is TListBox then
    (control as TListBox).color := aColor;
  control.enabled := enable;
  end;

// --------------------------------------------------------------------- @Table
procedure TRuleEditorForm.RuleGridDrawCell(Sender: TObject; Col,
  Row: Integer; Rect: TRect; State: TGridDrawState);
	var
  	cString: array[0..256] of char;
  	selected, focused: boolean;
  	ruleString: string;
    rule: TSRule;
	begin
  rule := nil;
	if (row > 0) and (row <= domain.world.rules.count) then
		rule := TSRule(domain.world.rules[row - 1]);
  if rule = nil then with RuleGrid.canvas do
    begin
    if row <> 0 then exit;
    // draw first row headers
    brush.color := clLtGray;
    font.color := clWindowText;
    font.style := [];
    fillRect(rect);
    ruleString := TSRule.headerForField(col);
    rect.left := rect.left + 5;
    rect.right := rect.right - 2;
    StrPLCopy(cString, ruleString, 255);
    DrawText(handle, cString, strLen(cString), rect, DT_LEFT or DT_NOPREFIX);
    exit;
    end;
  // draw rule cell text
  selected := rule.selected;
  focused := rule = self.rule;
  setCanvasColorsForSelection(RuleGrid.canvas, selected, focused, false);
  with RuleGrid.canvas do
    begin
		fillRect(rect);
    ruleString := rule.getTextForField(col);
		StrPLCopy(cString, ruleString, 255);
    rect.left := rect.left + 5;
		rect.right := rect.right - 2;
  	DrawText(handle, cString, strLen(cString), rect, DT_LEFT or DT_NOPREFIX);
    end;
	end;

procedure setCanvasColorsForSelection(canvas: TCanvas; selected, focused, isCommandInMap: boolean);
  begin
  with canvas do
    begin
    brush.color := clWindow;
    font.color := clWindowText;
    font.style := [];
    if focused and selected then
      begin
      brush.color := domain.options.selectedItemColor;
      font.color := domain.options.selectedTextColor;
      font.style := [fsBold];
      end
    else if focused then
      begin
      font.style := [fsBold];
      end
    else if selected then
      begin
      brush.color := domain.options.selectedItemColor;
      font.color := domain.options.selectedTextColor;
      end;
    if isCommandInMap then
      font.color := domain.options.commandTextColorInMap;
    end;
  end;

procedure TRuleEditorForm.RuleGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
	begin
 //	if ssCtrl in Shift then
  // 	begin
	//	RuleGrid.beginDrag(false);
 //		RuleGrid.FGridState := gsNormal;
 //		end;
{  self.commitChangesToRule;
  if RuleGrid.row = 0 then exit;
  if RuleGrid.row > domain.world.rules.count then exit;
  self.editRule(TSRule(domain.world.rules[RuleGrid.row - 1])); }
	end;

procedure TRuleEditorForm.RuleGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
  	rule: TSRule;
    mustRedraw: boolean;
    index, i: integer;
	begin
	//if ssCtrl in Shift then
  // 	begin
	//	RuleGrid.beginDrag(true);
  //		RuleGrid.FGridState := gsNormal;
	//	end;
  if RuleGrid.row = 0 then exit;
  if RuleGrid.row > domain.world.rules.count then exit;
  //RuleGrid.beginDrag(false);
  self.commitChangesToRule;
  index := RuleGrid.row - 1;
  rule := TSRule(domain.world.rules[index]);
  mustRedraw := false;
  // shift
  if (ssShift in shift) then
    begin
    if (lastSingleRuleIndex >= 0) and (lastSingleRuleIndex <= domain.world.rules.count - 1)
      and (lastSingleRuleIndex <> index) then
        begin
        domain.world.deselectAllExcept(rule);
        if lastSingleRuleIndex < index then
          for i := lastSingleRuleIndex to index do TSRule(domain.world.rules[i]).selected := true
        else if lastSingleRuleIndex > index then
          for i := lastSingleRuleIndex downto index do TSRule(domain.world.rules[i]).selected := true;
        mustRedraw := true;
        end;
    end
  // control
  else if (ssCtrl in Shift) then
    begin
  	rule.selected := not rule.selected;
    mustRedraw := true; // some sort of bug in updating occasionally otherwise
    end
  // just click
  else
    begin
    if not rule.selected then
      begin
      mustRedraw := domain.world.deselectAllExcept(rule);
      rule.selected := true;
      lastSingleRuleIndex := index;
      end
    else
      begin
      // do nothing except maybe drag...
      end;
    end;
  if rule.selected and (self.rule <> rule) and not (ssCtrl in Shift) and not (ssShift in shift) then
    begin
  	self.editRule(rule);
    mustRedraw := true;
    end;
  if mustRedraw then
   	RuleGrid.invalidate;
	end;

procedure TRuleEditorForm.ListPagesChange(Sender: TObject);
	begin
  if ListPages.ActivePage = TabSheetTable then
    begin
    if (rule = nil) or not rule.selected then
      {domain.world.deselectAllExcept(nil) pdf change - maybe contexts selected in map}
    else
      begin
  		self.scrollGridSelectionsIntoView(kFromTop);
      RuleGrid.invalidate;
      end;
    end     
  else if ListPages.ActivePage = TabSheetMap then
    begin
    self.MapPaintBoxChanged;
    self.scrollMapSelectionIntoView;
    end
  else if ListPages.ActivePage = TabSheetBrowse then
    self.setOrganizeByField(organizeByField);
	end;

procedure TRuleEditorForm.EditDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
	begin
	//
	end;

procedure TRuleEditorForm.TabSheetMapDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
	begin
	//
	end;

procedure TRuleEditorForm.TabSheetMapDragDrop(Sender, Source: TObject; X,
  Y: Integer);
	begin
	//
	end;

procedure TRuleEditorForm.scrollGridSelectionsIntoView(direction: boolean);
  var
  	rule: TSRule;
    firstSelectedRuleIndex: integer;
    i: integer;
  begin
  firstSelectedRuleIndex := -1;
  if direction = kFromBottom then
    for i := domain.world.rules.count - 1 downto 0 do
      begin
      rule := TSRule(domain.world.rules[i]);
      if not rule.selected then continue;
      firstSelectedRuleIndex := i;
      break;
      end
  else
    for i := 0 to domain.world.rules.count - 1 do
      begin
      rule := TSRule(domain.world.rules[i]);
      if not rule.selected then continue;
      firstSelectedRuleIndex := i;
      break;
      end;
  if firstSelectedRuleIndex = -1 then exit;
  inc(firstSelectedRuleIndex); // to account for header
  if (self.ruleGrid.TopRow <= firstSelectedRuleIndex) and
  	(self.ruleGrid.TopRow + self.ruleGrid.visibleRowCount > firstSelectedRuleIndex) then exit;
  if direction = kFromBottom then
    self.ruleGrid.TopRow := LocalIntMax(1, firstSelectedRuleIndex - self.ruleGrid.visibleRowCount + 1)
  else
    self.ruleGrid.TopRow := LocalIntMax(1, firstSelectedRuleIndex);
  end;

// ----------------------------------------------------------------------- @Map
function TRuleEditorForm.currentGraphView: TSMapView;
  begin
  result := Domain.mapView;
  end;

procedure TRuleEditorForm.mapChangedNotification(command: KfCommand; state: KfCommandChangeType);
  begin
  self.MapPaintBoxChanged;
  end;

procedure TRuleEditorForm.MapPaintBoxChanged;
  var
  	mapView: TSMapView;
    displayOptions: TSVariableDisplayOptions;
    i: integer;
	begin
  if ListPages.ActivePage <> TabSheetMap then exit;
  mapView := self.currentGraphView;
  if mapView = nil then exit;                      
  for i := 0 to 5 do
  	displayOptions[i] := false;
  displayOptions[kRuleContext] := true;
  displayOptions[kRuleMove] := true;
  displayOptions[kRuleCommand] := MenuMapsShowCommands.checked;
  MapImage.picture.bitmap.Canvas.brush.color := clWhite; // clBtnFace;
  MapImage.picture.bitmap.Canvas.FillRect(Rect(0,0, MapImage.picture.bitmap.width, MapImage.picture.bitmap.height));
  mapView.scroll := Point(-MapScrollBarHorizontal.position, -MapScrollBarVertical.position);
  mapView.displayOn(MapImage.picture.bitmap.Canvas, displayOptions, lastChoice, previousChoice);
  MapImage.invalidate;
 	end;

procedure TRuleEditorForm.ListPagesDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
	begin
	//
	end;

procedure TRuleEditorForm.MapListChange(Sender: TObject);
	begin
  lastChoice := nil;
  previousChoice := nil;
  self.MapPaintBoxChanged;
  self.adjustScrollBars;
	end;

function TRuleEditorForm.lastChoiceText: string;
  begin
  result := '';
  if lastChoice = nil then exit;
  if lastChoice is TSVariable then
    begin
    result := lastChoice.displayName;
    exit;
    end;
  result := (lastChoice as TSRule).command.phrase;
  end;

function TRuleEditorForm.makeChoice(choice: TSDraggableObject; multiSelect: boolean): boolean;
  begin
  result := false; {whether must redraw}
  if multiSelect then
    begin
  	if choice <> nil then
    	choice.selected := not choice.selected;
    result := true;
    end
  else
    begin
    if (choice = nil) or not choice.selected then
      begin
      result := domain.world.deselectAllExcept(choice);
  		if choice <> nil then
      	choice.selected := true;
      end
    else
      begin
      // do nothing except maybe drag...
      end;
    end;
  if lastChoice = choice then exit;
  result := true;
  if (choice <> nil) and choice.selected then
    begin
  	previousChoice := lastChoice;
  	lastChoice := choice;
  	if previousChoice is TSRule then
    	previousChoice := choice;
  	if lastChoice is TSRule then
    	previousChoice := choice;
    end
  else if (choice <> nil) and not choice.selected then
    begin
    if previousChoice = choice then previousChoice := nil;
    if lastChoice = choice then lastChoice := nil;
    end;
  end;

procedure TRuleEditorForm.XorRect(canvas: TCanvas; rect: TRect);
  var
  	oldMode: TPenMode;
  begin
  oldMode := canvas.Pen.mode;
  canvas.Brush.color := clNone;
  canvas.Pen.mode := pmXor;
  canvas.Pen.color := clWhite;
  canvas.Pen.style := psDot;
  //canvas.Pen.width := 2;
  with rect do
  	canvas.rectangle(left, top, right, bottom);
  //canvas.Pen.width := 1;
  canvas.Pen.mode := oldMode;
  canvas.Pen.style := psSolid;
  end;

procedure TRuleEditorForm.MapImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    newCommand:  TSMapDragCommand;
    draggedNode: TSDraggableObject;
  	mapView: TSMapView;
    displayOptions: TSVariableDisplayOptions;
    i: integer;
    multipleSelect: boolean;
    showString: string;
    textSize, centerPosition: TPoint;
	begin
  if Application.terminated then exit;
  self.commitChangesToRule;
  lastMapMouseDownPosition := Point(x + MapScrollBarHorizontal.position, y + MapScrollBarVertical.position);
  mapView := self.currentGraphView;
  if mapView = nil then exit;
  self.FocusControl(PanelMap);
  for i := 0 to 5 do
  	displayOptions[i] := false;
  displayOptions[kRuleContext] := true;
  displayOptions[kRuleMove] := true;
  displayOptions[kRuleCommand] := MenuMapsShowCommands.checked;
	draggedNode := mapView.nearestNode(Point(x + MapScrollBarHorizontal.position, y + MapScrollBarVertical.position), displayOptions);
  if button = mbRight then
    begin
    if draggedNode <> nil then
      begin
      showString := draggedNode.displayName;
      centerPosition := Point(draggedNode.center.x - MapScrollBarHorizontal.position, draggedNode.center.y - MapScrollBarVertical.position);
      end
    else
      begin
      showString := 'new item';
      centerPosition := Point(x, y);
      end;
    mapImage.canvas.brush.color := clAqua;
    mapImage.canvas.pen.style := psSolid;
    mapImage.canvas.font.style := [fsBold];
    textSize := Point(mapImage.canvas.textWidth(showString), mapImage.canvas.textHeight('W'));
    mapImage.canvas.rectangle(centerPosition.x - textSize.x div 2 - 2, centerPosition.y - textSize.y div 2 - 2,
        centerPosition.x + textSize.x div 2 + 2, centerPosition.y + textSize.y div 2 + 2);
    mapImage.canvas.textOut(centerPosition.x - textSize.x div 2,
      centerPosition.y - textSize.y div 2, showString);
    mapImage.canvas.font.style := [];
    end;
  if Button <> mbLeft then exit;
  multipleSelect := (ssShift in shift);
  mapSelectionInProgress := false;
  if draggedNode = nil then
    begin
    self.makeChoice(nil, multipleSelect);
    mapSelectionRect := Rect(x, y, x, y);
    XorRect(MapImage.canvas, mapSelectionRect);
  	mapSelectionInProgress := true;
    exit;
    end;
  //MapPaintBoxChanged;
  self.makeChoice(draggedNode, multipleSelect);
  if (ssCtrl in Shift)  then
  	begin
  	MapPaintBoxChanged;
  	MapImage.BeginDrag(true);
    exit;      
    end;
  if not multipleSelect then
  	self.MapImageDblClick(Sender);
  MapPaintBoxChanged;
  newCommand := TSMapDragCommand.create; // finds selected nodes in domain
  newCommand.notifyProcedure := self.mapChangedNotification;
  actionInProgress := domain.worldCommandList.mouseDown(newCommand, Point(x, y));
	end;

procedure TRuleEditorForm.MapImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
	begin
  if actionInProgress then
  	domain.worldCommandList.mouseMove(Point(x, y))
  else if mapSelectionInProgress then
    begin
    XorRect(MapImage.canvas, mapSelectionRect);
    mapSelectionRect.right := x;
    mapSelectionRect.bottom := y;
    XorRect(MapImage.canvas, mapSelectionRect);
    end;
	end;

procedure TRuleEditorForm.MapImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
	begin
  if actionInProgress then
    begin
  	domain.worldCommandList.mouseUp(Point(x, y));
    actionInProgress := false;
  	self.adjustScrollBars;
		end
  else if mapSelectionInProgress then
    begin
    XorRect(MapImage.canvas, mapSelectionRect);
    mapSelectionInProgress := false;
    if not (ssCtrl in Shift) then
      domain.world.deselectAllExcept(nil);
    with mapSelectionRect do
    	mapSelectionRect := Rect(left + MapScrollBarHorizontal.position, top + MapScrollBarVertical.position,
          right + MapScrollBarHorizontal.position, bottom  + MapScrollBarVertical.position);
    domain.world.selectInRectangle(mapSelectionRect);
   	MapPaintBoxChanged;
    end;
	end;

procedure TRuleEditorForm.MapImageDblClick(Sender: TObject);
  //var
    //row: integer;
    //count: integer;
    //rule: TSRule;
    {ruleIndex: integer; }
	begin
  if lastChoice = nil then exit;
  if lastChoice is TSRule then
  	begin
    self.editRule(lastChoice as TSRule);
    end;
    (*
  else
    begin
    count := 1;
    ruleIndex := domain.world.rules.indexOf(self.rule);
    while (count <= domain.world.rules.count) do
      begin
      row := (count + ruleIndex) mod domain.world.rules.count;
      rule := domain.world.rules[row];
      // unfinished - need to check requirements & changes
      if (rule.context.phrase = self.lastChoiceText) or
      		(rule.command.phrase = self.lastChoiceText) or
          (rule.move.phrase = self.lastChoiceText) then
        begin
      	self.editRule(rule);
        exit;
        end;
      inc(count);
      end;
    end;
    *)
	end;

procedure TRuleEditorForm.searchForAndSelectRule(aText: string; ignoreCase: boolean; goDown: boolean);
  var
    row: integer;
    count: integer;
    rule: TSRule;
    ruleIndex: integer;
    match: boolean;
    matchText: string;
  begin
  count := 1;
  ruleIndex := domain.world.rules.indexOf(self.rule);
  if ignoreCase then
     matchText := lowercase(aText)
  else
  	matchText := aText;
  while (count <= domain.world.rules.count) do
    begin
    if goDown then
    	row := (ruleIndex + count) mod domain.world.rules.count
    else
    	row := ((domain.world.rules.count * 2) + (ruleIndex - count)) mod domain.world.rules.count;
    rule := domain.world.rules[row];
    // unfinished - need to check requirements & changes
    if ignoreCase then
      match :=
    	(pos(matchText, lowercase(rule.context.phrase)) > 0) or
      (pos(matchText, lowercase(rule.command.phrase)) > 0) or
      (pos(matchText, lowercase(rule.reply)) > 0) or
      (pos(matchText, lowercase(rule.move.phrase)) > 0) or
      (pos(matchText, lowercase(rule.requirementsString)) > 0) or
      (pos(matchText, lowercase(rule.changesString)) > 0)
   else
     match :=
    	(pos(matchText, rule.context.phrase) > 0) or
      (pos(matchText, rule.command.phrase) > 0) or
      (pos(matchText, rule.reply) > 0) or
      (pos(matchText, rule.move.phrase) > 0) or
      (pos(matchText, rule.requirementsString) > 0) or
      (pos(matchText, rule.changesString) > 0);
   if match then
   		begin
      domain.world.deselectAllExcept(rule);
      self.editRule(rule);
  		self.updateForRuleChange;
      rule.selected := true;
  		self.scrollGridSelectionsIntoView(kFromBottom);
  		self.MapPaintBoxChanged;
  		self.scrollMapSelectionIntoView;
      exit;
      end;
    inc(count);
    end;
   ShowMessage('Search string "' + aText + '" not found.');
   end;

const kTopBottomBorderSize = 120; // accomodates growth
const kLeftRightBorderSize = 120; // accomodates growth

procedure TRuleEditorForm.adjustScrollBars;
  var
    top, bottom, left, right: integer;
  	graphView: TSMapView;
    xPosition, yPosition: integer;
    mapBoundsRect: TRect;
	begin
  graphView := self.currentGraphView;
  if graphView <> nil then
    begin
    mapBoundsRect := domain.world.boundsRect;
    top := mapBoundsRect.top;
    left := mapBoundsRect.left;
    bottom := mapBoundsRect.bottom;
    right := mapBoundsRect.right;
    if bottom < MapImage.height then
    	bottom := MapImage.height;
    if right < MapImage.width then
    	right := MapImage.width;
    end
  else
    begin
    top := 0;
  	bottom := MapImage.height;
  	left := 0;
  	right := MapImage.width;
		end;
  left := left - kLeftRightBorderSize;
  right := right + kLeftRightBorderSize;
  top := top - kTopBottomBorderSize;
  bottom := bottom + kTopBottomBorderSize;
  
  right := right - MapImage.width;
  bottom := bottom - MapImage.height;
  xPosition := MapScrollBarHorizontal.position;
  yPosition := MapScrollBarVertical.position;
  {if xPosition < left then xPosition := left;
  if xPosition > right then xPosition := right;
  if yPosition < top then yPosition := top;
  if yPosition > bottom then yPosition := bottom;}
  if xPosition < left then left := xPosition;
  if xPosition > right then right := xPosition;
  if yPosition < top then top := yPosition;
  if yPosition > bottom then bottom := yPosition;
  MapScrollBarHorizontal.SetParams(xPosition, left, right);
	MapScrollBarHorizontal.largeChange := MapImage.width;
  //LocalIntMax(1, LocalIntMin((right - left) div 10, MapPaintBox.width));
  MapScrollBarVertical.SetParams(yPosition, top, bottom);
	MapScrollBarVertical.largeChange := MapImage.height;
  //LocalIntMax(1, LocalIntMin((bottom - top) div 10, MapPaintBox.height));
  end;

procedure TRuleEditorForm.MapScrollBarVerticalChange(Sender: TObject);
	begin
	MapPaintBoxChanged;
	end;

procedure TRuleEditorForm.MapScrollBarHorizontalChange(Sender: TObject);
	begin
	MapPaintBoxChanged;
	end;
  
function TRuleEditorForm.goodPosition: TPoint;
  {var
  	mapBoundsRect: TRect;
    selection: TSDraggableObject; }
  begin
  if lastChoice <> nil then
    begin
    if previousChoice <> nil then
    	result := Point((previousChoice.position.x + lastChoice.position.x) div 2, (previousChoice.position.y + lastChoice.position.y) div 2 + 30)
    else
    	result := Point(lastChoice.position.x, lastChoice.position.y + 30);
    end
  else
    begin
   { mapBoundsRect := domain.world.boundsRect;
    result.x := (mapBoundsRect.left - mapBoundsRect.right) div 2;
    result.y := mapBoundsRect.bottom + 30;  }
   	result := Point(MapScrollBarHorizontal.position + MapImage.width div 2, MapScrollBarVertical.position +  MapImage.height div 2);
    end;
  result.x := result.x + random(200) - 100;
  result.y := result.y + random(200) - 100;
  (*if (domain <> nil) and (domain.world <> nil) then
    begin
    selection := domain.world.firstSelectedObject;
    if selection <> nil then
      begin
      result.x := selection.position.x;
      result.y := selection.position.y + 30;
      end;
    end;
  result := Point(MapScrollBarHorizontal.position + MapImage.width div 2, MapScrollBarVertical.position +  MapImage.height div 2);
  //result.x := result.x + random(200) - 100;
  //result.y := result.y + random(200) - 100;*)
  (*result := Point(MapScrollBarHorizontal.position + MapImage.width div 2, MapScrollBarVertical.position +  MapImage.height div 2);
  if (domain <> nil) and (domain.world <> nil) then
    begin
    selection := domain.world.firstSelectedObject;
    if selection <> nil then
      begin
      result.x := selection.position.x;
      result.y := selection.position.y;
      end;
    end;
  result.x := result.x + random(200) - 100;
  result.y := result.y + random(200) - 100;   *)
  end;

// ------------------------------------------------------------------- @Browser
procedure TRuleEditorForm.loadFirstListBox;
  var
  	i: integer;
  	variable: TSVariable;
  begin
  FirstListBox.clear;
  with firstListBoxLabel do
    begin
  	caption := 'All ' + lowerCase(TSRule.headerForField(organizeByField));
  	if caption[length(caption)] <> 's' then
      caption := caption + 's';
    end;
  for i := 0 to domain.world.variables.count - 1 do
    begin
    variable := domain.world.variables[i];
    if variable.hasUseagesForField(organizeByField) then
      begin
      FirstListBox.items.addObject(variable.phrase, variable);
  		end;
    end;
  end;

procedure TRuleEditorForm.loadSecondListBox;
  var
  	i: integer;
  	variable: TSVariable;
    rule: TSRule;
    displayFieldType: integer;
    selectedItemString: string;
  begin
  SecondListBox.clear;
  if organizeByField = kRuleCommand then
    begin
    displayFieldType := kRuleContext;
    SecondListBoxImage.picture.bitmap := ContextSpeedButton.glyph;
    end
  else
    begin
    displayFieldType := kRuleCommand;
    SecondListBoxImage.picture.bitmap := CommandSpeedButton.glyph;
    end;
  selectedItemString := lowerCase(TSRule.headerForField(organizeByField));
  // remove plural 's' for singular use
  if selectedItemString[length(selectedItemString)] = 's' then
    selectedItemString := copy(selectedItemString, 1, length(selectedItemString) - 1);
  secondListBoxLabel.caption := TSRule.headerForField(displayFieldType)
  	+ 's with selected ' + selectedItemString;
  if FirstListBox.itemIndex < 0 then exit;
  variable := TObject(FirstListBox.items.objects[FirstListBox.itemIndex]) as TSVariable;
  for i := 0 to domain.world.rules.count - 1 do
    begin
    rule := domain.world.rules[i];
    if rule.usesVariableFor(variable, organizeByField) then
      SecondListBox.items.addObject(rule.variableForField(displayFieldType).phrase, rule)
    end;
  end;

procedure TRuleEditorForm.FirstListBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
  var
  	i: integer;
    focused: boolean;
  	rule: TSRule;
  	variable: TSVariable;
	begin
  if (index < 0) or (index > FirstListBox.items.count - 1) then exit;
  focused := false;
  if odSelected in state then
  	for i := 0 to SecondListBox.items.count - 1 do
    	begin
  		rule := TObject(SecondListBox.items.objects[i]) as TSRule;
   		focused := (rule = self.rule) and rule.selected;
    	if focused then break;
    	end;
  variable := TObject(FirstListBox.items.objects[index]) as TSVariable;
  if variable = nil then exit;
  self.drawBrowserListBoxItem(FirstListBox, variable.phrase, index, rect, odSelected in state, focused);
	end;

procedure TRuleEditorForm.SecondListBoxDrawItem(Control: TWinControl;
    Index: Integer; Rect: TRect; State: TOwnerDrawState);
  var
  	selected, focused: boolean;
  	rule: TSRule;
    displayFieldType: integer;
    displayString: string;
	begin
  if (index < 0) or (index > SecondListBox.items.count - 1) then exit;
  rule := TObject(SecondListBox.items.objects[index]) as TSRule;
  selected := rule.selected;
  focused := rule = self.rule;
  if organizeByField = kRuleCommand then
    displayFieldType := kRuleContext
  else
    displayFieldType := kRuleCommand;
  displayString := rule.variableForField(displayFieldType).phrase;
  self.drawBrowserListBoxItem(SecondListBox, displayString, index, rect, selected, focused);
	end;

procedure TRuleEditorForm.drawBrowserListBoxItem(Control: TWinControl;
    displayString: string; Index: Integer; Rect: TRect; selected, focused: boolean);
  var
    listBox: TListBox;
    //cText: array[0..255] of Char;
  begin
  if Application.terminated then exit;
  listBox := control as TListBox;
  if listBox = nil then exit;
  if (index < 0) or (index > listBox.items.count - 1) then exit;
  setCanvasColorsForSelection(listBox.canvas, selected, focused, false);
  with listBox.canvas do
    begin
    fillRect(rect);
    //strPCopy(cText, displayString);
    rect.left := rect.left + 2; { margin for text }
    DrawText(handle, PChar(displayString), length(displayString), rect, DT_LEFT);
    end;
  end;

procedure TRuleEditorForm.FirstListBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
	begin
  FirstListBox.invalidate;
  self.loadSecondListBox;
	end;

procedure TRuleEditorForm.SecondListBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
  	rule, shiftRule: TSRule;
    index, i: integer;
	begin
	if SecondListBox.itemIndex < 0 then exit;
  index := SecondListBox.itemAtPos(Point(x, y), true);
  if index < 0 then
    begin
    domain.world.deselectAllExcept(nil);
  	FirstListBox.invalidate;
  	SecondListBox.invalidate;
    exit;
    end;
  rule := TObject(SecondListBox.items.objects[SecondListBox.itemIndex]) as TSRule;
  if rule = nil then exit;
  // shift
  if (ssShift in shift) then
    begin
    if (lastBrowserSingleRuleIndex >= 0) and (lastBrowserSingleRuleIndex <= SecondListBox.items.count - 1)
      and (lastSingleRuleIndex <> index) then
        begin
        domain.world.deselectAllExcept(rule);
        if lastBrowserSingleRuleIndex < index then
          for i := lastBrowserSingleRuleIndex to index do
            begin
            shiftRule := TObject(SecondListBox.items.objects[i]) as TSRule;
            shiftRule.selected := true;
            end
        else if lastBrowserSingleRuleIndex > index then
          for i := lastBrowserSingleRuleIndex downto index do
            begin
            shiftRule := TObject(SecondListBox.items.objects[i]) as TSRule;
            shiftRule.selected := true;
            end;
        end;
    end
  // control
  else if (ssCtrl in Shift) then
  	rule.selected := not rule.selected
  // just click
  else
    begin
    if not rule.selected then
      begin
      domain.world.deselectAllExcept(rule);
      rule.selected := true;
      lastBrowserSingleRuleIndex := index;
      end
    else
      begin
      // do nothing except maybe drag...
      end;
    end;
  if rule.selected and (self.rule <> rule) and not (ssCtrl in Shift) and not (ssShift in shift) then
  	self.editRule(rule);
  FirstListBox.invalidate;
  SecondListBox.invalidate;
	end;

procedure TRuleEditorForm.setOrganizeByField(newValue: integer);
  var variable: TSVariable;
	begin
  if (newValue < 0) or (newValue > kLastRuleField) then exit;
  domain.options.browseBy := newValue;
  MenuBrowseByContext.checked := newValue = kRuleContext;
  MenuBrowseByCommand.checked := newValue = kRuleCommand;
  MenuBrowseByMove.checked := newValue = kRuleMove;
  MenuBrowseByRequirements.checked := newValue = kRuleRequirements;
  MenuBrowseByChanges.checked := newValue = kRuleChanges;
  if newValue = kRuleContext then
    FirstListBoxImage.picture.bitmap := ConsoleForm.ContextButton.glyph;
  if newValue = kRuleCommand then
    FirstListBoxImage.picture.bitmap := ConsoleForm.CommandButton.glyph;
  if newValue = kRuleMove then
    FirstListBoxImage.picture.bitmap := ConsoleForm.MoveButton.glyph;
  if newValue = kRuleRequirements then
    FirstListBoxImage.picture.bitmap := ConsoleForm.RequirementsButton.glyph;
  if newValue = kRuleChanges then
    FirstListBoxImage.picture.bitmap := ConsoleForm.ChangesButton.glyph;
	// if organizeByField <> newValue then
  	begin
  	organizeByField := newValue;
		self.loadFirstListBox;
  	if rule <> nil then
      begin
      variable := rule.variableForFieldWithSelections(organizeByField, requirementsListBox.itemIndex, changesListBox.itemIndex);
  		firstListBox.itemIndex := firstListBox.Items.IndexOfObject(variable);
 			end;
		self.loadSecondListBox;
  	secondListBox.itemIndex := secondListBox.Items.IndexOfObject(rule);
  	end;
	end;

procedure TRuleEditorForm.firstListBoxImageClick(Sender: TObject);
	begin
  case organizeByField of
    kRuleContext: setOrganizeByField(kRuleCommand);
    kRuleCommand: setOrganizeByField(kRuleMove);
    kRuleMove:    setOrganizeByField(kRuleRequirements);
    kRuleRequirements: setOrganizeByField(kRuleChanges);
    kRuleChanges:      setOrganizeByField(kRuleContext);
    end;
	end;

procedure TRuleEditorForm.fillListBox(listBox: TListBox; list: TList);
  var
    i: integer;
    wrapper: TSDesiredStateVariableWrapper; // OK for requirements because is parent class
	begin
  listBox.Items.clear;
  if rule <> nil then
    begin
    for i := 0 to list.count - 1 do
      begin
      wrapper := list[i];
      listBox.items.addObject(wrapper.displayString, wrapper);
      end;
  	listBox.Items.add('');
    end;
  end;

function TRuleEditorForm.logicalStatementForListBox(listBox: TListBox): string;
  var
    i: integer;
  begin
  result := '';
  for i := 0 to listBox.items.count - 2 do   // use 2 because last is always blank for adding
    if result <> '' then
      result := result + ' & ' +  Trim(listBox.items[i])
    else
      result := Trim(listBox.items[i]);
  end;

procedure TRuleEditorForm.SpeedButtonClick(Sender: TObject);
	begin
  self.commitChangesToRule;
  self.switchToPage(TabSheetBrowse);
  self.setOrganizeByField((sender as TComponent).tag);
	end;

// ------------------------------------------------------------------ @Resizing
const
	kGap = 4;
  kGap2 = kGap * 2;
  kGap3 = kGap * 3;

procedure TRuleEditorForm.FormResize(Sender: TObject);
	// var proposedSize: integer;
	begin
  {PanelEditor.show; }
  if PanelEditor.height <> 1 then
    begin
    PanelEditor.enabled := true;
  	if PanelEditor.height <	kMinRestHeight + kMinRequirementsChangesHeight + kSplitterHeight + kSplitterHeight then
    	{PanelTop.height := self.clientHeight - kMinRestHeight - kMinRequirementsChangesHeight - kSplitterHeight - kSplitterHeight;}
    	begin
    	PanelEditor.top := self.clientHeight - kMinRestHeight - kMinRequirementsChangesHeight - kSplitterHeight;
    	PanelEditor.height := kMinRestHeight + kMinRequirementsChangesHeight + kSplitterHeight;
    	end;
  	if PanelEditor.height >	self.clientHeight - kMinListHeight - kSplitterHeight then
      begin
      PanelEditor.top := kMinListHeight + kSplitterHeight;
      PanelEditor.height := self.clientHeight - kMinListHeight - kSplitterHeight;
      end;
 // 	if PanelRest.height < kMinRestHeight then
  	if PanelRequirementsChanges.height > PanelEditor.height - kMinRestHeight - kSplitterHeight then
    	begin
    	PanelRequirementsChanges.height := PanelEditor.height - kMinRestHeight - kSplitterHeight;
    	end;
    end;
  if PanelLists.width - PanelFirstList.width < SplitterLists.minSize then
  	PanelFirstList.width := self.clientWidth - SplitterLists.minSize;
  {if PanelRequirementsChanges.height < SplitterRequirementsChanges.MinSize then
		begin
    PanelRequirementsChanges.height := SplitterRequirementsChanges.MinSize;
  	proposedSize := RuleEditorForm.clientHeight - SplitterRequirementsChanges.MinSize - PanelEditor.top - SplitterRequirementsChanges.height;
  	if proposedSize < 200 then
  		proposedSize := 200;
  	PanelRest.height := proposedSize;
  	end;}
	end;

procedure TRuleEditorForm.WMGetMinMaxInfo(var MSG: Tmessage);
  begin
  inherited;
  with PMinMaxInfo(MSG.lparam)^ do
    begin
    ptMinTrackSize.x := 300;
    ptMinTrackSize.y := 100 + kMinListHeight + kMinRestHeight + kMinRequirementsChangesHeight + kSplitterHeight + kSplitterHeight;
    end;
  end;

procedure TRuleEditorForm.PanelFirstListResize(Sender: TObject);
	begin
	with Sender as TPanel do
  	begin
		FirstListBox.width := width - kGap2;
  	FirstListBox.height := height - kGap2 - FirstListBox.top;
  	end;
	end;

procedure TRuleEditorForm.PanelSecondListResize(Sender: TObject);
	begin
	with Sender as TPanel do
  	begin
		SecondListBox.width := width - kGap2;
  	SecondListBox.height := height - kGap2 - SecondListBox.top;
  	end;
	end;

procedure TRuleEditorForm.PanelMapResize(Sender: TObject);
	begin
	MapImage.top := 0;
	MapImage.left := 0;
	MapImage.width := PanelMap.clientWidth - MapScrollBarVertical.width;
	MapImage.height := PanelMap.clientHeight - MapScrollBarHorizontal.height;
	MapScrollBarHorizontal.left := 0;
	MapScrollBarHorizontal.top := MapImage.height;
	MapScrollBarHorizontal.width := MapImage.width;
	MapScrollBarVertical.top := 0;
	MapScrollBarVertical.height := MapImage.height;
	MapScrollBarVertical.left := MapImage.width;
  // could fail - not sure what to do about it...
  if MapImage.picture.bitmap <> nil then
    begin
  	MapImage.picture.bitmap.width := MapImage.width;
 		MapImage.picture.bitmap.height := MapImage.height;
    end;
  self.adjustScrollBars;
	self.MapPaintBoxChanged;
	end;

procedure TRuleEditorForm.PanelRestResize(Sender: TObject);
	begin
  //if PanelRest.height < kMinRestHeight then
  //	PanelRest.height := kMinRestHeight;
	with Sender as TPanel do
  	begin
  	MoveSpeedButton.top := height - MoveSpeedButton.height - kGap;
  	MoveEdit.top := MoveSpeedButton.top;
    ReplyMemo.height := MoveEdit.top - ReplyMemo.top - kGap;
    ContextEdit.width := width - ContextEdit.left - kGap;
    CommandEdit.width := ContextEdit.width;
    ReplyMemo.width := ContextEdit.width;
    MoveEdit.width := ContextEdit.width;
    end;
	end;

procedure TRuleEditorForm.PanelRequirementsChangesResize(Sender: TObject);
	begin
  self.commitChangesToRule;  // just in case fooling with popup editor window...
	with Sender as TPanel do
  	begin
    RequirementsListBox.height := (height - kGap3) div 2;
    RequirementsListBox.width := width - RequirementsListBox.left - kGap;
    ChangesListBox.top := RequirementsListBox.top + RequirementsListBox.height + kGap;
    ChangesSpeedButton.top := ChangesListBox.top;
    ChangesListBox.height := RequirementsListBox.height;
    ChangesListBox.width := RequirementsListBox.width;
    end;
	end;

procedure TRuleEditorForm.PanelListsResize(Sender: TObject);
	begin
	//if PanelLists.height < kMinListHeight then
  //  PanelLists.height := kMinListHeight;
	end;

procedure TRuleEditorForm.SplitterRequirementsChangesMoved(Sender: TObject);
	begin
  self.FormResize(sender);
	end;

procedure TRuleEditorForm.SplitterEditMoved(Sender: TObject);
	begin
  if PanelEditor.visible and (PanelEditor.height <=	50) then
    begin
    //PanelEditor.hide;
    PanelTop.height := self.clientHeight - kSplitterHeight - 1;
    PanelEditor.height := 1;
    PanelEditor.enabled := false;
    MenuOptionsShowRuleEditor.checked := false;
		domain.options.showRuleEditor := false;
    end
  else
    begin
    {if not PanelEditor.visible then
      begin
    	PanelEditor.show;
    	{PanelTop.height := self.clientHeight - 200;
      end; }
    MenuOptionsShowRuleEditor.checked := true;
		domain.options.showRuleEditor := true;
    if PanelEditor.height = 1 then
    	PanelEditor.height := 2;
    PanelEditor.enabled := true;
  	self.FormResize(sender);
    end;
  domain.options.editorPanelEditorHeight := PanelEditor.height;
	end;

procedure TRuleEditorForm.MenuFileExportClick(Sender: TObject);
  var
    fileInfo: SaveFileNamesStructure;
	begin
  self.commitChangesToRule;
  lastSaveProceeded := getFileSaveInfo(kFileTypeWorld, kAskForFileName, 'export' + '.' + kWorldExtension, fileInfo);
  if not lastSaveProceeded then exit;
  try
    startFileSave(fileInfo);
    Domain.world.saveWorldToFile(fileInfo.tempFile, kSaveOnlySelectedRules);
    fileInfo.writingWasSuccessful := true;
  finally
    lastSaveProceeded := cleanUpAfterFileSave(fileInfo);
  end;
	end;

procedure TRuleEditorForm.scrollMapSelectionIntoView;
  var
  	intersection, visibleRect: TRect;
    rule: TSRule;
    variable: TSVariable;
    i: integer;
    upperLeftObject: TSDraggableObject;
    firstContextVariable: TSVariable;
  begin
  upperLeftObject := nil;
  visibleRect.left := MapScrollBarHorizontal.position;
  visibleRect.top := MapScrollBarVertical.position;
  visibleRect.right := visibleRect.left + MapImage.width;
  visibleRect.bottom := visibleRect.top + MapImage.height;
  for i := 0 to domain.world.rules.count - 1 do
    begin
    rule := TSRule(domain.world.rules[i]);
    if rule.selected then
      begin
    	IntersectRect(intersection, rule.bounds, visibleRect);
    	if not IsRectEmpty(intersection) then exit;
      if upperLeftObject = nil then
      	upperLeftObject := rule
      else if upperLeftObject.bounds.top > rule.bounds.top then
         upperLeftObject := rule
      else if upperLeftObject.bounds.left > rule.bounds.left then
         upperLeftObject := rule;
      end;
    end;
  firstContextVariable := nil;
  for i := 0 to domain.world.variables.count - 1 do
    begin
    variable := TSVariable(domain.world.variables[i]);
    if (firstContextVariable = nil) and (variable.hasUseagesForField(kRuleContext)) then
      firstContextVariable := variable;
    if variable.selected then
      begin
    	IntersectRect(intersection, variable.bounds, visibleRect);
    	if not IsRectEmpty(intersection) then exit;
      if upperLeftObject = nil then
      	upperLeftObject := variable
      else if upperLeftObject.bounds.top > variable.bounds.top then
         upperLeftObject := variable
      else if upperLeftObject.bounds.left > variable.bounds.left then
         upperLeftObject := variable;
      end;
    end;
  if upperLeftObject = nil then
     upperLeftObject := firstContextVariable;
  if upperLeftObject = nil then exit;
  MapScrollBarHorizontal.position :=
  		localIntMin(localIntMax(upperLeftObject.center.x - MapImage.width div 2,
    	MapScrollBarHorizontal.min), MapScrollBarHorizontal.max);
  MapScrollBarVertical.position :=
  		localIntMin(localIntMax(upperLeftObject.center.y + - MapImage.height div 2,
    	MapScrollBarVertical.min), MapScrollBarVertical.max);
  end;

procedure TRuleEditorForm.MenuEditInsertSoundClick(Sender: TObject);
  var
  	fileNameWithPath, shortFileName: string;
	begin
  self.commitChangesToRule;
  if self.ActiveControl <> ReplyMemo then
    begin
    ShowMessage('The reply field must be selected to insert a sound file.');
    exit;
    end;
  fileNameWithPath := getFileOpenInfo(kFileTypeSound, kNoSuggestedFile, 'Choose a sound file', kOtherExtOk);
  if fileNameWithPath = '' then exit;
  shortFileName := ExtractFileName(fileNameWithPath);
  if pos('.WAV', upperCase(shortFileName)) = length(shortFileName) - 3 then
    shortFileName := copy(shortFileName, 1, length(shortFileName) - 4);
  ReplyMemo.selText := ' {' + kPlaySoundMacroStart + shortFileName + '} ';
	end;

procedure TRuleEditorForm.insertSoundClick(Sender: TObject);
	begin
  self.MenuEditInsertSoundClick(self);
	end;

procedure TRuleEditorForm.InsertMusicButtonClick(Sender: TObject);
	begin
  self.MenuEditInsertMusicClick(self);
	end;

procedure TRuleEditorForm.MenuEditInsertMusicClick(Sender: TObject);
  var
  	fileNameWithPath, shortFileName: string;
	begin
  self.commitChangesToRule;
  if self.ActiveControl <> ReplyMemo then
    begin
    ShowMessage('The reply field must be selected to insert a music file.');
    exit;
    end;
  fileNameWithPath := getFileOpenInfo(kFileTypeMusic, kNoSuggestedFile, 'Choose a music file', kOtherExtOk);
  if fileNameWithPath = '' then exit;
  shortFileName := ExtractFileName(fileNameWithPath);
  if pos('.MID', upperCase(shortFileName)) = length(shortFileName) - 3 then
    shortFileName := copy(shortFileName, 1, length(shortFileName) - 4);
  ReplyMemo.selText := ' {' + kPlayMusicMacroStart + shortFileName + '} ';
	end;

procedure TRuleEditorForm.MenuEditInsertPictureClick(Sender: TObject);
  var
  	fileNameWithPath, shortFileName: string;
	begin
  self.commitChangesToRule;
  if self.ActiveControl <> ReplyMemo then
    begin
    ShowMessage('The reply field must be selected to insert a picture file.');
    exit;
    end;
  fileNameWithPath := getFileOpenInfo(kFileTypeBitmap, kNoSuggestedFile, 'Choose a bitmap file', kOtherExtNotOK);
  if fileNameWithPath = '' then exit;
  shortFileName := ExtractFileName(fileNameWithPath);
  if pos('.BMP', upperCase(shortFileName)) = length(shortFileName) - 3 then
    shortFileName := copy(shortFileName, 1, length(shortFileName) - 4);
  ReplyMemo.selText := ' {' + kShowPictureMacroStart + shortFileName + '} ';
	end;

procedure TRuleEditorForm.MediaPlayerNotify(Sender: TObject);
	begin
  if loopMusic and (MediaPlayer.FileName <> '') and (MediaPlayer.notifyValue = nvSuccessful) then
    begin
    MediaPlayer.Notify := true;
    MediaPlayer.Play;
    end;
	end;

procedure TRuleEditorForm.MenuRuleTestReplyClick(Sender: TObject);
  var
    oldSpeak, oldPlayMusic, oldPlaySounds: boolean;
	begin
  self.commitChangesToRule;
	ConsoleForm.speechSystem.haltSpeechAndSound;
  if rule = nil then exit;
  // temporarily turn on these options to test the reply
  oldSpeak := domain.options.playerSpeak;
  oldPlaySounds := domain.options.playerPlaySounds;
  oldPlayMusic := domain.options.playerPlayMusic;
  domain.options.playerSpeak := true;
  domain.options.playerPlaySounds := true;
  domain.options.playerPlayMusic := true;
  try
  	ConsoleForm.speechSystem.sayTextWithMacros(rule.reply);
  finally
  	domain.options.playerSpeak := oldSpeak;
  	domain.options.playerPlaySounds := oldPlaySounds;
  	domain.options.playerPlayMusic := oldPlayMusic;
  end;
	end;

procedure TRuleEditorForm.replyPictureMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
	begin
  self.MenuRuleTestReplyClick(self);
	end;

procedure TRuleEditorForm.MenuEditClick(Sender: TObject);
	begin
  // keep the undo menu up to date...
  self.commitChangesToRule;
	end;

procedure TRuleEditorForm.ReplyMemoMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
	begin
  // more fine grained tracking of changes to this field...
  self.commitChangesToRule;
	end;

procedure TRuleEditorForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
	begin
  if key = VK_ESCAPE then
	  ConsoleForm.speechSystem.haltSpeechAndSound 
  else if (ssCtrl in Shift) and ((key = ord('c')) or (key = ord('C'))) then
    begin
	  self.MenuEditCopyClick(self);
    Key := 0;
    end
  else if (ssCtrl in Shift) and ((key = ord('v')) or (key = ord('V'))) then
    begin
	  self.MenuEditPasteClick(self);
    Key := 0;
    end
  else if (ssCtrl in Shift) and ((key = ord('x')) or (key = ord('X'))) then
    begin
	  self.MenuEditCutClick(self);
    Key := 0;
    end
	end;

procedure TRuleEditorForm.MenuHelpContentsClick(Sender: TObject);
	begin
  self.commitChangesToRule;
	application.helpCommand(HELP_FINDER, 0);
	end;

procedure TRuleEditorForm.MenuHelpBasicConceptsClick(Sender: TObject);
	begin
  self.commitChangesToRule;
	Application.helpJump('A_summary_based_on_definitions');
	end;

procedure TRuleEditorForm.MenuHelpTutorialClick(Sender: TObject);
	begin
  self.commitChangesToRule;
	Application.helpJump('Basic_Tutorial');
	end;

procedure TRuleEditorForm.MenuHelpEditingWorldsClick(Sender: TObject);
	begin
  self.commitChangesToRule;
	Application.helpJump('Editing_worlds');
	end;

procedure TRuleEditorForm.MenuWorldSwitchToPlayerClick(Sender: TObject);
	begin
  self.commitChangesToRule;
  ConsoleForm.show;
	end;

procedure TRuleEditorForm.FormShow(Sender: TObject);
	begin
  DragAcceptFiles(Handle, True);
	end;

procedure TRuleEditorForm.PopupNewContextClick(Sender: TObject);
  var
  	newRule: TSRule;
    newRulesCommand: TSNewRulesCommand;
	begin
  if Application.terminated then exit;
  self.commitChangesToRule;
  newRulesCommand := TSNewRulesCommand.create;
  newRule := domain.world.newRule;
  newRulesCommand.addRule(newRule);
  while domain.world.findVariable('new context ' + intToStr(numNewContextsMadeByPopupMenuThisSession)) <> nil do
    inc(numNewContextsMadeByPopupMenuThisSession);
  newRule.setContext('new context ' + intToStr(numNewContextsMadeByPopupMenuThisSession));
  newRule.setCommand('look');
  newRule.setReply('There is nothing of interest here.');

  newRule.position := Point(lastMapMouseDownPosition.x + 30, lastMapMouseDownPosition.y + 30);
  newRule.context.position := lastMapMouseDownPosition;

  domain.world.deselectAllExcept(newRule);
  newRule.selected := true;
  domain.worldCommandList.doCommand(newRulesCommand);
  self.editRule(newRule);
  self.activeControl := ContextEdit;
  ContextEdit.selStart := 0;
  ContextEdit.selLength := length(ContextEdit.text);
  MapPaintBoxChanged;
	end;

procedure TRuleEditorForm.PopupNewCommandClick(Sender: TObject);
  var
    rule, newRule: TSRule;
    newRulesCommand: TSNewRulesCommand;
    variable: TSVariable;
    i, newRuleCount: integer;
	begin
  if Application.terminated then exit;
  self.commitChangesToRule;
  while domain.world.findVariable('new command ' + intToStr(numNewCommandsMadeByPopupMenuThisSession)) <> nil do
    inc(numNewCommandsMadeByPopupMenuThisSession);
  newRule := nil;
  newRuleCount := 0;
  for i := 0 to domain.world.variables.count - 1 do
  	begin
    variable := TSVariable(domain.world.variables[i]);
    if variable.selected then
      begin
  		newRulesCommand := TSNewRulesCommand.create;
  		newRule := domain.world.newRule;
  		newRulesCommand.addRule(newRule);
      newRule.setContext(variable.phrase);
  		newRule.setCommand('new command ' + intToStr(numNewCommandsMadeByPopupMenuThisSession));
  		newRule.setReply('Nothing happens.');
      newRule.position := Point(lastMapMouseDownPosition.x, lastMapMouseDownPosition.y + 30 * newRuleCount);
      domain.worldCommandList.doCommand(newRulesCommand);
      inc(newRuleCount);
      end;
    end;
  for i := 0 to domain.world.rules.count - 1 do
  	begin
    rule := TSRule(domain.world.rules[i]);
    if rule.selected then
      begin
  		newRulesCommand := TSNewRulesCommand.create;
  		newRule := domain.world.newRule;
  		newRulesCommand.addRule(newRule);
      newRule.setContext(rule.context.phrase);
  		newRule.setCommand('new command ' + intToStr(numNewCommandsMadeByPopupMenuThisSession));
  		newRule.setReply('Nothing happens.');
      newRule.position := Point(lastMapMouseDownPosition.x, lastMapMouseDownPosition.y + 30 * newRuleCount);
      domain.worldCommandList.doCommand(newRulesCommand);
      inc(newRuleCount);
      end;
    end;
  if newRule = nil then
    begin
    messageDlg('To make a new command,' + chr(13) + 'select at least one context or command'
      + chr(13) + 'and right-click where you want to place the new command.', mtInformation, [mbOk], 0);
    MapPaintBoxChanged;
    exit;
    end;
  domain.world.deselectAllExcept(newRule);
  newRule.selected := true;
  self.editRule(newRule);
  self.activeControl := CommandEdit;
  CommandEdit.selStart := 0;
  CommandEdit.selLength := length(CommandEdit.text);
  MapPaintBoxChanged;
	end;

procedure TRuleEditorForm.PopupNewLinkClick(Sender: TObject);
  var
    draggableNode: TSDraggableObject;
    contextToMoveTo: TSVariable;
    newRulesCommand: TSNewRulesCommand;
    variable: TSVariable;
    rule, newRule: TSRule;
    mapView: TSMapView;
    displayOptions: TSVariableDisplayOptions;
    i: integer;
    atLeastOneRuleChanged: boolean;
	begin
  if Application.terminated then exit;
  self.commitChangesToRule;
  mapView := self.currentGraphView;
  if mapView = nil then exit;
  for i := 0 to 5 do
  	displayOptions[i] := false;
  displayOptions[kRuleContext] := true;
  displayOptions[kRuleMove] := true;
  displayOptions[kRuleCommand] := MenuMapsShowCommands.checked;
  draggableNode := mapView.nearestNode(lastMapMouseDownPosition, displayOptions);
  if (draggableNode = nil) or not (draggableNode is TSVariable) then
    begin
    messageDlg('To build a link,' + chr(13) + 'select at least one context or command'
      + chr(13) + 'and right-click on a context.', mtInformation, [mbOk], 0);
    MapPaintBoxChanged;
    exit;
    end;
  contextToMoveTo := draggableNode as TSVariable;
  newRule := nil;
  for i := 0 to domain.world.variables.count - 1 do
  	begin
    variable := TSVariable(domain.world.variables[i]);
    if variable.selected then
      begin
  		newRulesCommand := TSNewRulesCommand.create;
  		newRule := domain.world.newRule;
  		newRulesCommand.addRule(newRule);
      newRule.setContext(variable.phrase);
  		newRule.setCommand('move to ' + contextToMoveTo.phrase);
  		newRule.setReply('You move to ' + contextToMoveTo.phrase + '.');
      newRule.setMove(contextToMoveTo.phrase);
      newRule.position.x := (variable.position.x + contextToMoveTo.position.x) div 2;
      newRule.position.y := (variable.position.y + contextToMoveTo.position.y) div 2;
      domain.worldCommandList.doCommand(newRulesCommand);
      end;
    end;
  atLeastOneRuleChanged := false;
  for i := 0 to domain.world.rules.count - 1 do
  	begin
    rule := TSRule(domain.world.rules[i]);
    if rule.selected then
      begin
  		if contextToMoveTo.phrase <> rule.move.phrase then
        domain.worldCommandList.ruleFieldChange(rule, kRuleMove, contextToMoveTo.phrase);
      atLeastOneRuleChanged := true;
      end;
    end;
  if newRule <> nil then
    begin
  	domain.world.deselectAllExcept(newRule);
  	newRule.selected := true;
  	self.editRule(newRule);
  	self.activeControl := CommandEdit;
  	CommandEdit.selStart := 0;
  	CommandEdit.selLength := length(CommandEdit.text);
    end;
  if (newRule <> nil) or (atLeastOneRuleChanged) then
  	MapPaintBoxChanged
  else
    begin
    messageDlg('To build a link,' + chr(13) + 'select at least one context or command'
      + chr(13) + 'and right-click on a context.', mtInformation, [mbOk], 0);
    MapPaintBoxChanged;
    end;
	end;


procedure TRuleEditorForm.MenuToolsGenerateJavaClick(Sender: TObject);
  var
    javaWriter: TSJavaWriter;
    response: integer;
	begin
  response := MessageDlg('StoryHarp will compile the world file ' + chr(13) + chr(13) +
    '    ' + domain.worldFileName + chr(13) + chr(13) +
    'into a Java applet source code file ' + chr(13) + chr(13) +
 		'    ' + GetCurrentDir + '\Story.java' + chr(13) + chr(13) +
    'To produce a Java applet, you will need to compile' + chr(13) +
    'the Java source with a Java development system.' + chr(13) + chr(13) +
    'See the help system under "Java" for details.', mtconfirmation, [MBOK, MBCANCEL], 0) ;
  if response <> IDOK then exit;
  javaWriter := TSJavaWriter.create;
  try
  javaWriter.writeJavaProgram('Story.java');
  finally
  javaWriter.free;
  end;
  MessageDlg('File ' + GetCurrentDir + '\Story.java was written.', mtinformation, [MBOK], 0);
  end;

{ ----------------------------------------------------------------------------- *palette stuff }
function TRuleEditorForm.GetPalette: HPALETTE;
  begin
  result := PictureForm.PictureImage.picture.bitmap.palette;
  end;

{overriden because paint box will not update correctly}
{makes window take first priority for palettes}
function TRuleEditorForm.PaletteChanged(Foreground: Boolean): Boolean;
	var
  	oldPalette, palette: HPALETTE;
  	windowHandle: HWnd;
  	DC: HDC;
	begin
  result := false;
  if Application.terminated then exit;
  palette := getPalette;
  if palette <> 0 then
  	begin
    DC := getDeviceContext(WindowHandle);
    oldPalette := selectPalette(DC, palette, not foreground);
    { if palette changed, repaint drawing }
    if (realizePalette(DC) <> 0) and (PictureForm <> nil) and (PictureForm.PictureImage <> nil) then
      PictureForm.PictureImage.invalidate;
    selectPalette(DC, oldPalette, True);
    realizePalette(DC);
    releaseDC(windowHandle, DC);
  	end;
  result := inherited paletteChanged(foreground);
	end;

end.
