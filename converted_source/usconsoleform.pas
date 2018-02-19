unit usconsoleform;

interface

uses
  // Windows,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, ComCtrls, StdCtrls, 
  USWorld, USSpeech, UCommand, QuickFillComboBox, Buttons, MPlayer;

type
  TConsoleForm = class(TForm)
    StatusBar: TStatusBar;
    PanelConsole: TPanel;
    SplitterConsole: TSplitter;
    TranscriptEdit: TRichEdit;
    VisibleCommandsList: TListBox;
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuFileOpenSession: TMenuItem;
    MenuFileNewSession: TMenuItem;
    MenuFileSaveSession: TMenuItem;
    MenuFileSaveSessionAs: TMenuItem;
    N3: TMenuItem;
    MenuFileExit: TMenuItem;
    MenuEdit: TMenuItem;
    MenuEditUndo: TMenuItem;
    MenuEditRedo: TMenuItem;
    N5: TMenuItem;
    MenuEditCopy: TMenuItem;
    MenuOptions: TMenuItem;
    MenuOptionsSpeak: TMenuItem;
    MenuOptionsSounds: TMenuItem;
    MenuHelp: TMenuItem;
    MenuHelpContents: TMenuItem;
    MenuHelpPlayingStories: TMenuItem;
    MenuHelpRegisterLine: TMenuItem;
    MenuHelpAbout: TMenuItem;
    PanelVariables: TPanel;
    VariablesControlPanel: TPanel;
    ContextButton: TSpeedButton;
    MoveButton: TSpeedButton;
    RequirementsButton: TSpeedButton;
    ChangesButton: TSpeedButton;
    CommandButton: TSpeedButton;
    ShowOnlyTrueVariablesButton: TSpeedButton;
    FocusComboBox: TQuickFillComboBox;
    VariablesListBox: TListBox;
    SplitterVariables: TSplitter;
    MenuHelpRegister: TMenuItem;
    MenuEditStory: TMenuItem;
    MenuOptionsShowVariables: TMenuItem;
    MenuOptionsVoiceUndo: TMenuItem;
    MenuOptionsVoiceRedo: TMenuItem;
    MenuOptionsUpdateEditorSelections: TMenuItem;
    MenuFileOpenWorld: TMenuItem;
    N1: TMenuItem;
    N4: TMenuItem;
    MenuDevelopment: TMenuItem;
    N7: TMenuItem;
    MenuHelpAgent: TMenuItem;
    AfterRegisterMenuSeparator: TMenuItem;
    MenuOptionsPlayMusic: TMenuItem;
    MediaPath1: TMenuItem;
    N2: TMenuItem;
    MenuSettingsSayOptionsAfterLook: TMenuItem;
    MediaPlayer1: TMediaPlayer;
    picturesAndSymbolsPanel: TPanel;
    contextPicture: TImage;
    contextSymbol: TImage;
    commandPicture: TImage;
    commandSymbol: TImage;
    movePicture: TImage;
    moveSymbol: TImage;
    requirementsPicture: TImage;
    requirementsSymbol: TImage;
    changesPicture: TImage;
    changesSymbol: TImage;
    plusPicture: TImage;
    plusSymbol: TImage;
    MenuSettingsCharacter: TMenuItem;
    replyPicture: TImage;
    replySymbol: TImage;
    N8: TMenuItem;
    MenuEditRepeatLast: TMenuItem;
    MenuSettingsShowTranscript: TMenuItem;
    MenuSettingsPlayerFont: TMenuItem;
    FontDialog: TFontDialog;
    MenuSettingsShowPictures: TMenuItem;
    N6: TMenuItem;
    MenuFileOpenPictureWindow: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure VisibleCommandsListClick(Sender: TObject);
    procedure MenuOptionsSpeakClick(Sender: TObject);
    procedure MenuOptionsSoundsClick(Sender: TObject);
    procedure MenuEditUndoClick(Sender: TObject);
    procedure MenuEditRedoClick(Sender: TObject);
    procedure MenuFileOpenSessionClick(Sender: TObject);
    procedure MenuFileNewSessionClick(Sender: TObject);
    procedure MenuFileSaveSessionClick(Sender: TObject);
    procedure MenuFileSaveSessionAsClick(Sender: TObject);
    procedure MenuFileExitClick(Sender: TObject);
    procedure MenuEditCopyClick(Sender: TObject);
    procedure MenuWindowRuleEditorClick(Sender: TObject);
    procedure MenuHelpContentsClick(Sender: TObject);
    procedure MenuHelpPlayingStoriesClick(Sender: TObject);
    procedure MenuHelpAboutClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure VariablesListBoxDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure VariableButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FocusComboBoxKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FocusComboBoxChange(Sender: TObject);
    procedure VariablesListBoxMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MenuOptionsShowVariablesClick(Sender: TObject);
    procedure MenuEditStoryClick(Sender: TObject);
    procedure MenuOptionsVoiceUndoClick(Sender: TObject);
    procedure MenuOptionsVoiceRedoClick(Sender: TObject);
    procedure MenuOptionsUpdateEditorSelectionsClick(Sender: TObject);
    procedure MenuFileOpenWorldClick(Sender: TObject);
    procedure VariablesControlPanelResize(Sender: TObject);
    procedure MenuOptionsPlayMusicClick(Sender: TObject);
    procedure MediaPath1Click(Sender: TObject);
    procedure MenuSettingsSayOptionsAfterLookClick(Sender: TObject);
    procedure MenuSettingsCharacterClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MenuEditRepeatLastClick(Sender: TObject);
    procedure MenuHelpRegisterClick(Sender: TObject);
    procedure MenuHelpAgentClick(Sender: TObject);
    procedure MenuSettingsShowTranscriptClick(Sender: TObject);
    procedure PanelConsoleResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuSettingsPlayerFontClick(Sender: TObject);
    procedure PanelVariablesResize(Sender: TObject);
    procedure MenuSettingsShowPicturesClick(Sender: TObject);
    procedure MenuFileOpenPictureWindowClick(Sender: TObject);
  private
    procedure WMGetMinMaxInfo(var MSG: Tmessage);  message WM_GetMinMaxInfo;
    procedure WMQueryEndSession(var message: TWMQueryEndSession); message WM_QueryEndSession;
    procedure WMEndSession(var message: TWMEndSession); message WM_EndSession;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  public
    lastSaveProceeded: boolean;
    debugWorldLoaded: boolean;
    registered: boolean;
    speechSystem: TSSpeechSystem;
  	locationCacheValid: boolean;
    contextRect, requirementsRect, commandRect, moveRect, changesRect: TRect;
    isOnlyPlayer: boolean;
    openedEditorThisSession: boolean;
		procedure updateTitles;
		procedure openSessionOrWorldFile(const fileNameWithPath: string);
    procedure clearTranscript;
    procedure scrollTranscriptEndIntoView;
		procedure addLineToTranscript(const text: string; newColor: TColor);
		function askForSaveSessionAndProceed: boolean;
		function cleanUpBeforeExit: boolean;
		procedure updateForRegistrationChange;
		function storeIniFile: boolean;
		procedure updateMenusForUndoRedo;
		procedure updateForChangeToDomainOptions;
		procedure updateForChangeToShowingVariables;
    procedure commandChangedNotification(command: KfCommand; state: KfCommandChangeType);
		procedure updateMenus;
		procedure updateViews;
		procedure updateVariables;
		procedure setButtonGlyphs(buttonSymbols: boolean);
    procedure playerOnly;
    procedure reportMode(status: string);
		procedure updateForShowingTranscript;
    procedure DoShowHint(var HintStr: ansistring; var CanShow: Boolean; var HintInfo: THintInfo);
  end;

procedure drawGraphicCentered(graphic: TGraphic; canvas: TCanvas; rectangle: TRect);

var
  ConsoleForm: TConsoleForm;

implementation

uses Clipbrd, ShellAPI, uregister, USDomain, UFileSupport,
  USRuleEditorForm, USChangeLog, USMediaDirForm, uabout, USAbout, USPictureForm;

{$R *.DFM}

const
  kIconSize = 16;

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

procedure TConsoleForm.WMGetMinMaxInfo(var MSG: Tmessage);
  begin
  inherited;
  with PMinMaxInfo(MSG.lparam)^ do
    begin
    ptMinTrackSize.x := 285;
    ptMinTrackSize.y := 250;
    //ptMaxTrackSize.x := 558;
    end;
  end;               

procedure TConsoleForm.playerOnly;
  begin
  isOnlyPlayer := true;
  RuleEditorForm.hide;
  MenuDevelopment.visible := false;
  PanelVariables.visible := false;
  MenuHelpRegister.visible := false;
  MenuHelpRegisterLine.visible := false;
  StatusBar.Panels[1].Text := 'Player-only mode';
  end;

procedure TConsoleForm.FormCreate(Sender: TObject);
	begin
  Application.helpFile := extractFilePath(application.exeName) + 'StoryHarp.hlp';
  Application.OnShowHint := DoShowHint;
  Application.showHint := true;
  isOnlyPlayer := false;
  speechSystem := TSSpeechSystem.create;
  domain.sessionCommandList.notifyProcedure := self.commandChangedNotification;
  domain.setFormSize(self, domain.options.consoleWindowRect);
  self.updateForChangeToDomainOptions;
  if domain.options.consoleBottomHeight > 0 then
    begin
    VisibleCommandsList.height := domain.options.consoleBottomHeight;
    VisibleCommandsList.top := PanelConsole.clientHeight - VisibleCommandsList.height;
    end;
 // if PanelVariables.visible then
 //   begin
  	if (domain.options.consoleRightWidth > 0) then
      begin
    	PanelVariables.width := domain.options.consoleRightWidth;
      PanelVariables.left := self.clientWidth - PanelVariables.width;
      end;
 //   end;
  self.updateViews;
  self.updateForShowingTranscript;
 	end;

procedure TConsoleForm.setButtonGlyphs(buttonSymbols: boolean);
  begin
  if buttonSymbols then
    begin
    ContextButton.glyph := contextSymbol.picture.bitmap;
    CommandButton.glyph := commandSymbol.picture.bitmap;
    MoveButton.glyph := moveSymbol.picture.bitmap;
  	RequirementsButton.glyph := requirementsSymbol.picture.bitmap;
    ChangesButton.glyph := changesSymbol.picture.bitmap;
  	ShowOnlyTrueVariablesButton.glyph := plusSymbol.picture.bitmap;
    RuleEditorForm.replyPicture.picture.bitmap := replySymbol.picture.bitmap;
    end
 	else
    begin
    ContextButton.glyph := contextPicture.picture.bitmap;
    CommandButton.glyph := commandPicture.picture.bitmap;
    MoveButton.glyph := movePicture.picture.bitmap;
  	RequirementsButton.glyph := requirementsPicture.picture.bitmap;
    ChangesButton.glyph := changesPicture.picture.bitmap;
  	ShowOnlyTrueVariablesButton.glyph := plusPicture.picture.bitmap;
    RuleEditorForm.replyPicture.picture.bitmap := replyPicture.picture.bitmap;
    end;
  end;

procedure TConsoleForm.FormDestroy(Sender: TObject);
	begin
  speechSystem.free;
  speechSystem := nil;
  end;

procedure TConsoleForm.FormActivate(Sender: TObject);
	begin
  if application.terminated then exit;
  // pdf v1.2 modified from just updating variables
  domain.world.updateAvailable;
  self.updateViews;
	end;

procedure TConsoleForm.VisibleCommandsListClick(Sender: TObject);
  var theCommand: string;
	begin
  if VisibleCommandsList.itemIndex < 0 then exit;
  theCommand := VisibleCommandsList.items[VisibleCommandsList.itemIndex];
  speechSystem.doCommand(theCommand);
  if domain.options.updateEditorAfterCommandDone then
    RuleEditorForm.trackLastCommand;
	end;

{ --------------------------- HINTS ------------------------------------- }

procedure TConsoleForm.DoShowHint(var HintStr: ansistring; var CanShow: Boolean; var HintInfo: THintInfo);
  var
    itemAtPos, col, row: longint;
    listBox: TListBox;
    rule: TSRule;
  begin
  if Application.terminated then exit;
  if HintInfo.HintControl is TListBox then
    begin
  	listBox := HintInfo.HintControl as TListBox;
  	if (listBox = VisibleCommandsList) or (listBox = VariablesListBox)
    	or (listBox = RuleEditorForm.FirstListBox) or (listBox = RuleEditorForm.SecondListBox) then
    	begin
    	itemAtPos := listBox.itemAtPos(hintInfo.cursorPos, true); {true = must be on an item}
    	if (itemAtPos >= 0) then
      	begin
      	HintStr := listBox.items[itemAtPos];
      	hintInfo.cursorRect := listBox.itemRect(itemAtPos);
      	end;
    	end;
    end
  else if HintInfo.HintControl = RuleEditorForm.RuleGrid then
    begin
    RuleEditorForm.RuleGrid.mouseToCell(HintInfo.cursorPos.x, HintInfo.cursorPos.y, col, row);
    rule := nil;
	  if (row > 0) and (row <= domain.world.rules.count) then
		  rule := TSRule(domain.world.rules[row - 1]);
    if rule = nil then exit;
    HintStr := rule.getTextForField(col);
    hintInfo.cursorRect := RuleEditorForm.RuleGrid.cellRect(col, row);
    end;
  hintInfo.HintMaxWidth := 250;
  end;

{ --------------------------- TRANSCRIPT ------------------------------------- }

procedure TConsoleForm.clearTranscript;
  begin
  TranscriptEdit.Text := '';
  PictureForm.clearPictures; // is this the right place? cfk
  end;

procedure TConsoleForm.addLineToTranscript(const text: string; newColor: TColor);
  begin
  TranscriptEdit.selStart := Length(TranscriptEdit.Text);
	with TranscriptEdit.SelAttributes do
  	begin
    color := newColor;
  	name := domain.options.playerFontName;
		size := domain.options.playerFontSize;
  	end;
  TranscriptEdit.selText := text + char(13) + char(10);
  end;

procedure TConsoleForm.scrollTranscriptEndIntoView;
  begin
  TranscriptEdit.SelStart := Length(TranscriptEdit.Text);
  TranscriptEdit.SelLength := 0;
  TranscriptEdit.Perform(EM_SCROLLCARET, 0, 0);
  end;

{ --------------------------- SUPPORT --------------------------------------- }

// will need seperate one for sessions...

function TConsoleForm.askForSaveSessionAndProceed: boolean;
  var
    messageBoxResult: integer;
  begin
  result := true;
  if not domain.isSessionFileChanged then exit;
  application.bringToFront;
  {cfk fix - put help context in}
  messageBoxResult := MessageDlg('Save changes to session ' + extractFileName(domain.sessionFileName) + '?',
      mtConfirmation, mbYesNoCancel, 0);
  case messageBoxResult of
    IDCANCEL: result := false;
    IDYES:
    	begin
      self.MenuFileSaveSessionClick(Self);
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

function TConsoleForm.cleanUpBeforeExit: boolean; // returns false if user cancels
  var
  	response: integer;
  	i: integer;
  begin
  result := true;
  domain.options.consoleWindowRect := rect(left, top, width, height);
  domain.options.consoleBottomHeight := VisibleCommandsList.height;
  domain.options.consoleRightWidth := PanelVariables.width;
  with RuleEditorForm do
    begin
    domain.options.editorWindowRect := rect(left, top, width, height);
    domain.options.editorPanelEditorHeight := PanelEditor.height;
    domain.options.editorPanelRequirementsChangesHeight := PanelRequirementsChanges.height;
    if ListPages.ActivePage = TabSheetTable then
      domain.options.pageShowing := kPageTable
    else if ListPages.ActivePage = TabSheetMap then
      domain.options.pageShowing := kPageMap
    else if ListPages.ActivePage = TabSheetBrowse then
      domain.options.pageShowing := kPageBrowser
    else
      domain.options.pageShowing := kPageTable; // default
    domain.options.editorPanelFirstListWidth := PanelFirstList.width;
    end;
  with PictureForm do
    domain.options.pictureWindowRect := rect(left, top, width, height);
  with ChangeLogForm do
    domain.options.logFileWindowRect := rect(left, top, width, height);
  if domain.useIniFile then
    if not self.storeIniFile then
      begin
      result := false;
      exit;
      end;
  Randomize;
  for i := 0 to 30 do random;
  if (not domain.playerOnly) and (not domain.registered) and
  		(domain.accumulatedUnregisteredTime > 4.0 / 24.0) and
      (random < min(domain.accumulatedUnregisteredTime, 0.9)) then
    begin
    UnregisteredAboutForm.initializeWithWhetherClosingProgram(true);
    response := UnregisteredAboutForm.showModal;
    self.updateForRegistrationChange;
    if response = mrCancel then
      begin
      result := false;
      exit;
      end;
    end;
  Application.helpCommand(HELP_QUIT, 0);
  end;

procedure TConsoleForm.updateForRegistrationChange;
  var
    unregisteredEditing: boolean;
  begin
  unregisteredEditing := (not domain.registered) and (not domain.playerOnly);
  MenuHelpRegister.visible := unregisteredEditing;
  MenuHelpRegisterLine.visible := unregisteredEditing;
  StatusBar.visible := unregisteredEditing;
  if RuleEditorForm <> nil then
    begin
  	RuleEditorForm.MenuHelpRegister.visible := unregisteredEditing;
  	RuleEditorForm.AfterRegisterMenuSeparator.visible := unregisteredEditing;
    end;
  if domain.registered then
   	StatusBar.Panels[1].Text := 'Registered to ' + domain.registrationName
  else if not domain.playerOnly then
  	StatusBar.Panels[1].Text := 'Unregistered'
  else
  	StatusBar.Panels[1].Text := 'Player-only mode';
  end;

procedure TConsoleForm.reportMode(status: string);
  begin
  StatusBar.Panels[0].Text := status;
  StatusBar.refresh;
  end;

function TConsoleForm.storeIniFile: boolean;
  var
    fileSavedOK, choseAnotherFileName: boolean;
    buttonPressed: Word;
    saveDialog: TSaveDialog;
  begin
  result := true;
  fileSavedOK := false;
  choseAnotherFileName := false;
  while not fileSavedOK do
    begin
    try
      domain.storeProfileInformation;
      fileSavedOK := true;
    except
      fileSavedOK := false;
    end;
  	if not fileSavedOK then
    	begin
      buttonPressed := MessageDlg('Could not save settings to ' + chr(13) + chr(13)
        + '  ' + domain.iniFileName + chr(13) + chr(13)
        + 'Would you like to save them to another file?', mtError, mbYesNoCancel, 0);
      case buttonPressed of
        IDYES:
          begin
          saveDialog := TSaveDialog.create(application);
          try
            with saveDialog do
              begin
              fileName := domain.iniFileName;
              filter := 'Ini files (*.ini)|*.ini|All files (*.*)|*.*';
              defaultExt := 'ini';
              options := options + [ofPathMustExist, ofOverwritePrompt, ofHideReadOnly, ofNoReadOnlyReturn];
              end;
            result := saveDialog.execute;
            if result then
              begin
              domain.iniFileName := saveDialog.fileName;
              choseAnotherFileName := true;
              end;
          finally
            saveDialog.free;
          end;
          if not result then exit;
          end;
        IDNO: exit;
        IDCANCEL: begin result := false; exit; end;
        end;
    	end;
    end;
  if fileSavedOK and choseAnotherFileName then
    ShowMessage('Your settings have been saved in ' + chr(13) + chr(13)
        + '  ' + domain.iniFileName + chr(13) + chr(13)
        + 'But StoryHarp will load the original settings file again at startup.' + chr(13)
        + 'To use this settings file at startup, search in the help system' + chr(13)
        + 'for "alternate settings file".');
  end;

{ ---------------------- FILE MENU -------------------------}

procedure TConsoleForm.MenuFileOpenSessionClick(Sender: TObject);
	var
    fileNameWithPath: string;
	begin
	if not self.askForSaveSessionAndProceed then exit;
  fileNameWithPath := getFileOpenInfo(kFileTypeSession, domain.sessionFileName, 'Choose a session file', kOtherExtNotOk);
  if fileNameWithPath = '' then exit;
	self.speechSystem.haltSpeechAndSound;
  self.speechSystem.speakSound('music'); // turns it off
  self.openSessionOrWorldFile(fileNameWithPath);
  self.updateTitles;
  end;

procedure TConsoleForm.MenuFileOpenWorldClick(Sender: TObject);
	var
    fileNameWithPath: string;
	begin
	if not self.askForSaveSessionAndProceed then exit;
  fileNameWithPath := getFileOpenInfo(kFileTypeWorld, domain.sessionFileName, 'Choose a world file', kOtherExtNotOk);
  if fileNameWithPath = '' then exit;
	self.speechSystem.haltSpeechAndSound;
  self.speechSystem.speakSound('music'); // turns it off
  self.openSessionOrWorldFile(fileNameWithPath);
  self.updateTitles;
  end;

procedure TConsoleForm.WMDropFiles(var Msg: TWMDropFiles);
  var
    CFileName: array[0..MAX_PATH] of Char;
  begin
  try
    if DragQueryFile(Msg.Drop, 0, CFileName, MAX_PATH) > 0 then
    begin
      if (pos('.WLD', upperCase(CFileName)) <= 0) and (pos('.SES', upperCase(CFileName)) <= 0) then exit;
	    if not self.askForSaveSessionAndProceed then exit;
			self.speechSystem.haltSpeechAndSound;
  		self.speechSystem.speakSound('music'); // turns it off
  		self.openSessionOrWorldFile(CFileName);
  		self.updateTitles;
      Msg.Result := 0;
    end;
  finally
    DragFinish(Msg.Drop);
  end;
  end;

procedure TConsoleForm.openSessionOrWorldFile(const fileNameWithPath: string);
	var
    extension: string;
  begin
  extension := extractFileExt(fileNameWithPath);
  if upperCase(extension) = '.' + upperCase(kWorldExtension) then
    begin
		if not RuleEditorForm.askForSaveWorldAndProceed then exit;
    RuleEditorForm.openWorldFile(fileNameWithPath);
    domain.world.setInitialFocus;
  	self.clearTranscript;
  	domain.world.updateAvailable;
  	self.updateViews; // pdf v1.2 moved after updateAvailable
    if domain.world.rules.count > 0 then
    	speechSystem.doCommand(TSRule(domain.world.rules[0]).command.phrase);
    exit;
    end;
  if upperCase(extension) <> '.' + upperCase(kSessionExtension) then
    begin
    ShowMessage('Unsupported file extension for ' + fileNameWithPath + '(' + extension + ').');
    exit;
    end;
  try
    startWaitMessage('Opening ' + extractFileName(fileNameWithPath));
  	Domain.loadSession(fileNameWithPath);
  	domain.world.updateAvailable;
  except
    on E: Exception do
      begin
      stopWaitMessage;
  		ShowMessage(E.message);
  		ShowMessage('Could not load file ' + fileNameWithPath);
  		domain.newWorld;
  		self.updateViews;
    	exit;
      end;
	end;
  stopWaitMessage;
  self.clearTranscript;
  self.updateViews;
	end;

procedure TConsoleForm.MenuFileNewSessionClick(Sender: TObject);
	begin
	if not self.askForSaveSessionAndProceed then exit;
	self.speechSystem.haltSpeechAndSound;
  self.speechSystem.speakSound('music'); // turns it off
	Domain.newSession;
  self.clearTranscript;
  domain.world.updateAvailable;  // pdf v 1.2 moved above previous two
  self.updateViews;
  self.updateTitles;
  if domain.world.rules.count > 0 then
  	speechSystem.doCommand(TSRule(domain.world.rules[0]).command.phrase);
	end;

procedure TConsoleForm.MenuFileSaveSessionClick(Sender: TObject);
  var
    fileInfo: SaveFileNamesStructure;
  begin
  if pos(upperCase(kUnsavedSessionFileName), upperCase(ExtractFileName(Domain.sessionFileName))) > 0 then
    begin
    self.MenuFileSaveSessionAsClick(self);
    exit;
    end;
  lastSaveProceeded := getFileSaveInfo(kFileTypeSession, kDontAskForFileName, Domain.sessionFileName, fileInfo);
  if not lastSaveProceeded then exit;
  try
    startFileSave(fileInfo);
    Domain.saveSession(fileInfo.tempFile);
    fileInfo.writingWasSuccessful := true;
  finally
    lastSaveProceeded := cleanUpAfterFileSave(fileInfo);
  end;
  if lastSaveProceeded then
    begin
  	domain.sessionChangeCount := 0;
    domain.sessionCommandList.clear;
  	domain.options.mostRecentSession := fileInfo.newFile;
  	self.updateMenus;
    end;
	end;

procedure TConsoleForm.MenuFileSaveSessionAsClick(Sender: TObject);
  var
    fileInfo: SaveFileNamesStructure;
	begin
  lastSaveProceeded := getFileSaveInfo(kFileTypeSession, kAskForFileName, Domain.sessionFileName, fileInfo);
  if not lastSaveProceeded then exit;
  try
    startFileSave(fileInfo);
    Domain.saveSession(fileInfo.tempFile);
    fileInfo.writingWasSuccessful := true;
  finally
    lastSaveProceeded := cleanUpAfterFileSave(fileInfo);
  end;
  if lastSaveProceeded then
    begin
  	domain.sessionChangeCount := 0;
    domain.sessionCommandList.clear;
  	Domain.sessionFileName := fileInfo.newFile;
  	domain.options.mostRecentSession := fileInfo.newFile;
  	self.updateMenus;
    end;
  self.updateTitles;
	end;

procedure TConsoleForm.MenuFileExitClick(Sender: TObject);
  begin
	if not self.askForSaveSessionAndProceed then exit;
	if not RuleEditorForm.askForSaveWorldAndProceed then exit;
  if not self.cleanUpBeforeExit then exit;
  Application.terminate;
  end;

procedure TConsoleForm.WMQueryEndSession(var message: TWMQueryEndSession);
  begin
  inherited;
  {looks like bug in Agent that if shutting down will hang}
  if self.speechSystem.usingSpeech then
    begin
    MessageDlg('Please close StoryHarp before shutting down Windows.', mtInformation, [mbOk], 0);
    message.result := 0;
    exit;
    end;
	if not self.askForSaveSessionAndProceed then
    message.result := 0  // prevents windows from shutting down
  else if not RuleEditorForm.askForSaveWorldAndProceed then
    message.result := 0
  else if not self.cleanUpBeforeExit then
    message.result := 0;
  end;

procedure TConsoleForm.WMEndSession(var message: TWMEndSession);
  begin
  inherited;
  // speechSystem.systemShuttingDown := true;
  end;

procedure TConsoleForm.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
  { same as exit, but can't call exit because we have to set the action flag }
	if not self.askForSaveSessionAndProceed then
  	begin
    action := caNone;
    exit;
    end;
	if not RuleEditorForm.askForSaveWorldAndProceed then
  	begin
    action := caNone;
    exit;
    end;
  if not self.cleanUpBeforeExit then
    action := caNone;
  end;

{ ---------------------- EDIT MENU -------------------------}
procedure TConsoleForm.MenuEditUndoClick(Sender: TObject);
	begin
  domain.sessionCommandList.undoLast;
	end;

procedure TConsoleForm.MenuEditRedoClick(Sender: TObject);
	begin
  domain.sessionCommandList.redoLast;
	end;

procedure TConsoleForm.MenuEditCopyClick(Sender: TObject);
  var
  	clip: string;
	begin
  clip := TranscriptEdit.selText;
	Clipboard.setTextBuf(pchar(clip));
	end;

{ --------------- OPTIONS MENU ---------------}
procedure TConsoleForm.MenuOptionsSpeakClick(Sender: TObject);
	begin
  domain.options.playerSpeak := not domain.options.playerSpeak;
	MenuOptionsSpeak.Checked := domain.options.playerSpeak;
  if not domain.options.playerSpeak then
    speechSystem.haltSpeech;
	end;

procedure TConsoleForm.MenuOptionsSoundsClick(Sender: TObject);
	begin
  domain.options.playerPlaySounds := not domain.options.playerPlaySounds;
	MenuOptionsSounds.Checked := domain.options.playerPlaySounds;
  if not domain.options.playerPlaySounds then
    speechSystem.haltSound;
	end;

procedure TConsoleForm.MenuOptionsPlayMusicClick(Sender: TObject);
	begin
  domain.options.playerPlayMusic := not domain.options.playerPlayMusic;
	MenuOptionsPlayMusic.Checked := domain.options.playerPlayMusic;
  if not domain.options.playerPlayMusic then
    begin
    RuleEditorForm.mediaPlayer.close;
    RuleEditorForm.mediaPlayer.fileName := '';
    end;
	end;

procedure TConsoleForm.MenuSettingsShowTranscriptClick(Sender: TObject);
	begin
  domain.options.showTranscript := not domain.options.showTranscript;
  self.updateForShowingTranscript;
  self.PanelConsoleResize(self);
	end;

procedure TConsoleForm.MenuSettingsShowPicturesClick(Sender: TObject);
	begin
  domain.options.showPictures := not domain.options.showPictures;
  MenuSettingsShowPictures.checked := domain.options.showPictures;
  if not domain.options.showPictures then
    PictureForm.hide
  else
    PictureForm.show;
	end;

procedure TConsoleForm.updateForShowingTranscript;
  begin
  MenuSettingsShowTranscript.checked := domain.options.showTranscript;
  if domain.options.showTranscript then
    begin
    TranscriptEdit.visible := true;
    SplitterConsole.visible := true;
  	if domain.options.consoleBottomHeight > 0 then
    	begin
    	VisibleCommandsList.height := domain.options.consoleBottomHeight;
    	VisibleCommandsList.top := PanelConsole.clientHeight - VisibleCommandsList.height;
    	end;
    end
  else
    begin
    TranscriptEdit.visible := false;
    SplitterConsole.visible := false;
    VisibleCommandsList.top := SplitterConsole.height;
    VisibleCommandsList.height := PanelConsole.clientHeight - SplitterConsole.height;
    end;
  end;

{ --------------- DEVELOPMENT MENU ---------------}
procedure TConsoleForm.MenuWindowRuleEditorClick(Sender: TObject);
	begin
	RuleEditorForm.show;
	end;

{ --------------- HELP MENU ---------------}
procedure TConsoleForm.MenuHelpContentsClick(Sender: TObject);
	begin
	application.helpCommand(HELP_FINDER, 0);
  end;

procedure TConsoleForm.MenuHelpPlayingStoriesClick(Sender: TObject);
	begin
	Application.helpJump('Playing_StoryHarp_Audioventures');
	end;

procedure TConsoleForm.MenuHelpAgentClick(Sender: TObject);
	begin
	Application.helpJump('Installing_and_Using_Microsoft_Agent');
	end;

procedure TConsoleForm.MenuHelpRegisterClick(Sender: TObject);
	begin
	RegistrationForm.showModal;
  self.updateForRegistrationChange;
	end;

procedure TConsoleForm.MenuHelpAboutClick(Sender: TObject);
	begin
  if domain.registered then
    begin
    if aboutForm = nil then exit;
    aboutForm.setUpAsSplashOrAbout(kAsAbout);
    aboutForm.showModal;
    end
  else
    begin
    if UnRegisteredAboutForm = nil then exit;
    UnRegisteredAboutForm.initializeWithWhetherClosingProgram(false);
    UnRegisteredAboutForm.showModal;
    self.updateForRegistrationChange;
    end;
	end;

{ ----------------------------- MENU UPDATING ------------------------------ }
procedure TConsoleForm.updateForChangeToDomainOptions;
  begin
  MenuOptionsSpeak.checked := domain.options.playerSpeak;
	MenuOptionsSounds.Checked := domain.options.playerPlaySounds;
	MenuOptionsPlayMusic.Checked := domain.options.playerPlayMusic;
  MenuSettingsShowTranscript.checked := domain.options.showTranscript;
  MenuSettingsShowPictures.checked := domain.options.showPictures;
  MenuSettingsSayOptionsAfterLook.checked := domain.options.sayOptionsAfterLook;
	MenuOptionsVoiceUndo.Checked := domain.options.useVoiceToUndo;
	MenuOptionsVoiceRedo.Checked := domain.options.useVoiceToRedo;
	MenuOptionsShowVariables.Checked := domain.options.showVariables;
  self.font.name := domain.options.playerFontName;
  self.font.size := domain.options.playerFontSize;
  VariablesListBox.itemHeight := localIntMax(self.canvas.textHeight('W') + 2, 16);
  TranscriptEdit.font.name := domain.options.playerFontName;
  TranscriptEdit.font.size := domain.options.playerFontSize;
  TranscriptEdit.invalidate;
  self.updateForChangeToShowingVariables;
 	MenuOptionsUpdateEditorSelections.checked := domain.options.updateEditorAfterCommandDone;
  end;

procedure TConsoleForm.updateForChangeToShowingVariables;
  begin
  if domain.options.showVariables then
    begin
    self.updateVariables;
    PanelVariables.show;
    SplitterVariables.show;
    SplitterVariables.left := PanelConsole.width;
    end
  else
    begin
    PanelVariables.hide;
    SplitterVariables.hide;
    end
	end;

procedure TConsoleForm.updateMenusForUndoRedo;
	begin
  if domain.sessionCommandList.isUndoEnabled then
  	begin
    MenuEditUndo.enabled := true;
    MenuEditUndo.caption := '&Undo ' + domain.sessionCommandList.undoDescription;
    end
  else
  	begin
    MenuEditUndo.enabled := false;
    MenuEditUndo.caption := 'Can''t undo';
    end;
  if domain.sessionCommandList.isRedoEnabled then
  	begin
    MenuEditRedo.enabled := true;
    MenuEditRedo.caption := '&Redo ' + domain.sessionCommandList.redoDescription;
    end
  else
  	begin
    MenuEditRedo.enabled := false;
    MenuEditRedo.caption := 'Can''t redo';
    end;
  end;

procedure TConsoleForm.commandChangedNotification(command: KfCommand; state: KfCommandChangeType);
	begin
  case state of
    commandDone: inc(domain.sessionChangeCount);
    commandUndone: dec(domain.sessionChangeCount);
  end;
  self.updateMenus; // may also change save availability
	end;

procedure TConsoleForm.updateMenus;
  begin
  MenuFileSaveSession.enabled := domain.isSessionFileChanged;
  MenuFileSaveSessionAs.enabled := true;
  self.updateMenusForUndoRedo;
  //MenuFileCloseSession.enabled := domain.isWorldFileLoaded;
  end;

procedure TConsoleForm.updateViews;
  begin
  self.updateMenus;
  speechSystem.listenForAvailableCommands;
  if panelVariables.visible then self.updateVariables;
  end;

{----------------------- variables ----------------------------------}

procedure TConsoleForm.updateVariables;
  var
  	i: integer;
    variable: TSVariable;
    oldTop: string;
    oldTopIndex: integer;
  begin
  oldTop := '';
  oldTopIndex := VariablesListBox.topIndex;
  if (oldTopIndex >= 0) and (VariablesListBox.items.count > oldTopIndex) then
    oldTop := VariablesListBox.items[oldTopIndex];
  VariablesListBox.clear;
  if not locationCacheValid then
  	FocusComboBox.clear;
  if domain.world.variables.count > 0 then
    for i := 0 to domain.world.variables.count - 1 do
      begin
      variable := TSVariable(domain.world.variables[i]);
      if not ShowOnlyTrueVariablesButton.down or (variable.getState = kPresent) then
        begin
        if (ContextButton.down and (variable.contextUseages > 0)) or
        (MoveButton.down and (variable.moveUseages > 0)) or
        (RequirementsButton.down and (variable.requirementsUseages > 0)) or
        (ChangesButton.down and (variable.changesUseages > 0)) or
        (CommandButton.down and (variable.commandUseages > 0)) then
       		VariablesListBox.items.addObject(variable.phrase, variable);
        end;
      if not locationCacheValid then
      	if (variable.contextUseages > 0) or (variable.moveUseages > 0) then
      		FocusComboBox.items.addObject(variable.phrase, variable);
      end;
  VariablesListBox.invalidate;
  VariablesListBox.refresh;
  locationCacheValid := true;
  if oldTopIndex >= 0 then
    begin
    if (oldTop <> '') and (VariablesListBox.items.count > oldTopIndex) and (VariablesListBox.items[oldTopIndex] = oldTop) then
      VariablesListBox.topIndex := oldTopIndex;
    end;
  if domain.world.focus = nil then exit;
  FocusComboBox.ItemIndex := FocusComboBox.items.IndexOf(domain.world.focus.phrase);
  end;

procedure drawRectangle(canvas: TCanvas; rectangle: TRect);
  begin
  canvas.Brush.style := bsClear;
  canvas.pen.color := clGray;
  with rectangle do
  	canvas.rectangle(left, top, right+1, bottom+1);
  end;

procedure drawGraphicCentered(graphic: TGraphic; canvas: TCanvas; rectangle: TRect);
  begin
  canvas.draw(
  	rectangle.left + (rectangle.right - rectangle.left) div 2 - graphic.width div 2,
    rectangle.top + (rectangle.bottom - rectangle.top) div 2 - graphic.height div 2,
    graphic);
  end;

procedure carveOffRect(var bigRect: TRect; var littleRect: TRect; width: integer; fromRight: boolean);
  begin
  littleRect := bigRect;
  if fromRight then
    begin
    bigRect.right := bigRect.right - width;
 		littleRect.left := bigRect.right;
    end
  else
    begin
    bigRect.left := bigRect.left + width;
  	littleRect.right := bigRect.left;
    end;
  end;


procedure TConsoleForm.VariablesListBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
  var
    selected: boolean;
    wastedRect: TRect;
    bigRect, stateRect, stringRect: TRect;
    variable: TSVariable;
    displayString: string;
  begin
  if Application.terminated then exit;
  if (VariablesListBox.items.count <= 0) or (index < 0) or (index > domain.world.variables.count - 1) then exit;
  selected := (odSelected in state);
  variable := VariablesListBox.items.objects[index] as TSVariable;
  if variable = nil then exit;

  { set up rectangles }
  bigRect := rect;
  if VariablesListBox.items.count <= (VariablesListBox.clientHeight div kIconSize) then
   	carveOffRect(bigRect, wastedRect, kIconSize, true);

  carveOffRect(bigRect, stateRect, kIconSize, false);
  carveOffRect(bigRect, commandRect, kIconSize, true);
  carveOffRect(bigRect, changesRect, kIconSize, true);
  carveOffRect(bigRect, requirementsRect, kIconSize, true);
  carveOffRect(bigRect, moveRect, kIconSize, true);
  carveOffRect(bigRect, contextRect, kIconSize, true);

  stringRect := bigRect;

  { fill first box with white, rest with clHighlight if selected }
  with VariablesListBox.canvas do
    begin
    brush.style := bsSolid;
    brush.color := clWindow;
    fillRect(rect);
    if variable.getState = kPresent then
    	drawGraphicCentered(ShowOnlyTrueVariablesButton.Glyph, VariablesListBox.canvas, stateRect);
    if variable.contextUseages > 0 then
    	drawGraphicCentered(ContextButton.Glyph, VariablesListBox.canvas, contextRect);
    if variable.requirementsUseages > 0 then
    	drawGraphicCentered(RequirementsButton.Glyph, VariablesListBox.canvas, requirementsRect);
    if variable.commandUseages > 0 then
    	drawGraphicCentered(CommandButton.Glyph, VariablesListBox.canvas, commandRect);
    if variable.moveUseages > 0 then
    	drawGraphicCentered(MoveButton.Glyph, VariablesListBox.canvas, moveRect);
    if variable.changesUseages > 0 then
    	drawGraphicCentered(ChangesButton.Glyph, VariablesListBox.canvas, changesRect);
    font := VariablesListBox.font;
    if selected then
      begin
      brush.color := clHighlight;
      font.color := clHighlightText;
      end
    else
      begin
      brush.color := clWindow;
      font.color := clBtnText;
      end;
    fillRect(stringRect);
    stringRect.left := stringRect.left + 5; { margin for text }
    displayString := VariablesListBox.items[index] + '...............................................' +
    '.............................................................................................';
    drawText(handle, Pchar(displayString), Length(displayString), stringRect, DT_LEFT);
    end;
  end;

procedure TConsoleForm.VariableButtonClick(Sender: TObject);
	begin
	self.updateVariables;
	end;

procedure TConsoleForm.FormResize(Sender: TObject);
	begin
	VariablesListBox.invalidate;
  if PanelVariables.Width > self.clientWidth - 60 - SplitterVariables.width then
    begin
  	PanelVariables.Left := 60 + SplitterVariables.width;
  	PanelVariables.Width := self.clientWidth - 60 - SplitterVariables.width;
    end;
	FocusComboBox.Width := VariablesControlPanel.clientWidth - FocusComboBox.left - 4;
	end;

procedure TConsoleForm.PanelConsoleResize(Sender: TObject);
	begin
  if TranscriptEdit.visible then
    begin
  	if VisibleCommandsList.height > PanelConsole.clientHeight - 60 - SplitterConsole.height then
    	begin
  		VisibleCommandsList.top := 60 + SplitterConsole.height;
  		VisibleCommandsList.height := PanelConsole.clientHeight - 60 - SplitterConsole.height;
    	end;
    end
  else
    begin
    VisibleCommandsList.top := SplitterConsole.height;
    VisibleCommandsList.height := PanelConsole.clientHeight - SplitterConsole.height;
    end;
	end;

procedure TConsoleForm.FocusComboBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
	begin
  self.FocusComboBoxChange(Sender);
	end;

procedure TConsoleForm.FocusComboBoxChange(Sender: TObject);
  var
  	newFocus: TSVariable;
    index: integer;
    oldSelStart: integer;
	begin
  index := FocusComboBox.items.indexOf(FocusComboBox.text);
  if index < 0 then exit;
  newFocus := FocusComboBox.items.objects[index] as TSVariable;
  if (newFocus = domain.world.focus) and (newFocus.getState = kPresent) then exit;
  oldSelStart := FocusComboBox.selStart;
  domain.sessionCommandList.moveFocus(newFocus);
  FocusComboBox.selStart := oldSelStart;
	end;

procedure TConsoleForm.VariablesListBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
	var
		variable: TSVariable;
	begin
	if VariablesListBox.itemIndex < 0 then exit;
	if VariablesListBox.itemIndex >  VariablesListBox.items.count - 1 then exit;
  variable := TSVariable(VariablesListBox.items.objects[VariablesListBox.itemIndex]);
	if x < 16 then
  	begin
  	domain.sessionCommandList.toggleVariable(variable);
    exit;
  	end;
  if (RuleEditorForm = nil) or (not RuleEditorForm.visible) then exit;
  if (x > contextRect.left) and (x <= contextRect.right) then
    begin
    if variable.contextUseages <= 0 then exit;
    RuleEditorForm.setOrganizeByField(kRuleContext);
    end
  else if (x > requirementsRect.left) and (x <= requirementsRect.right) then
    begin
    if variable.requirementsUseages <= 0 then exit;
    RuleEditorForm.setOrganizeByField(kRuleRequirements);
    end
  else if (x > commandRect.left) and (x <= commandRect.right) then
    begin
    if variable.commandUseages <= 0 then exit;
    RuleEditorForm.setOrganizeByField(kRuleCommand);
    end
  else if (x > moveRect.left) and (x <= moveRect.right) then
    begin
    if variable.moveUseages <= 0 then exit;
    RuleEditorForm.setOrganizeByField(kRuleMove);
    end
  else if (x > changesRect.left) and (x <= changesRect.right) then
    begin
    if variable.changesUseages <= 0 then exit;
    RuleEditorForm.setOrganizeByField(kRuleChanges);
    end
  else
  	exit;
  RuleEditorForm.ListPages.ActivePage := RuleEditorForm.TabSheetBrowse;
  RuleEditorForm.firstListBox.itemIndex := RuleEditorForm.firstListBox.items.indexOf(variable.phrase);
  RuleEditorForm.loadSecondListBox;
	end;

procedure TConsoleForm.MenuOptionsShowVariablesClick(Sender: TObject);
	begin
  domain.options.showVariables := not domain.options.showVariables;
	MenuOptionsShowVariables.Checked := domain.options.showVariables;
  if domain.options.showVariables then
    self.clientWidth := self.clientWidth + PanelVariables.width + SplitterVariables.width
  else
    self.clientWidth := self.clientWidth - PanelVariables.width - SplitterVariables.width;
  self.updateForChangeToShowingVariables;
  end;

procedure TConsoleForm.MenuEditStoryClick(Sender: TObject);
	begin
	RuleEditorForm.show;
  openedEditorThisSession := true;
	end;

procedure TConsoleForm.MenuOptionsVoiceUndoClick(Sender: TObject);
	begin
  domain.options.useVoiceToUndo := not domain.options.useVoiceToUndo;
	MenuOptionsVoiceUndo.Checked := domain.options.useVoiceToUndo;
	end;

procedure TConsoleForm.MenuOptionsVoiceRedoClick(Sender: TObject);
	begin
  domain.options.useVoiceToRedo := not domain.options.useVoiceToRedo;
	MenuOptionsVoiceRedo.Checked := domain.options.useVoiceToRedo;
	end;

procedure TConsoleForm.MenuOptionsUpdateEditorSelectionsClick(Sender: TObject);
	begin
  domain.options.updateEditorAfterCommandDone := not domain.options.updateEditorAfterCommandDone;
 	MenuOptionsUpdateEditorSelections.checked := domain.options.updateEditorAfterCommandDone;
	end;

const
kProgramName = 'StoryHarp';
//kProgramName = 'TeleTale';
{kProgramName = 'Audioventure'; }

// kProgramName = 'PhraseMaze';
// kProgramName = 'TalkingTales';
// kProgramName = 'TalkTales';
// kProgramName = 'Voiceventure';
// kProgramName = 'VoicePaths';
// kProgramName = 'VoiceWalk';
// kProgramName = 'VoiceTracks';
// kProgramName = 'VoiceTrip';
// kProgramName = 'VoiceVenture';
// kProgramName = 'VoiceTrek';
// kProgramName = 'Voicecapade';
// kProgramName = 'VoicePaths';
// kProgramName = 'Storyspeak';
// kProgramName = 'Storytalk';

 {
 At the dawn of the third millenium,
 the laws of space and time keep humans close to Sol.
 Most of them live in billions of space habitats called 'gardens'.
 These are their stories...
 }

{
 At the dawn of the third millenium,
 the laws of space and time keep humans close to Sol.
 Most of them live in billions of space habitats called 'gardens'.
 These are their stories...
 }

procedure TConsoleForm.updateTitles;
  begin
  self.caption := 'StoryHarp - ' + ExtractFileName(domain.worldFileName)
   + ' (' + ExtractFileName(domain.sessionFileName) + ')'; 
  RuleEditorForm.caption := 'StoryHarp World Editor - ' + ExtractFileName(domain.worldFileName);
  end;

procedure TConsoleForm.PanelVariablesResize(Sender: TObject);
	begin
  VariablesControlPanel.height := FocusComboBox.height + ContextButton.height + 4 * 3;
	end;

procedure TConsoleForm.VariablesControlPanelResize(Sender: TObject);
	begin
  with FocusComboBox do setBounds(4, 4,
  	VariablesControlPanel.clientWidth - FocusComboBox.left - 4, height);
  ContextButton.top := FocusComboBox.top + FocusComboBox.height + 4;
  MoveButton.top := ContextButton.top;
  RequirementsButton.top := ContextButton.top;
  ChangesButton.top := ContextButton.top;
  CommandButton.top := ContextButton.top;
  ShowOnlyTrueVariablesButton.top := ContextButton.top;
	end;

procedure TConsoleForm.MenuSettingsCharacterClick(Sender: TObject);
  var
    fileNameWithPath: string;
	begin
  fileNameWithPath := getFileOpenInfo(kFileTypeAgentCharacter,
  		kDefaultAgentCharacterFileName, 'Choose a Microsoft Agent character file', kOtherExtNotOK);
  if fileNameWithPath = '' then exit;
  speechSystem.loadAgentCharacter(fileNameWithPath);
	end;

procedure TConsoleForm.MediaPath1Click(Sender: TObject);
	begin
  ExtraMediaDirectoryForm.showModal;
	end;

procedure TConsoleForm.MenuSettingsSayOptionsAfterLookClick(Sender: TObject);
	begin
  domain.options.sayOptionsAfterLook := not domain.options.sayOptionsAfterLook;
  MenuSettingsSayOptionsAfterLook.checked := domain.options.sayOptionsAfterLook;
	end;

procedure TConsoleForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
	begin
  if key = VK_ESCAPE then
	  speechSystem.haltSpeechAndSound;
	end;

procedure TConsoleForm.MenuEditRepeatLastClick(Sender: TObject);
	begin
  speechSystem.haltSpeechAndSound;
  speechSystem.sayTextWithMacros(speechSystem.lastSaidTextWithMacros);
	end;

procedure TConsoleForm.FormShow(Sender: TObject);
	begin
  DragAcceptFiles(Handle, True);
	end;

procedure TConsoleForm.MenuSettingsPlayerFontClick(Sender: TObject);
	begin
  fontDialog.font.name := domain.options.playerFontName;
  fontDialog.font.size := domain.options.playerFontSize;
  if fontDialog.execute then
    begin
    domain.options.playerFontName := fontDialog.font.name;
    domain.options.playerFontSize := fontDialog.font.size;
    self.font.name := domain.options.playerFontName;
    self.font.size := domain.options.playerFontSize;
    VariablesListBox.itemHeight := localIntMax(self.canvas.textHeight('W') + 2, 16);
    TranscriptEdit.font.name := domain.options.playerFontName;
    TranscriptEdit.font.size := domain.options.playerFontSize;
    // make existing text change font 
  	TranscriptEdit.selStart := 0;
    TranscriptEdit.selLength := Length(TranscriptEdit.Text);
		TranscriptEdit.SelAttributes.name := domain.options.playerFontName;
		TranscriptEdit.SelAttributes.size := domain.options.playerFontSize;
    TranscriptEdit.selLength := 0;
		self.PanelVariablesResize(self);
		self.VariablesControlPanelResize(self);
    end;
	end;

procedure TConsoleForm.MenuFileOpenPictureWindowClick(Sender: TObject);
	begin
  PictureForm.show;
	end;

end.
