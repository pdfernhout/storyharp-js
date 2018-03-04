unit usdomain;

interface

uses Windows, Graphics, Forms, Classes, USWorld, USCommands, USMapView;

type
    DomainOptionsStructure = record
    	// files
      extraMediaDirectory, logFileName, agentCharacterFileName: string;
      mostRecentSession, mostRecentWorld: string;
      // player
      playerSpeak, playerPlaySounds, playerPlayMusic, showTranscript, showPictures: boolean;
      sayOptionsAfterLook, useVoiceToUndo, useVoiceToRedo: boolean;
      showVariables, updateEditorAfterCommandDone: boolean;
      playerFontSize: smallint;
      playerFontName: string;
      // Agent
      suppressAgentNotPresentWarning: boolean;
      // editor
      selectedItemColor, selectedTextColor, commandTextColorInMap: TColor;
      showCommandsInMap, showCommandPrefixInMap: boolean;
      tableFontName, mapFontName, browserFontName: string;
      tableFontSize, mapFontSize, browserFontSize: smallint;
      showRuleEditor, showButtonBar: boolean;
      browseBy: smallint;
      pageShowing: smallint;
      buttonSymbols: boolean;
      // windows
    	consoleWindowRect, editorWindowRect, logFileWindowRect: TRect;
      consoleBottomHeight, consoleRightWidth: integer;
      editorPanelEditorHeight, editorPanelRequirementsChangesHeight: integer;
      editorPanelFirstListWidth: integer;
      pictureWindowRect: TRect;
      end;

const
  kMinWidthOnScreen = 40; kMinHeightOnScreen = 20;
  kDefaultLogFileName = 'StoryHarp.log';
  kPageTable = 0; kPageMap = 1; kPageBrowser = 2;
  kDefaultAgentCharacterFileName = 'StoryHarp.acs';
  kDefaultIniFileName = 'StoryHarp.ini';
  kEncryptingMultiplierForAccumulatedUnregisteredTime = 1;
  kKeyForAccumulatedUnregisteredTime = 'Time scale fraction';

type
TSDomain = class(TObject)
    public
    world: TWorld;
    sessionCommandList: TSCommandList;
    worldCommandList: TSCommandList;
    sessionFileName: string;
    worldFileName: string;
    sessionChangeCount: integer;
    worldChangeCount: integer;
    isWorldFileLoaded: boolean;
    mapView: TSMapView;
    changeLock: integer;
    options: DomainOptionsStructure;
    iniFileName: string;
    sessionOrWorldStartupFileName: string;
    playerOnly: boolean;
    useIniFile: boolean;
    registrationName: string;
    registrationCode: string;
    registered: boolean;
    startTimeThisSession: TDateTime;
    accumulatedUnregisteredTime: TDateTime;
    justRegistered: boolean;
  	constructor create;
  	destructor destroy; override;
    procedure loadSession(fileName: string);
		procedure saveSession(fileName: string);
    procedure loadWorld(fileName: string);
		procedure mergeWorld(fileName: string);
		procedure saveWorld(fileName: string);
    function isWorldFileChanged: boolean;
    function isSessionFileChanged: boolean;
		procedure newWorld;
		procedure newSession;
    procedure beginUpdate;
    procedure endUpdate;
    function ignoreChanges: boolean;
		procedure resetWorldChangeCount;
    procedure worldChangeDone;
    procedure worldChangeUndone;
		procedure storeProfileInformation;
		procedure getProfileInformation;
    function loadIni(const section: string; const theField: string; const theDefault: string): string;
		procedure defaultOptions;
    procedure setFormSize(aForm: TForm; newRect: TRect);
		procedure readCommandLine;
		procedure readIniFile;
    function windowsDirectory: string;
		procedure loadFileAtStartupAndInitializeForms;
    end;

function min(a: single; b: single): single;
function max(a: single; b: single): single;

var
	domain: TSDomain;

const kUnsavedWorldFileName = 'untitled';
const kUnsavedSessionFileName = 'untitled';
const kWorldExtension = 'wld';
const kSessionExtension = 'ses';

implementation

uses SysUtils, IniFiles, USConsoleForm, USRuleEditorForm, UFileSupport, Dialogs, URegisterSupport;

function min(a: single; b: single): single;
  begin
  if (a < b) then result := a else result := b;
  end;

function max(a: single; b: single): single;
  begin
  if (a > b) then result := a else result := b;
  end;

function hexEncode(const aString: string): string;
  var
  	i: integer;
    letter: char;
  begin
  result := '';
  for i := 0 to length(aString) - 1 do
    begin
    letter := aString[i + 1]; // ((i+4) mod length(aString))
    result := result + chr(ord('A') + (ord(letter) div 32));
    result := result + chr(ord('A') + (ord(letter) mod 32));
    end;
  end;

function hexUnencode(const encodedString: string): string;
  var
  	i: integer;
    letter: char;
    value: integer;
  begin
  result := '';
  value := 0;
  for i := 0 to length(encodedString) - 1 do
    begin
    letter := encodedString[i + 1];
    if i mod 2 = 0 then
    	value := (ord(letter) - ord('A')) * 32
    else
      begin
   	  value := value + (ord(letter) - ord('A'));
    	result := result + chr(value);
      end;
    end;
  end;

constructor TSDomain.create;
	begin
  inherited create;
	world := TWorld.create;
  sessionCommandList := TSCommandList.create;
  sessionCommandList.setNewUndoLimit(1000);
  worldCommandList := TSCommandList.create;
  worldCommandList.setNewUndoLimit(1000);
  worldFileName := kUnsavedWorldFileName + '.' + kWorldExtension;
  sessionFileName := kUnsavedSessionFileName + '.' + kSessionExtension;
  sessionChangeCount := 0;
  worldChangeCount := 0;
  isWorldFileLoaded := false;
  mapView := TSMapView.create;
  sessionOrWorldStartupFileName := '';
  playerOnly := false;
  useIniFile := true;
  iniFileName := kDefaultIniFileName;
  self.readCommandLine;
  self.readIniFile;
  // if not registered then  // registration stored in ini file
  startTimeThisSession := Now;
  justRegistered := false;
  end;

destructor TSDomain.destroy;
	begin
  mapView.free;
  mapView := nil;
	sessionCommandList.free;
  sessionCommandList := nil;
	worldCommandList.free;
  worldCommandList := nil;
	world.free;
	world := nil;
  inherited destroy;
  end;

procedure TSDomain.readCommandLine;
  var i: integer;
  begin
  if ParamCount > 0 then
    begin
    for i := 1 to ParamCount do
      if uppercase(ParamStr(i)) = '/I=' then
        useIniFile := false
      else if uppercase(ParamStr(i)) = '/I' then
        useIniFile := false
      else if pos('/I=', uppercase(ParamStr(i))) = 1 then
        iniFileName := copy(ParamStr(i), 4, length(ParamStr(i)))
      else if pos('/I', uppercase(ParamStr(i))) = 1 then
        iniFileName := copy(ParamStr(i), 3, length(ParamStr(i)))
      else if pos('/P', uppercase(ParamStr(i))) = 1 then
        self.playerOnly := true
      else if (sessionOrWorldStartupFileName = '') and (pos('/', uppercase(ParamStr(i))) <> 1) then
        sessionOrWorldStartupFileName := ParamStr(i)
      else
        ShowMessage('Improper parameter string ' + ParamStr(i));
    end;
  end;

procedure TSDomain.readIniFile;
  var iniFileFound: boolean;
  begin
  if not useIniFile then
    begin
    self.defaultOptions;
    exit;
    end;
  if extractFilePath(iniFileName) <> '' then // ini file name has path
    begin
    // alternate ini file must exist before user uses it
    iniFileFound := fileExists(iniFileName);
    if not iniFileFound then
      begin
      ShowMessage('Could not find alternate settings file ' + chr(13) + chr(13)
        + '  ' + iniFileName + chr(13) + chr(13)
        + 'Using standard settings file in Windows directory instead.');
      iniFileName := kDefaultIniFileName;
      iniFileFound := fileExists(windowsDirectory + '\' + iniFileName);
      iniFileName := windowsDirectory + '\' + iniFileName;
      end;
    end
  else
    begin
    iniFileFound := fileExists(windowsDirectory + '\' + iniFileName);
    iniFileName := windowsDirectory + '\' + iniFileName;
    end;
  if iniFileFound and useIniFile then
    self.getProfileInformation
  else
    self.defaultOptions;
  end;

procedure TSDomain.loadFileAtStartupAndInitializeForms;
	var
   Year, Month, Day: Word;
  begin
  if sessionOrWorldStartupFileName <> '' then
    ConsoleForm.openSessionOrWorldFile(ParamStr(1))
  else
    begin
    if (options.mostRecentSession <> '') and (fileExists(options.mostRecentSession)) then
    	ConsoleForm.openSessionOrWorldFile(options.mostRecentSession)
    else if (options.mostRecentWorld <> '') and (fileExists(options.mostRecentWorld)) then
    	ConsoleForm.openSessionOrWorldFile(options.mostRecentWorld);
    end;
  if playerOnly then
    ConsoleForm.playerOnly;
  ConsoleForm.updateForRegistrationChange;
  ConsoleForm.updateTitles;
	RuleEditorForm.updateForRuleChange;
  RuleEditorForm.MapPaintBoxChanged;
  RuleEditorForm.adjustScrollBars;
  RuleEditorForm.updateViews;
  RuleEditorForm.editRule(nil);

  DecodeDate(Now, Year, Month, Day);
  if (not registered) and (Year >= 2000) then
    MessageDlg('This evaluation copy of StoryHarp is out of date.' + char(13) + char(13) +
    	'Please check for an updated evaluation version at:'  + char(13) +
      'http://www.kurtz-fernhout.com'  + char(13) +  char(13) +
      'The web site may also have updated pricing information.' + char(13) +
      'This message will disappear when the product is registered.', mtInformation, [mbOK], 0);
  end;

procedure TSDomain.storeProfileInformation;
  var
    section: string;
    iniFile: TIniFile;
    saveNumber: single;
  begin
  iniFile := TIniFile.create(iniFileName);
  try
  with iniFile, options do
    begin
    // files
    section := 'Files';
    writeString(section, 'Log file', logFileName);
    writeString(section, 'Agent character file', agentCharacterFileName);
    writeString(section, 'Extra media directory', extraMediaDirectory);
    writeString(section, 'Most recent session', mostRecentSession);
    writeString(section, 'Most recent world', mostRecentWorld);
    // player options
    section := 'Player';
    writeString(section, 'Speak', boolToStr(playerSpeak));
    writeString(section, 'Play sounds', boolToStr(playerPlaySounds));
    writeString(section, 'Play music', boolToStr(playerPlayMusic));
    writeString(section, 'Show transcript', boolToStr(showTranscript));
    writeString(section, 'Show pictures', boolToStr(showPictures));
    writeString(section, 'Say options after look', boolToStr(sayOptionsAfterLook));
    writeString(section, 'Use voice to undo', boolToStr(useVoiceToUndo));
    writeString(section, 'Use voice to redo', boolToStr(useVoiceToRedo));
    writeString(section, 'Show variables', boolToStr(showVariables));
    writeString(section, 'Update editor after command done', boolToStr(updateEditorAfterCommandDone));
    writeString(section, 'Suppress agent not present warning', boolToStr(suppressAgentNotPresentWarning));
    writeString(section, 'Player font size', intToStr(playerFontSize));
    writeString(section, 'Player font name', playerFontName);
    // editor options
    section := 'Editor';
    writeString(section, 'Background color for selected items', intToStr(selectedItemColor));
    writeString(section, 'Text color for selected items', intToStr(selectedTextColor));
    writeString(section, 'Text color for commands in map', intToStr(commandTextColorInMap));
    writeString(section, 'Show commands in map', boolToStr(showCommandsInMap));
    writeString(section, 'Show command prefix in map', boolToStr(showCommandPrefixInMap));
    writeString(section, 'Table font name', tableFontName);
    writeString(section, 'Table font size', intToStr(tableFontSize));
    writeString(section, 'Map font name', mapFontName);
    writeString(section, 'Map font size', intToStr(mapFontSize));
    writeString(section, 'Browser font name', browserFontName);
    writeString(section, 'Browser font size', intToStr(browserFontSize));
    writeString(section, 'Show rule editor', boolToStr(showRuleEditor));
    writeString(section, 'Show button bar', boolToStr(showButtonBar));
    writeString(section, 'Browse by (context, rule, reply, move, requirements, changes)', intToStr(browseBy));
    writeString(section, 'Page showing (table, map, browser)', intToStr(pageShowing));
    writeString(section, 'Button symbols', boolToStr(buttonSymbols));
    // registration, embedded here to hide time scale fraction
    // always track useage
    if justRegistered then
      begin
      section := 'Registration';
      writeString(section, 'R1', 'BQRESTYUBSHQYIBLJHSD');
      writeString(section, 'R2', 'BTBTBYBUOBTRST');
      writeString(section, 'R3', hexEncode(registrationCode));
      writeString(section, 'R4', hexEncode(registrationName));
     end;
    if (not registered) and (not self.playerOnly) then
      begin
   	  section := 'Editor';
    	accumulatedUnregisteredTime := accumulatedUnregisteredTime + max((now - startTimeThisSession), 0);
    	saveNumber := accumulatedUnregisteredTime * kEncryptingMultiplierForAccumulatedUnregisteredTime;
    	writeString(section, kKeyForAccumulatedUnregisteredTime, floatToStr(saveNumber));
      end;
    // windows
    section := 'Windows';
    writeString(section, 'Player window position', rectToString(consoleWindowRect));
    writeString(section, 'Editor window position', rectToString(editorWindowRect));
    writeString(section, 'Log file window position', rectToString(logFileWindowRect));
    writeString(section, 'Player horizontal splitter', intToStr(consoleBottomHeight));
    writeString(section, 'Player vertical splitter', intToStr(consoleRightWidth));
    writeString(section, 'Editor top splitter', intToStr(editorPanelEditorHeight));
    writeString(section, 'Editor bottom splitter', intToStr(editorPanelRequirementsChangesHeight));
    writeString(section, 'Editor browser splitter', intToStr(editorPanelFirstListWidth));
    writeString(section, 'Picture window position', rectToString(pictureWindowRect));
    end;
  finally
    iniFile.free;
  end;
  end;

procedure TSDomain.getProfileInformation;
  var
    section: string;
    iniFile: TIniFile;
    timeString: string;
    readNumber: single;
  begin
  iniFile := TIniFile.create(iniFileName);
  try
  with iniFile, options do
    begin
    // files
    section := 'Files';
    logFileName := readString(section, 'Log file', ExtractFilePath(Application.exeName) + kDefaultLogFileName);
    agentCharacterFileName := readString(section, 'Agent character file', ExtractFilePath(Application.exeName) + kDefaultAgentCharacterFileName);
    extraMediaDirectory := readString(section, 'Extra media directory', '');
    mostRecentSession := readString(section, 'Most recent session', '');
    mostRecentWorld := readString(section, 'Most recent world', '');
    // player options
    section := 'Player';
    playerSpeak := strToBool(readString(section, 'Speak', 'true'));
    playerPlaySounds := strToBool(readString(section, 'Play sounds', 'true'));
    playerPlayMusic := strToBool(readString(section, 'Play music', 'true'));
    showTranscript := strToBool(readString(section, 'Show transcript', 'true'));
    showPictures := strToBool(readString(section, 'Show pictures', 'true'));
    sayOptionsAfterLook := strToBool(readString(section, 'Say options after look', 'true'));
    useVoiceToUndo := strToBool(readString(section, 'Use voice to undo', 'false'));
    useVoiceToRedo := strToBool(readString(section, 'Use voice to redo', 'false'));
    showVariables := strToBool(readString(section, 'Show variables', 'false'));
    updateEditorAfterCommandDone := strToBool(readString(section, 'Update editor after command done', 'true'));
    suppressAgentNotPresentWarning := strToBool(readString(section, 'Suppress agent not present warning', 'false'));
    playerFontSize := strToInt(readString(section, 'Player font size', '8'));
    playerFontName := readString(section, 'Player font name', 'Arial');
    // editor options
    section := 'Editor';
    selectedItemColor := strToInt(readString(section, 'Background color for selected items', intToStr(clYellow)));
    selectedTextColor := strToInt(readString(section, 'Text color for selected items', intToStr(clBlack)));
    commandTextColorInMap := strToInt(readString(section, 'Text color for commands in map', intToStr(clBlue)));
    showCommandsInMap := strToBool(readString(section, 'Show commands in map', 'true'));
    showCommandPrefixInMap := strToBool(readString(section, 'Show command prefix in map', 'false'));
    tableFontName := readString(section, 'Table font name', 'Arial');
    tableFontSize := strToInt(readString(section, 'Table font size', '8'));
    mapFontName := readString(section, 'Map font name', 'Arial');
    mapFontSize := strToInt(readString(section, 'Map font size', '8'));
    browserFontName := readString(section, 'Browser font name', 'Arial');
    browserFontSize := strToInt(readString(section, 'Browser font size', '8'));
    showRuleEditor := strToBool(readString(section, 'Show rule editor', 'true'));
    showButtonBar := strToBool(readString(section, 'Show button bar', 'true'));
    browseBy := strToInt(readString(section, 'Browse by (context, rule, reply, move, requirements, changes)', '0'));
    pageShowing := strToInt(readString(section, 'Page showing (table, map, browser)', '0'));
    buttonSymbols := strToBool(readString(section, 'Button symbols', 'true'));
    // windows
    section := 'Windows';
    consoleWindowRect := stringToRect(readString(section, 'Player window position', '150 20 200 420'));
    editorWindowRect := stringToRect(readString(section, 'Editor window position', '40 30 560 400'));
    logFileWindowRect := stringToRect(readString(section, 'Log file window position', '50 50 550 400'));
    consoleBottomHeight := strToInt(readString(section, 'Player horizontal splitter', '200'));
    consoleRightWidth := strToInt(readString(section, 'Player vertical splitter', '200'));
    editorPanelEditorHeight := strToInt(readString(section, 'Editor top splitter', '150'));
    editorPanelRequirementsChangesHeight := strToInt(readString(section, 'Editor bottom splitter', '100'));
    editorPanelFirstListWidth := strToInt(readString(section, 'Editor browser splitter', '200'));
    pictureWindowRect := stringToRect(readString(section, 'Picture window position', '200 200 200 200'));
    // registration
    section := 'Registration';
    registrationName := readString(section, 'R4', '');
    registrationName := hexUnencode(registrationName);
    registrationCode := readString(section, 'R3', '');
    registrationCode := hexUnencode(registrationCode);
    registered := RegistrationMatch(registrationName, registrationCode);
    if not registered then
      begin
      section := 'Editor';
      timeString := readString(section, kKeyForAccumulatedUnregisteredTime, '0');
      readNumber := max(strToFloat(timeString), 0);
      accumulatedUnregisteredTime := readNumber / kEncryptingMultiplierForAccumulatedUnregisteredTime;
      end;
    end;
  finally
    iniFile.free;
  end;
  end;

procedure TSDomain.defaultOptions;
  begin
  // this is for first-time use, when there is no ini file
  with options do
    begin
    // files
    logFileName := ExtractFilePath(Application.exeName) + kDefaultLogFileName;
    agentCharacterFileName := ExtractFilePath(Application.exeName) + kDefaultAgentCharacterFileName;
    extraMediaDirectory := '';
    mostRecentSession := '';
    mostRecentWorld := '';
    // player options
    playerSpeak := true;
    playerPlaySounds := true;
    playerPlayMusic := true;
    showTranscript := true;
    showPictures := true;
    sayOptionsAfterLook := true;
    useVoiceToUndo := false;
    useVoiceToRedo := false;
    showVariables := false;
    updateEditorAfterCommandDone := true;
    suppressAgentNotPresentWarning := false;
    playerFontSize := 8;
    playerFontName := 'Arial';
    // editor options
    selectedItemColor := clYellow;
    selectedTextColor := clBlack;
    commandTextColorInMap := clBlue;
    showCommandsInMap := true;
    showCommandPrefixInMap := false;
    tableFontName := 'Arial';
    tableFontSize := 8;
    mapFontName := 'Arial';
    mapFontSize := 8;
    browserFontName := 'Arial';
    browserFontSize := 8;
    showRuleEditor := true;
    showButtonBar := true;
    browseBy := kRuleContext;
    pageShowing := 0;
    buttonSymbols := false;
    // windows
    consoleWindowRect := Rect(150, 20, 200, 420);
    editorWindowRect := Rect(40, 30, 560, 400);
    logFileWindowRect := Rect(50, 50, 550, 400);
    consoleBottomHeight := 200;
    consoleRightWidth := 200;
    editorPanelEditorHeight := 150;
    editorPanelRequirementsChangesHeight := 100;
    editorPanelFirstListWidth := 200;
    pictureWindowRect := Rect(200, 200, 200, 200);
    end;
  end;

function TSDomain.windowsDirectory: string;
  var
    cString: array[0..255] of char;
  begin
  result := '';
  getWindowsDirectory(cString, 256);
  result := strPas(cString);
  end;

function TSDomain.loadIni(const section: string; const theField: string; const theDefault: string): string;
  var
    iniFile: TIniFile;
  begin
  iniFile := TIniFile.create(iniFileName);
  try
    result := iniFile.ReadString(section, theField, theDefault);
  finally
    iniFile.free;
  end;
  end;

procedure TSDomain.setFormSize(aForm: TForm; newRect: TRect);
  begin
  if aForm = nil then exit;
  with newRect do
    if (left <> 0) or (right <> 0) or (top <> 0) or (bottom <> 0) then
      begin
      if left > screen.width - kMinWidthOnScreen then left := screen.width - kMinWidthOnScreen;
      if top > screen.height - kMinHeightOnScreen then top := screen.height - kMinHeightOnScreen;
      aForm.position := poDesigned;
      aForm.setBounds(left, top, right, bottom);
      end;
  end;

procedure TSDomain.newWorld;
  begin
  sessionCommandList.clear;
  sessionFileName := kUnsavedSessionFileName + '.' + kSessionExtension;
  sessionChangeCount := 0;
  worldCommandList.clear;
  worldFileName := kUnsavedWorldFileName + '.' + kWorldExtension;
  worldChangeCount := 0;
  isWorldFileLoaded := false;
  world.newWorld;
  end;

procedure TSDomain.newSession;
  begin
  sessionCommandList.clear;
  sessionFileName := kUnsavedSessionFileName + '.' + kSessionExtension;
  sessionChangeCount := 0;
  world.newSession;
  end;

procedure TSDomain.loadSession(fileName: string);
  begin
  sessionCommandList.clear;
  sessionFileName := fileName;
  sessionChangeCount := 0;
  if world.loadSessionFromFile(fileName, ExtractFileName(worldfileName)) then
    begin
  	options.mostRecentWorld := worldfileName;
  	options.mostRecentSession := fileName;
    end;
  end;

procedure TSDomain.saveSession(fileName: string);
  begin
  world.saveSessionToFile(fileName, ExtractFileName(worldfileName));
  options.mostRecentWorld := worldfileName;
  end;

function TSDomain.isSessionFileChanged: boolean;
  begin
  result := sessionChangeCount <> 0;
  end;

procedure TSDomain.loadWorld(fileName: string);
  begin
  sessionCommandList.clear;
  sessionFileName := kUnsavedSessionFileName + '.' + kSessionExtension;
  sessionChangeCount := 0;
  worldCommandList.clear;
  worldFileName := fileName;
  worldChangeCount := 0;
  world.resetVariablesAndRules;
  world.focus := nil;
  world.previousFocus := nil;
  RuleEditorForm.lastChoice := nil;
  RuleEditorForm.previousChoice := nil;
  if world.loadWorldFromFile(fileName) then
    begin
  	isWorldFileLoaded := true;
  	options.mostRecentWorld := worldfileName;
  	options.mostRecentSession := '';
    end;
  end;

procedure TSDomain.mergeWorld(fileName: string);
  begin
  // don't clear things
  world.loadWorldFromFile(fileName);
  inc(worldChangeCount);
  end;

procedure TSDomain.saveWorld(fileName: string);
  begin
  world.saveWorldToFile(fileName, kSaveAllRules);
  end;

function TSDomain.isWorldFileChanged: boolean;
  begin
  result := worldChangeCount <> 0;
  end;

procedure TSDomain.beginUpdate;
  begin
  inc(changeLock);
  end;

procedure TSDomain.endUpdate;
  begin
  dec(changeLock);
  end;

function TSDomain.ignoreChanges: boolean;
  begin
  result := changeLock <> 0;
  end;

procedure TSDomain.resetWorldChangeCount;
  begin
  worldChangeCount := 0;
  end;

procedure TSDomain.worldChangeDone;
  begin
  inc(worldChangeCount);
  RuleEditorForm.updateMenus;
  end;

procedure TSDomain.worldChangeUndone;
  begin
  dec(worldChangeCount);
  RuleEditorForm.updateMenus;
  end;

{initialization and finalization -----------------------------------------}

initialization
	begin
  domain := nil;
  domain := TSDomain.create;
	end;

finalization
	begin
  domain.free;
	end;

end.
