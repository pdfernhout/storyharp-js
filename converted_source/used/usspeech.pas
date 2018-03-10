unit USSpeech;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Dialogs,
		 USWorld, OleCtrls, AgentObjects_TLB;

type

  TSSpeechSystem = class(TObject)
   	commandsListenedFor: TStringList;
    lastSaidTextWithMacros: string;
    riddleIndex: integer;
		usingSpeech: boolean;
    agent: TAgent;
    mediaPlayerOpened: boolean;
    characterLoaded: boolean;
    sayOptionsMacroInForce: boolean;
    systemShuttingDown: boolean;
		constructor create;
		procedure connectToSpeechEngine;
		procedure loadAgentCharacter(fileName: string);
		procedure disconnectFromSpeechEngine;
		destructor destroy; override;
		procedure AgentCommand(Sender: TObject; UserInput: IDispatch);
		procedure doCommand(const utterance: string);
		procedure sayOptions;
		procedure clearVoiceCommands;
		procedure listenForPhraseCaptionCommand(const listenFor: string; const commandCaption: string; const command: string);
		procedure sayTextWithMacros(const aString: string);
		procedure listenForAvailableCommands;
		function hideRiddleAnswerForCommand(const aString: string): string;
		procedure listenForCommand(const aString: string);
    procedure speakText(const somethingToSay: string);
		procedure speakSound(const soundDesignation: string);
		procedure showPicture(const pictureName: string; const reply: string);
    function stripMacros(const aString: string): string;
    procedure haltSpeechAndSound;
		procedure haltSpeech;
		procedure haltSound;
		procedure checkForSayOptionsMacro;
    end;

const commandListCommand = 'options';
const repeatLastSayCommand = 'say that again';
const undoCommand = 'undo';
const redoCommand = 'redo';
const firstRiddleAnswer = 'say an answer for a riddle';
const agentName = 'StoryHarp';
function getWindowsMediaDirectory: string;

implementation

uses Forms, USConsoleForm, USDomain, USRuleEditorForm, MMSystem, MPlayer,
  USVariableCommands, registry, USAgentWarning, USPictureForm;
             
constructor TSSpeechSystem.create;
  var
  	tryToLaunchAgent: boolean;
    //registry: TRegistry;
   // theKey: string;
  	AgentWarningForm: TAgentWarningForm;
	begin
	commandsListenedFor := TStringList.create;
  usingSpeech := false;
  tryToLaunchAgent := true;
  {registry := TRegistry.create;
  try                       //\HKEY_CLASSES_ROOT\Agent.Server
  //registry.           HKEY_USERS\S-1-5-21-114899144-1244818041-1703228666-1000\Software\Microsoft\Microsoft Agent
  theKey := registry.currentPath;
  // theKey := theKey + 'd';
  registry.rootKey := HKEY_CLASSES_ROOT;
	if not registry.KeyExists('Agent.Server') then
//	if not registry.KeyExists('Software\Microsoft\Microsoft Agent') then
    begin
    tryToLaunchAgent :=	MessageDlg('Microsoft Agent does not appear to be installed.' + chr(13) +
      'Do you want to try to use it anyway?' + chr(13) +
      '(Choose No unless you have installed it.)', mtConfirmation, [mbYes, mbNo, mbCancel], 0) = IDYES;
    end;
  finally
  registry.free;
  end;  }
  try
  if tryToLaunchAgent then
  	try
      // for testing -- raise Exception.Create('Test exception');
			agent := TAgent.create(ConsoleForm);
  		usingSpeech := true;
  	except
      if not domain.options.suppressAgentNotPresentWarning then
        begin
  	  	AgentWarningForm := TAgentWarningForm.create(nil);
        try
    	  AgentWarningForm.showModal;
        finally
        AgentWarningForm.free;
        end;
        end;
  	end;
  if usingSpeech then
    begin
    agent.OnCommand := AgentCommand;
    end;
  self.connectToSpeechEngine;
  finally
  if not usingSpeech then
  	begin
    ConsoleForm.MenuOptionsSpeak.enabled := false;
    ConsoleForm.MenuOptionsSounds.enabled := false;
    ConsoleForm.MenuSettingsCharacter.enabled := false;
    ConsoleForm.MenuSettingsSayOptionsAfterLook.enabled := false;
    ConsoleForm.MenuOptionsVoiceUndo.enabled := false;
    ConsoleForm.MenuOptionsVoiceRedo.enabled := false;
    end
  else
    domain.options.suppressAgentNotPresentWarning := false; // reset this flag if it ever works
  end;
	end;

procedure TSSpeechSystem.connectToSpeechEngine;
  begin
  if not usingSpeech then exit;
  try
	agent.Connected := True;
  except
  	ShowMessage('Could not properly connect to Microsoft agent');
  	usingSpeech := false;
    exit;
  end;
  try
    self.loadAgentCharacter(domain.options.agentCharacterFileName);
  except
  	ShowMessage('Could not properly load Microsoft Agent character from EXE directory');
  	usingSpeech := false;
    exit;
  end;
  end;

procedure TSSpeechSystem.loadAgentCharacter(fileName: string);
  begin
  if (not fileExists(fileName)) and (pos(':', fileName) <= 0) then
      fileName := ExtractFilePath(Application.exeName) + fileName;
  if not fileExists(fileName) then
    begin
    ShowMessage('Agent character file ' + fileName + ' not found; using default.');
    fileName := ExtractFilePath(Application.exeName) + kDefaultAgentCharacterFileName;
    end;
  try
    if characterLoaded then
      agent.Characters.Unload(agentName);
		agent.Characters.Load(agentName, fileName);
    characterLoaded := true;
    domain.options.agentCharacterFileName := fileName;
  	try
			agent.Characters[agentName].Show(false);
			agent.Characters[agentName].SoundEffectsOn := true;
			//self.say('I am a genie who will tell you stories.');
  	except
    	ShowMessage('Could not properly configure Microsoft agent');
  		usingSpeech := false;
  	end;
  except
  	ShowMessage('Could not properly load Microsoft Agent character.');
  	usingSpeech := false;
  end;
  end;

{looks like bug in Agent that if shutting down will hang}
procedure TSSpeechSystem.disconnectFromSpeechEngine;
  begin
  if not usingSpeech then exit;
  if not systemShuttingDown then
    if agent <> nil then
		  agent.Connected := False;
  usingSpeech := false;
  end;

{looks like bug in Agent that if shutting down will hang}
destructor TSSpeechSystem.destroy;
	begin
  commandsListenedFor.free;
  commandsListenedFor := nil;
  self.disconnectFromSpeechEngine;
  if not systemShuttingDown then
    begin
    agent.free;
    agent := nil;
    end;
  inherited Destroy;
  end;

procedure TSSpeechSystem.AgentCommand(Sender: TObject; UserInput: IDispatch);
	var commandString: string;
	begin
	commandString := IAgentCtlUserInput(UserInput).Voice;
  if commandString <> '' then // I don't know why blank entries get sent
    begin
  	self.doCommand(commandString);
    if domain.options.updateEditorAfterCommandDone then
      RuleEditorForm.trackLastCommand;
    end;
  end;

procedure TSSpeechSystem.doCommand(const utterance: string);
  var
    commandPhrase: string;
    commandPhraseModified: string;
    {command: TSDoCommandPhrase;}
  begin
	self.haltSpeechAndSound;
  commandPhrase := utterance;
  if commandPhrase = undoCommand then
  	begin
    if domain.sessionCommandList.isUndoEnabled then
    	domain.sessionCommandList.undoLast
    else
    	self.speakText('There is nothing to undo.');
    exit;
    end;
  if commandPhrase = redoCommand then
  	begin
    if domain.sessionCommandList.isRedoEnabled then
    	domain.sessionCommandList.redoLast
    else
    	self.speakText('There is nothing to redo.');
    exit;
    end;
  if commandPhrase = repeatLastSayCommand then
    begin
    self.sayTextWithMacros(lastSaidTextWithMacros);
    exit;
    end;
  if (commandPhrase = commandListCommand) then
  	begin
    self.sayOptions;
    exit;
    end;
  // for riddles - need to be reassembled into command string first
  if commandPhrase = firstRiddleAnswer then
    begin
    commandPhrase := 'answer ';
    if not InputQuery('Riddle answer', 'Please enter the answer to a riddle.', commandPhrase) then
    	exit;
    end;

  if (commandsListenedFor.indexOf(commandPhrase) = -1) then
    commandPhraseModified := '$' + commandPhrase
  else
   	commandPhraseModified := commandPhrase;

  if (ConsoleForm.speechSystem.commandsListenedFor.indexOf(commandPhraseModified) = -1) then
    begin
  	// elimitate leading $
  	if (length(commandPhrase) > 1) and (commandPhrase[1] = '$') then
    	commandPhrase := copy(commandPhrase, 2, length(commandPhrase));
    
  	ConsoleForm.addLineToTranscript('> ' + commandPhrase, clRed);
    // bug - or bad riddle answer.
    ConsoleForm.addLineToTranscript('That accomplishes nothing.', clBlue);
  	ConsoleForm.scrollTranscriptEndIntoView;
    exit;
    end;

  {command := }domain.sessionCommandList.doCommandPhrase(commandPhraseModified);
  {if command.shiftsFocus then  - might lead to recursion if do look commands...}
  if (commandPhrase = 'look') and (domain.options.sayOptionsAfterLook) then
  	begin
    self.sayOptions;
    end;
  end;

procedure TSSpeechSystem.sayOptions;
	var
   thingsToSay: string;
   thing: string;
   i: integer;
	begin
  riddleIndex := 1;
  thingsToSay := '';
	if commandsListenedFor.count > 0 then
		for i := 0 to commandsListenedFor.count - 1 do
			begin
    	thing := commandsListenedFor[i];
      thing := self.hideRiddleAnswerForCommand(thing);
      // only tell of first riddle answer
      if (pos('$', commandsListenedFor[i]) = 1) and (riddleIndex > 2) then
      	thing := commandListCommand;
    	if (thing <> commandListCommand) and (thing <> undoCommand) and
      		(thing <> redoCommand) and (thing <> repeatLastSayCommand) then // and (thing <> 'look') then
      	begin
      	if Length(thingsToSay) <> 0 then
          begin
          thingsToSay := thingsToSay + ', ';
          // won't work if options are last
          // works, but sounds wrong
          //if i = commandsListenedFor.count - 1 then
          //	thingsToSay := thingsToSay + 'and ';
          end;
      	thingsToSay := thingsToSay + thing;
      	end;
    	end;
  if Length(Trim(thingsToSay)) = 0 then
  	thingsToSay := 'nothing except ' + commandListCommand + '.';
  self.speakText('You can say: ' + thingsToSay + '.');
  end;

function joinSentences(const firstSentence: string; const secondSentence: string): string;
  begin
  result := Trim(firstSentence);
  if (Trim(secondSentence)) <> '' then
    begin
    if result <> '' then
    	result := result + ' ';
  	result := result + Trim(secondSentence);
    end;
  end;

function TSSpeechSystem.stripMacros(const aString: string): string;
  var
    remaining, macro, toSay: string;
    startPosition, endPosition, wholeLength: integer;
  begin
  remaining := aString;
  result := '';
  wholeLength := Length(remaining);
  while (Length(remaining) > 0) do
    begin
    startPosition := pos('{', remaining);
    if startPosition > 0 then
      begin
      toSay := Copy(remaining, 1, startPosition - 1);
      result := joinSentences(result, toSay);
      remaining := Copy(remaining, startPosition + 1, wholeLength);
      end
    else
      begin
      result := joinSentences(result, remaining);
      exit;
      end;
    endPosition := pos('}', remaining);
    if endPosition = 0 then
      begin
      result := joinSentences(result, remaining); // error - unmatched braces
      exit;
      end;
    macro := Copy(remaining, 1, endPosition - 1);
    remaining := Copy(remaining, endPosition + 1, wholeLength);
    end;
  end;

procedure TSSpeechSystem.sayTextWithMacros(const aString: string);
  var
    remaining, macro, toSay: string;
    startPosition, endPosition, wholeLength: integer;
  begin
  remaining := aString;
  toSay := '';
  wholeLength := Length(remaining);
  while (Length(remaining) > 0) do
    begin
    startPosition := pos('{', remaining);
    if startPosition > 0 then
      begin
      toSay := Copy(remaining, 1, startPosition - 1);
      if Trim(toSay) <> '' then
      	self.speakText(toSay);
      remaining := Copy(remaining, startPosition + 1, wholeLength);
      end
    else
      begin
      if Trim(remaining) <> '' then
      	self.speakText(remaining);
      exit;
      end;
    endPosition := pos('}', remaining);
    if endPosition = 0 then
      begin
      self.speakText(remaining); // error - unmatched braces
      exit;
      end;
    macro := Trim(Copy(remaining, 1, endPosition - 1));
    remaining := Copy(remaining, endPosition + 1, wholeLength);
  	if pos('options', macro) = 1 then   // cfk added
    	self.sayOptionsMacroInForce := true
    else if pos('picture ', macro) = 1 then
      self.showPicture(macro, stripMacros(aString))
    else
    	self.speakSound(macro);
    end;
  end;

procedure TSSpeechSystem.checkForSayOptionsMacro;  // cfk added
  begin
  if not sayOptionsMacroInForce then exit;
  try
    self.sayOptions;
  finally
    sayOptionsMacroInForce := false;
  end;
  end;

procedure TSSpeechSystem.speakText(const somethingToSay: string);
  begin
  if not usingSpeech then exit;
  if not domain.options.playerSpeak then exit;
  agent.Characters[agentName].Speak(somethingToSay, '');
  end;

function findFileInDirectoryRecursive(const fileName : string; const searchDir: string): string;
	var
  	searchRec:  SysUtils.TSearchRec;
	begin
  result := '';
  {if searchDir[Length(searchDir)] <> '\' then
    searchDir := searchDir + '\'; }
  if FindFirst(searchDir + '*.*', faDirectory, searchRec) = 0 then
    repeat
    if searchRec.Name[1] <> '.' then
      begin
      if ((searchRec.Attr and faDirectory) > 0) then
        result := findFileInDirectoryRecursive(fileName, searchDir + searchRec.Name + '\')
      else
        begin
        if AnsiCompareText(searchRec.Name, fileName) = 0 then
        	result := searchDir + searchRec.Name;
        end;
      end;
		{ Application.ProcessMessages;  }
    until (result <> '') or (FindNext(searchRec) <> 0);
  SysUtils.FindClose(searchRec);
  end;

function getWindowsMediaDirectory: string;
  var
    windowsDirectory: string;
  begin
  result := '';
  SetLength(windowsDirectory, MAX_PATH + 3);
  GetWindowsDirectory(pchar(windowsDirectory), MAX_PATH + 1);
  windowsDirectory := Copy(windowsDirectory, 1, strlen(pchar(windowsDirectory)));
  if (windowsDirectory <> '') and (windowsDirectory[length(windowsDirectory)] <> '\') then
    windowsDirectory := windowsDirectory + '\';
  result := windowsDirectory + 'media\';
  end;

function findFileRecursivelyInMediaDirectories(const fileName : string; const extraMediaDirectory: string): string;
  var
    worldFileDirectory, exeDirectory, windowsMediaDirectory: string;
	begin
  // search in world file directory -- new sounds probably are where world is
  worldFileDirectory := ExtractFilePath(domain.worldFileName);
  if worldFileDirectory = '' then
    worldFileDirectory := GetCurrentDir + '\';
  if worldFileDirectory <> '' then
  	result := findFileInDirectoryRecursive(fileName, worldFileDirectory);
  if result <> '' then exit;
  // search in auxillary directory -- there might be an auxially CD-ROM
  if extraMediaDirectory <> '' then
  	result := findFileInDirectoryRecursive(fileName, extraMediaDirectory);
  if result <> '' then exit;
  // search in exe directory -- sounds might be shipped with the program
  exeDirectory := ExtractFilePath(Application.ExeName);
  if exeDirectory <> '' then
  	result := findFileInDirectoryRecursive(fileName, exeDirectory);
  if result <> '' then exit;
  // search in windows media directory -- default windows supplied items
  windowsMediaDirectory := getWindowsMediaDirectory;
  consoleForm.reportMode('Searching');
  if windowsMediaDirectory <> '' then
  	result := findFileInDirectoryRecursive(fileName, windowsMediaDirectory);
  consoleForm.reportMode('Running');
  end;

procedure TSSpeechSystem.haltSpeechAndSound;
  begin
  if not usingSpeech then exit;
  //if not domain.options.playerSpeak then exit;
  //if not domain.options.playerPlaySounds then exit;
  agent.Characters[agentName].StopAll('Play,Speak');
  end;

procedure TSSpeechSystem.haltSpeech;
  begin
  if not usingSpeech then exit;
  //if not domain.options.playerSpeak then exit;
  agent.Characters[agentName].StopAll('Speak');
  end;

procedure TSSpeechSystem.haltSound;
  begin
  if not usingSpeech then exit;
  //if not domain.options.playerPlaySounds then exit;
  agent.Characters[agentName].StopAll('Play');
  end;

procedure TSSpeechSystem.speakSound(const soundDesignation: string);
  var
    soundFile, soundFileWithPath: string;
    music: boolean;
    musiconce: boolean;
  begin
  soundFile := trim(soundDesignation);
  music := pos('music ', soundFile) = 1;
  musiconce := pos('musiconce ', soundFile) = 1;
  music := music or (soundFile = 'music');
  musiconce := musiconce or (soundFile = 'musiconce');
  if music or musiconce then // or musicwait then
    begin
    if not domain.options.playerPlayMusic then exit;
    if music then
    	soundFile := Copy(soundFile, length('music ') + 1, length(soundFile))
    else
      soundFile := Copy(soundFile, length('musiconce ') + 1, length(soundFile));
    soundFile := trim(soundFile);
    if soundFile = '' then
      begin
    	RuleEditorForm.mediaPlayer.close;
      RuleEditorForm.mediaPlayer.fileName := '';
      exit;
      end;
  	if pos('.', soundFile) <= 0 then
    	soundFile := soundFile + '.mid';
    soundFileWithPath := soundFile;
  	if pos(':', soundFileWithPath) <= 0 then
    	soundFileWithPath := findFileRecursivelyInMediaDirectories(soundFile, domain.options.extraMediaDirectory);
  	if (soundFileWithPath <> '') and (pos(':', soundFileWithPath) <= 0) then
    	soundFileWithPath := GetCurrentDir + '\' + soundFileWithPath;
  	if FileExists(soundFileWithPath) then
      begin
      if RuleEditorForm.mediaPlayer.fileName = soundFileWithPath then
      	begin
        // already playing
        if RuleEditorForm.mediaPlayer.mode = mpPlaying then
        	exit;
        end;
    	RuleEditorForm.mediaPlayer.fileName := soundFileWithPath;
    	//if not mediaPlayerOpened then
    	RuleEditorForm.mediaPlayer.open;
   		//mediaPlayerOpened := true;
     {RuleEditorForm.mediaPlayer.wait := musicwait;}
      RuleEditorForm.MediaPlayer.Notify := not musiconce;
      RuleEditorForm.loopMusic := true;
    	RuleEditorForm.mediaPlayer.play;
      end
    else
    	RuleEditorForm.mediaPlayer.close;
    exit;
    end;
  if not usingSpeech then exit;
  if not domain.options.playerPlaySounds then exit;
 { if pos('run ', soundFile) = 1 then
    begin
    soundFile := Copy(soundFile, length('run ') + 1, length(soundFile));
    WinExec(pchar(soundFile), SW_SHOWNORMAL);
    exit;
    end;}
  if pos(kPlaySoundMacroStart, soundFile) = 1 then
    soundFile := Copy(soundFile, length(kPlaySoundMacroStart) + 1, length(soundFile))
  else if soundFile = 'sound' then
    soundFile := '';
  soundFile := trim(soundFile);
  if (length(soundFile) > 0) and (pos('.', soundFile) <= 0) then
    soundFile := soundFile + '.wav';
  soundFileWithPath := soundFile;
  if pos(':', soundFileWithPath) <= 0 then
  	soundFileWithPath := findFileRecursivelyInMediaDirectories(soundFile, domain.options.extraMediaDirectory);
  if (soundFileWithPath <> '') and (pos(':', soundFileWithPath) <= 0) then
    soundFileWithPath := GetCurrentDir + '\' + soundFileWithPath;
  if FileExists(soundFileWithPath) then
    agent.Characters[agentName].Speak('', soundFileWithPath);
  end;

procedure TSSpeechSystem.showPicture(const pictureName: string; const reply: string);
  var
    pictureFile, pictureFileWithPath: string;
  begin
  pictureFile := Copy(pictureName, length('picture ') + 1, length(pictureName));
  pictureFile := trim(pictureFile);
  if pictureFile = '' then exit;
  if pos('.', pictureFile) <= 0 then
    pictureFile := pictureFile + '.bmp';
  pictureFileWithPath := pictureFile;
  if pos(':', pictureFileWithPath) <= 0 then
    pictureFileWithPath := findFileRecursivelyInMediaDirectories(pictureFile, domain.options.extraMediaDirectory);
  if (pictureFileWithPath <> '') and (pos(':', pictureFileWithPath) <= 0) then
    pictureFileWithPath := GetCurrentDir + '\' + pictureFileWithPath;
  if FileExists(pictureFileWithPath) then
    PictureForm.addPictureFromFile(pictureFileWithPath, reply);
  end;

procedure TSSpeechSystem.clearVoiceCommands;
  begin
  if not usingSpeech then exit;
	agent.Characters[agentName].Commands.RemoveAll;
  end;

function TSSpeechSystem.hideRiddleAnswerForCommand(const aString: string): string;
  begin
  if Pos('$', aString) = 1 then
    begin
    if riddleIndex = 1 then
    	result := firstRiddleAnswer
    else if riddleIndex = 2 then
     	result := 'say another answer for a riddle'
    else
   	  result := 'say yet another answer for a riddle - number ' + IntToStr(riddleIndex);
    riddleIndex := riddleIndex + 1;
    end
  else
    result := aString;
  end;

procedure TSSpeechSystem.listenForPhraseCaptionCommand(const listenFor: string; const commandCaption: string; const command: string);
  begin
  if (listenFor = '') or (command = '') or (commandCaption = '') then exit;
  if usingSpeech then
  	agent.Characters[agentName].Commands.Add(commandCaption, commandCaption, listenFor, true, true);
  commandsListenedFor.add(command);
  end;

procedure TSSpeechSystem.listenForCommand(const aString: string);
  var
  	listenFor: string;
    commandCaption: string;
  begin
  if (commandsListenedFor.indexOf(aString) = -1) then
    begin
    commandCaption := self.hideRiddleAnswerForCommand(aString);
    if commandCaption <> aString then
    	listenFor := Trim(Copy(aString,  2, Length(aString)))
    else
      listenFor := aString;
    self.listenForPhraseCaptionCommand(listenFor, commandCaption, aString);
    end;
  end;

// setsa up speech system and also updates GUI list of commands
procedure TSSpeechSystem.listenForAvailableCommands;
  var
  	rule: TSRule;
    i: integer;
    command: string;
  begin
  self.clearVoiceCommands;
  commandsListenedFor.clear;
  ConsoleForm.VisibleCommandsList.clear;

  riddleIndex := 1;

  if domain.world.rules.count <= 0 then exit;

  self.listenForCommand(commandListCommand);
  self.listenForCommand(repeatLastSayCommand);
  if domain.options.useVoiceToUndo then
  	self.listenForCommand(undoCommand);
  if domain.options.useVoiceToRedo then
  	self.listenForCommand(redoCommand);

  for i := 0 to domain.world.rules.count - 1 do
    begin
    rule := TSRule(domain.world.rules.items[i]);
    if rule.available then
      begin
      if rule.command.phrase = '' then continue;
      self.listenForCommand(rule.command.phrase);
      command := rule.command.phrase;
      // only list first riddle answer
      if (pos('$', rule.command.phrase) = 1) then
     		command := firstRiddleAnswer;
      if ConsoleForm.VisibleCommandsList.items.indexOf(command) = -1 then
        ConsoleForm.VisibleCommandsList.items.addObject(command, rule);
      end;
    end;
  end;

end.
