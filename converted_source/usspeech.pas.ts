// unit USSpeech

from conversion_common import *
import uspictureform
import usagentwarning
import usvariablecommands
import usruleeditorform
import usdomain
import usconsoleform
import usworld
import delphi_compatability

// const
const commandListCommand = "options"


// const
const repeatLastSayCommand = "say that again"


// const
const undoCommand = "undo"


// const
const redoCommand = "redo"


// const
const firstRiddleAnswer = "say an answer for a riddle"


// const
const agentName = "StoryHarp"


function joinSentences(firstSentence: string, secondSentence: string): string {
    let result = ""
    result = trim(firstSentence)
    if ((trim(secondSentence)) !== "") {
        if (result !== "") {
            result = result + " "
        }
        result = result + trim(secondSentence)
    }
    return result
}

function findFileInDirectoryRecursive(fileName: string, searchDir: string): string {
    let result = ""
    let searchRec: SysUtils.TSearchRec
    
    result = ""
    if (UNRESOLVED.FindFirst(searchDir + "*.*", UNRESOLVED.faDirectory, searchRec) === 0) {
        // Application.ProcessMessages;  
        do {
            if searchRec.Name[1] != ".":
                if ((searchRec.Attr and UNRESOLVED.faDirectory) > 0):
                    //if searchDir[Length(searchDir)] <> '\' then
                    //    searchDir := searchDir + '\'; 
                    result = findFileInDirectoryRecursive(fileName, searchDir + searchRec.Name + "\\")
                else:
                    if UNRESOLVED.AnsiCompareText(searchRec.Name, fileName) == 0:
                        result = searchDir + searchRec.Name
        } while (!((result !== "") || (UNRESOLVED.FindNext(searchRec) !== 0)))
    }
    UNRESOLVED.SysUtils.FindClose(searchRec)
    return result
}

function getWindowsMediaDirectory(): string {
    let result = ""
    let windowsDirectory: string
    
    result = ""
    UNRESOLVED.SetLength(windowsDirectory, UNRESOLVED.MAX_PATH + 3)
    UNRESOLVED.GetWindowsDirectory(windowsDirectory, UNRESOLVED.MAX_PATH + 1)
    windowsDirectory = UNRESOLVED.Copy(windowsDirectory, 1, len(windowsDirectory))
    if ((windowsDirectory !== "") && (windowsDirectory[len(windowsDirectory)] !== "\\")) {
        windowsDirectory = windowsDirectory + "\\"
    }
    result = windowsDirectory + "media\\"
    return result
}

function findFileRecursivelyInMediaDirectories(fileName: string, extraMediaDirectory: string): string {
    let result = ""
    let worldFileDirectory: string
    let exeDirectory: string
    let windowsMediaDirectory: string
    
    // search in world file directory -- new sounds probably are where world is
    worldFileDirectory = ExtractFilePath(usdomain.domain.worldFileName)
    if (worldFileDirectory === "") {
        worldFileDirectory = UNRESOLVED.GetCurrentDir + "\\"
    }
    if (worldFileDirectory !== "") {
        result = findFileInDirectoryRecursive(fileName, worldFileDirectory)
    }
    if (result !== "") {
        return result
    }
    if (extraMediaDirectory !== "") {
        // search in auxillary directory -- there might be an auxially CD-ROM
        result = findFileInDirectoryRecursive(fileName, extraMediaDirectory)
    }
    if (result !== "") {
        return result
    }
    // search in exe directory -- sounds might be shipped with the program
    exeDirectory = ExtractFilePath(delphi_compatability.Application.ExeName)
    if (exeDirectory !== "") {
        result = findFileInDirectoryRecursive(fileName, exeDirectory)
    }
    if (result !== "") {
        return result
    }
    // search in windows media directory -- default windows supplied items
    windowsMediaDirectory = getWindowsMediaDirectory()
    usconsoleform.ConsoleForm.reportMode("Searching")
    if (windowsMediaDirectory !== "") {
        result = findFileInDirectoryRecursive(fileName, windowsMediaDirectory)
    }
    usconsoleform.ConsoleForm.reportMode("Running")
    return result
}


export class TSSpeechSystem {
    commandsListenedFor: TStringList = new TStringList()
    lastSaidTextWithMacros: string = ""
    riddleIndex: int = 0
    usingSpeech: boolean = false
    agent: TAgent = new TAgent()
    mediaPlayerOpened: boolean = false
    characterLoaded: boolean = false
    sayOptionsMacroInForce: boolean = false
    systemShuttingDown: boolean = false
    
    create(): void {
        let tryToLaunchAgent: boolean
        let AgentWarningForm: TAgentWarningForm
        
        //registry: TRegistry;
        // theKey: string;
        this.commandsListenedFor = delphi_compatability.TStringList.create
        this.usingSpeech = false
        tryToLaunchAgent = true
        try {
            if (tryToLaunchAgent) {
                try {
                    //registry := TRegistry.create;
                    //  try                       //\HKEY_CLASSES_ROOT\Agent.Server
                    //  //registry.           HKEY_USERS\S-1-5-21-114899144-1244818041-1703228666-1000\Software\Microsoft\Microsoft Agent
                    //  theKey := registry.currentPath;
                    //  // theKey := theKey + 'd';
                    //  registry.rootKey := HKEY_CLASSES_ROOT;
                    //	if not registry.KeyExists('Agent.Server') then
                    ////	if not registry.KeyExists('Software\Microsoft\Microsoft Agent') then
                    //    begin
                    //    tryToLaunchAgent :=	MessageDlg('Microsoft Agent does not appear to be installed.' + chr(13) +
                    //      'Do you want to try to use it anyway?' + chr(13) +
                    //      '(Choose No unless you have installed it.)', mtConfirmation, [mbYes, mbNo, mbCancel], 0) = IDYES;
                    //    end;
                    //  finally
                    //  registry.free;
                    //  end;  
                    // for testing -- raise Exception.Create('Test exception');
                    this.agent = UNRESOLVED.TAgent.create(usconsoleform.ConsoleForm)
                    this.usingSpeech = true
                } catch (Exception e) {
                    if (!usdomain.domain.options.suppressAgentNotPresentWarning) {
                        AgentWarningForm = usagentwarning.TAgentWarningForm().Create(null)
                        try {
                            AgentWarningForm.ShowModal()
                        } finally {
                            AgentWarningForm.free
                        }
                    }
                }
            }
            if (this.usingSpeech) {
                this.agent.OnCommand = this.AgentCommand
            }
            this.connectToSpeechEngine()
        } finally {
            if (!this.usingSpeech) {
                usconsoleform.ConsoleForm.MenuOptionsSpeak.enabled = false
                usconsoleform.ConsoleForm.MenuOptionsSounds.enabled = false
                usconsoleform.ConsoleForm.MenuSettingsCharacter.enabled = false
                usconsoleform.ConsoleForm.MenuSettingsSayOptionsAfterLook.enabled = false
                usconsoleform.ConsoleForm.MenuOptionsVoiceUndo.enabled = false
                usconsoleform.ConsoleForm.MenuOptionsVoiceRedo.enabled = false
            } else {
                // reset this flag if it ever works
                usdomain.domain.options.suppressAgentNotPresentWarning = false
            }
        }
    }
    
    connectToSpeechEngine(): void {
        if (!this.usingSpeech) {
            return
        }
        try {
            this.agent.Connected = true
        } catch (Exception e) {
            ShowMessage("Could not properly connect to Microsoft agent")
            this.usingSpeech = false
            return
        }
        try {
            this.loadAgentCharacter(usdomain.domain.options.agentCharacterFileName)
        } catch (Exception e) {
            ShowMessage("Could not properly load Microsoft Agent character from EXE directory")
            this.usingSpeech = false
            return
        }
    }
    
    loadAgentCharacter(fileName: string): void {
        if ((!FileExists(fileName)) && (UNRESOLVED.pos(":", fileName) <= 0)) {
            fileName = ExtractFilePath(delphi_compatability.Application.exeName) + fileName
        }
        if (!FileExists(fileName)) {
            ShowMessage("Agent character file " + fileName + " not found; using default.")
            fileName = ExtractFilePath(delphi_compatability.Application.exeName) + usdomain.kDefaultAgentCharacterFileName
        }
        try {
            if (this.characterLoaded) {
                this.agent.Characters.Unload(agentName)
            }
            this.agent.Characters.Load(agentName, fileName)
            this.characterLoaded = true
            usdomain.domain.options.agentCharacterFileName = fileName
            try {
                this.agent.Characters[agentName].Show(false)
                this.agent.Characters[agentName].SoundEffectsOn = true
            } catch (Exception e) {
                //self.say('I am a genie who will tell you stories.');
                ShowMessage("Could not properly configure Microsoft agent")
                this.usingSpeech = false
            }
        } catch (Exception e) {
            ShowMessage("Could not properly load Microsoft Agent character.")
            this.usingSpeech = false
        }
    }
    
    //looks like bug in Agent that if shutting down will hang
    disconnectFromSpeechEngine(): void {
        if (!this.usingSpeech) {
            return
        }
        if (!this.systemShuttingDown) {
            if (this.agent !== null) {
                this.agent.Connected = false
            }
        }
        this.usingSpeech = false
    }
    
    //looks like bug in Agent that if shutting down will hang
    destroy(): void {
        this.commandsListenedFor.free
        this.commandsListenedFor = null
        this.disconnectFromSpeechEngine()
        if (!this.systemShuttingDown) {
            this.agent.free
            this.agent = null
        }
        TObject.prototype.destroy.call(this)
    }
    
    AgentCommand(Sender: TObject, UserInput: IDispatch): void {
        let commandString: string
        
        commandString = UNRESOLVED.IAgentCtlUserInput(UserInput).Voice
        if (commandString !== "") {
            // I don't know why blank entries get sent
            this.doCommand(commandString)
            if (usdomain.domain.options.updateEditorAfterCommandDone) {
                usruleeditorform.RuleEditorForm.trackLastCommand()
            }
        }
    }
    
    doCommand(utterance: string): void {
        let commandPhrase: string
        let commandPhraseModified: string
        
        //command: TSDoCommandPhraseCommand;
        this.haltSpeechAndSound()
        commandPhrase = utterance
        if (commandPhrase === undoCommand) {
            if (usdomain.domain.sessionCommandList.isUndoEnabled()) {
                usdomain.domain.sessionCommandList.undoLast()
            } else {
                this.speakText("There is nothing to undo.")
            }
            return
        }
        if (commandPhrase === redoCommand) {
            if (usdomain.domain.sessionCommandList.isRedoEnabled()) {
                usdomain.domain.sessionCommandList.redoLast()
            } else {
                this.speakText("There is nothing to redo.")
            }
            return
        }
        if (commandPhrase === repeatLastSayCommand) {
            this.sayTextWithMacros(this.lastSaidTextWithMacros)
            return
        }
        if ((commandPhrase === commandListCommand)) {
            this.sayOptions()
            return
        }
        if (commandPhrase === firstRiddleAnswer) {
            // for riddles - need to be reassembled into command string first
            commandPhrase = "answer "
            if (!InputQuery("Riddle answer", "Please enter the answer to a riddle.", commandPhrase)) {
                return
            }
        }
        if ((this.commandsListenedFor.IndexOf(commandPhrase) === -1)) {
            commandPhraseModified = "$" + commandPhrase
        } else {
            commandPhraseModified = commandPhrase
        }
        if ((usconsoleform.ConsoleForm.speechSystem.commandsListenedFor.IndexOf(commandPhraseModified) === -1)) {
            if ((len(commandPhrase) > 1) && (commandPhrase[1] === "$")) {
                // elimitate leading $
                commandPhrase = UNRESOLVED.copy(commandPhrase, 2, len(commandPhrase))
            }
            usconsoleform.ConsoleForm.addLineToTranscript("> " + commandPhrase, delphi_compatability.clRed)
            // bug - or bad riddle answer.
            usconsoleform.ConsoleForm.addLineToTranscript("That accomplishes nothing.", delphi_compatability.clBlue)
            usconsoleform.ConsoleForm.scrollTranscriptEndIntoView()
            return
        }
        //command := 
        usdomain.domain.sessionCommandList.doCommandPhrase(commandPhraseModified)
        if ((commandPhrase === "look") && (usdomain.domain.options.sayOptionsAfterLook)) {
            //if command.shiftsFocus then  - might lead to recursion if do look commands...
            this.sayOptions()
        }
    }
    
    sayOptions(): void {
        let thingsToSay: string
        let thing: string
        let i: int
        
        this.riddleIndex = 1
        thingsToSay = ""
        if (this.commandsListenedFor.Count > 0) {
            for (i = 0; i <= this.commandsListenedFor.Count - 1; i++) {
                thing = this.commandsListenedFor[i]
                thing = this.hideRiddleAnswerForCommand(thing)
                if ((UNRESOLVED.pos("$", this.commandsListenedFor[i]) === 1) && (this.riddleIndex > 2)) {
                    // only tell of first riddle answer
                    thing = commandListCommand
                }
                if ((thing !== commandListCommand) && (thing !== undoCommand) && (thing !== redoCommand) && (thing !== repeatLastSayCommand)) {
                    if (len(thingsToSay) !== 0) {
                        // and (thing <> 'look') then
                        thingsToSay = thingsToSay + ", "
                        // won't work if options are last
                        // works, but sounds wrong
                        //if i = commandsListenedFor.count - 1 then
                        //	thingsToSay := thingsToSay + 'and ';
                    }
                    thingsToSay = thingsToSay + thing
                }
            }
        }
        if (len(trim(thingsToSay)) === 0) {
            thingsToSay = "nothing except " + commandListCommand + "."
        }
        this.speakText("You can say: " + thingsToSay + ".")
    }
    
    stripMacros(aString: string): string {
        let result = ""
        let remaining: string
        let macro: string
        let toSay: string
        let startPosition: int
        let endPosition: int
        let wholeLength: int
        
        remaining = aString
        result = ""
        wholeLength = len(remaining)
        while ((len(remaining) > 0)) {
            startPosition = UNRESOLVED.pos("{", remaining)
            if (startPosition > 0) {
                toSay = UNRESOLVED.Copy(remaining, 1, startPosition - 1)
                result = joinSentences(result, toSay)
                remaining = UNRESOLVED.Copy(remaining, startPosition + 1, wholeLength)
            } else {
                result = joinSentences(result, remaining)
                return result
            }
            endPosition = UNRESOLVED.pos("}", remaining)
            if (endPosition === 0) {
                // error - unmatched braces
                result = joinSentences(result, remaining)
                return result
            }
            macro = UNRESOLVED.Copy(remaining, 1, endPosition - 1)
            remaining = UNRESOLVED.Copy(remaining, endPosition + 1, wholeLength)
        }
        return result
    }
    
    sayTextWithMacros(aString: string): void {
        let remaining: string
        let macro: string
        let toSay: string
        let startPosition: int
        let endPosition: int
        let wholeLength: int
        
        remaining = aString
        toSay = ""
        wholeLength = len(remaining)
        while ((len(remaining) > 0)) {
            startPosition = UNRESOLVED.pos("{", remaining)
            if (startPosition > 0) {
                toSay = UNRESOLVED.Copy(remaining, 1, startPosition - 1)
                if (trim(toSay) !== "") {
                    this.speakText(toSay)
                }
                remaining = UNRESOLVED.Copy(remaining, startPosition + 1, wholeLength)
            } else {
                if (trim(remaining) !== "") {
                    this.speakText(remaining)
                }
                return
            }
            endPosition = UNRESOLVED.pos("}", remaining)
            if (endPosition === 0) {
                // error - unmatched braces
                this.speakText(remaining)
                return
            }
            macro = trim(UNRESOLVED.Copy(remaining, 1, endPosition - 1))
            remaining = UNRESOLVED.Copy(remaining, endPosition + 1, wholeLength)
            if (UNRESOLVED.pos("options", macro) === 1) {
                // cfk added
                this.sayOptionsMacroInForce = true
            } else if (UNRESOLVED.pos("picture ", macro) === 1) {
                this.showPicture(macro, this.stripMacros(aString))
            } else {
                this.speakSound(macro)
            }
        }
    }
    
    // cfk added
    checkForSayOptionsMacro(): void {
        if (!this.sayOptionsMacroInForce) {
            return
        }
        try {
            this.sayOptions()
        } finally {
            this.sayOptionsMacroInForce = false
        }
    }
    
    speakText(somethingToSay: string): void {
        if (!this.usingSpeech) {
            return
        }
        if (!usdomain.domain.options.playerSpeak) {
            return
        }
        this.agent.Characters[agentName].Speak(somethingToSay, "")
    }
    
    haltSpeechAndSound(): void {
        if (!this.usingSpeech) {
            return
        }
        //if not domain.options.playerSpeak then exit;
        //if not domain.options.playerPlaySounds then exit;
        this.agent.Characters[agentName].StopAll("Play,Speak")
    }
    
    haltSpeech(): void {
        if (!this.usingSpeech) {
            return
        }
        //if not domain.options.playerSpeak then exit;
        this.agent.Characters[agentName].StopAll("Speak")
    }
    
    haltSound(): void {
        if (!this.usingSpeech) {
            return
        }
        //if not domain.options.playerPlaySounds then exit;
        this.agent.Characters[agentName].StopAll("Play")
    }
    
    speakSound(soundDesignation: string): void {
        let soundFile: string
        let soundFileWithPath: string
        let music: boolean
        let musiconce: boolean
        
        soundFile = trim(soundDesignation)
        music = UNRESOLVED.pos("music ", soundFile) === 1
        musiconce = UNRESOLVED.pos("musiconce ", soundFile) === 1
        music = music || (soundFile === "music")
        musiconce = musiconce || (soundFile === "musiconce")
        if (music || musiconce) {
            if (!usdomain.domain.options.playerPlayMusic) {
                // or musicwait then
                return
            }
            if (music) {
                soundFile = UNRESOLVED.Copy(soundFile, len("music ") + 1, len(soundFile))
            } else {
                soundFile = UNRESOLVED.Copy(soundFile, len("musiconce ") + 1, len(soundFile))
            }
            soundFile = trim(soundFile)
            if (soundFile === "") {
                usruleeditorform.RuleEditorForm.MediaPlayer.close
                usruleeditorform.RuleEditorForm.MediaPlayer.fileName = ""
                return
            }
            if (UNRESOLVED.pos(".", soundFile) <= 0) {
                soundFile = soundFile + ".mid"
            }
            soundFileWithPath = soundFile
            if (UNRESOLVED.pos(":", soundFileWithPath) <= 0) {
                soundFileWithPath = findFileRecursivelyInMediaDirectories(soundFile, usdomain.domain.options.extraMediaDirectory)
            }
            if ((soundFileWithPath !== "") && (UNRESOLVED.pos(":", soundFileWithPath) <= 0)) {
                soundFileWithPath = UNRESOLVED.GetCurrentDir + "\\" + soundFileWithPath
            }
            if (FileExists(soundFileWithPath)) {
                if (usruleeditorform.RuleEditorForm.MediaPlayer.fileName === soundFileWithPath) {
                    if (usruleeditorform.RuleEditorForm.MediaPlayer.mode === UNRESOLVED.mpPlaying) {
                        // already playing
                        return
                    }
                }
                usruleeditorform.RuleEditorForm.MediaPlayer.fileName = soundFileWithPath
                //if not mediaPlayerOpened then
                usruleeditorform.RuleEditorForm.MediaPlayer.open
                //mediaPlayerOpened := true;
                //RuleEditorForm.mediaPlayer.wait := musicwait;
                usruleeditorform.RuleEditorForm.MediaPlayer.Notify = !musiconce
                usruleeditorform.RuleEditorForm.loopMusic = true
                usruleeditorform.RuleEditorForm.MediaPlayer.play
            } else {
                usruleeditorform.RuleEditorForm.MediaPlayer.close
            }
            return
        }
        if (!this.usingSpeech) {
            return
        }
        if (!usdomain.domain.options.playerPlaySounds) {
            return
        }
        if (UNRESOLVED.pos(usruleeditorform.kPlaySoundMacroStart, soundFile) === 1) {
            // if pos('run ', soundFile) = 1 then
            //    begin
            //    soundFile := Copy(soundFile, length('run ') + 1, length(soundFile));
            //    WinExec(pchar(soundFile), SW_SHOWNORMAL);
            //    exit;
            //    end;
            soundFile = UNRESOLVED.Copy(soundFile, len(usruleeditorform.kPlaySoundMacroStart) + 1, len(soundFile))
        } else if (soundFile === "sound") {
            soundFile = ""
        }
        soundFile = trim(soundFile)
        if ((len(soundFile) > 0) && (UNRESOLVED.pos(".", soundFile) <= 0)) {
            soundFile = soundFile + ".wav"
        }
        soundFileWithPath = soundFile
        if (UNRESOLVED.pos(":", soundFileWithPath) <= 0) {
            soundFileWithPath = findFileRecursivelyInMediaDirectories(soundFile, usdomain.domain.options.extraMediaDirectory)
        }
        if ((soundFileWithPath !== "") && (UNRESOLVED.pos(":", soundFileWithPath) <= 0)) {
            soundFileWithPath = UNRESOLVED.GetCurrentDir + "\\" + soundFileWithPath
        }
        if (FileExists(soundFileWithPath)) {
            this.agent.Characters[agentName].Speak("", soundFileWithPath)
        }
    }
    
    showPicture(pictureName: string, reply: string): void {
        let pictureFile: string
        let pictureFileWithPath: string
        
        pictureFile = UNRESOLVED.Copy(pictureName, len("picture ") + 1, len(pictureName))
        pictureFile = trim(pictureFile)
        if (pictureFile === "") {
            return
        }
        if (UNRESOLVED.pos(".", pictureFile) <= 0) {
            pictureFile = pictureFile + ".bmp"
        }
        pictureFileWithPath = pictureFile
        if (UNRESOLVED.pos(":", pictureFileWithPath) <= 0) {
            pictureFileWithPath = findFileRecursivelyInMediaDirectories(pictureFile, usdomain.domain.options.extraMediaDirectory)
        }
        if ((pictureFileWithPath !== "") && (UNRESOLVED.pos(":", pictureFileWithPath) <= 0)) {
            pictureFileWithPath = UNRESOLVED.GetCurrentDir + "\\" + pictureFileWithPath
        }
        if (FileExists(pictureFileWithPath)) {
            uspictureform.PictureForm.addPictureFromFile(pictureFileWithPath, reply)
        }
    }
    
    clearVoiceCommands(): void {
        if (!this.usingSpeech) {
            return
        }
        this.agent.Characters[agentName].Commands.RemoveAll
    }
    
    hideRiddleAnswerForCommand(aString: string): string {
        let result = ""
        if (UNRESOLVED.Pos("$", aString) === 1) {
            if (this.riddleIndex === 1) {
                result = firstRiddleAnswer
            } else if (this.riddleIndex === 2) {
                result = "say another answer for a riddle"
            } else {
                result = "say yet another answer for a riddle - number " + IntToStr(this.riddleIndex)
            }
            this.riddleIndex = this.riddleIndex + 1
        } else {
            result = aString
        }
        return result
    }
    
    listenForPhraseCaptionCommand(listenFor: string, commandCaption: string, command: string): void {
        if ((listenFor === "") || (command === "") || (commandCaption === "")) {
            return
        }
        if (this.usingSpeech) {
            this.agent.Characters[agentName].Commands.Add(commandCaption, commandCaption, listenFor, true, true)
        }
        this.commandsListenedFor.Add(command)
    }
    
    listenForCommand(aString: string): void {
        let listenFor: string
        let commandCaption: string
        
        if ((this.commandsListenedFor.IndexOf(aString) === -1)) {
            commandCaption = this.hideRiddleAnswerForCommand(aString)
            if (commandCaption !== aString) {
                listenFor = trim(UNRESOLVED.Copy(aString, 2, len(aString)))
            } else {
                listenFor = aString
            }
            this.listenForPhraseCaptionCommand(listenFor, commandCaption, aString)
        }
    }
    
    // setsa up speech system and also updates GUI list of commands
    listenForAvailableCommands(): void {
        let rule: TSRule
        let i: int
        let command: string
        
        this.clearVoiceCommands()
        this.commandsListenedFor.Clear()
        usconsoleform.ConsoleForm.VisibleCommandsList.Clear()
        this.riddleIndex = 1
        if (usdomain.domain.world.rules.Count <= 0) {
            return
        }
        this.listenForCommand(commandListCommand)
        this.listenForCommand(repeatLastSayCommand)
        if (usdomain.domain.options.useVoiceToUndo) {
            this.listenForCommand(undoCommand)
        }
        if (usdomain.domain.options.useVoiceToRedo) {
            this.listenForCommand(redoCommand)
        }
        for (i = 0; i <= usdomain.domain.world.rules.Count - 1; i++) {
            rule = usworld.TSRule(usdomain.domain.world.rules.Items[i])
            if (rule.available) {
                if (rule.command.phrase === "") {
                    continue
                }
                this.listenForCommand(rule.command.phrase)
                command = rule.command.phrase
                if ((UNRESOLVED.pos("$", rule.command.phrase) === 1)) {
                    // only list first riddle answer
                    command = firstRiddleAnswer
                }
                if (usconsoleform.ConsoleForm.VisibleCommandsList.Items.IndexOf(command) === -1) {
                    usconsoleform.ConsoleForm.VisibleCommandsList.Items.AddObject(command, rule)
                }
            }
        }
    }
    
}

