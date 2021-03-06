export interface TSOptions {
}

/*
export interface DomainOptionsStructure {
    extraMediaDirectory: string
    logFileName: string
    agentCharacterFileName: string
    mostRecentSession: string
    mostRecentWorld: string
    playerSpeak: boolean
    playerPlaySounds: boolean
    playerPlayMusic: boolean
    showTranscript: boolean
    showPictures: boolean
    sayOptionsAfterLook: boolean
    useVoiceToUndo: boolean
    useVoiceToRedo: boolean
    showVariables: boolean
    updateEditorAfterCommandDone: boolean
    playerFontSize: short
    playerFontName: string
    suppressAgentNotPresentWarning: boolean
    selectedItemColor: TColor
    selectedTextColor: TColor
    commandTextColorInMap: TColor
    showCommandsInMap: boolean
    showCommandPrefixInMap: boolean
    tableFontName: string
    mapFontName: string
    browserFontName: string
    tableFontSize: short
    mapFontSize: short
    browserFontSize: short
    showRuleEditor: boolean
    showButtonBar: boolean
    browseBy: short
    pageShowing: short
    buttonSymbols: boolean
    consoleWindowRect: TRect
    editorWindowRect: TRect
    logFileWindowRect: TRect
    consoleBottomHeight: int
    consoleRightWidth: int
    editorPanelEditorHeight: int
    editorPanelRequirementsChangesHeight: int
    editorPanelFirstListWidth: int
    pictureWindowRect: TRect
}

// const
const kDefaultLogFileName = "StoryHarp.log"
const kDefaultIniFileName = "StoryHarp.ini"

readIniFile(): void {
    let iniFileFound: boolean
    
    if (!this.useIniFile) {
        this.defaultOptions()
        return
    }
    if (ExtractFilePath(this.iniFileName) !== "") {
        // ini file name has path
        // alternate ini file must exist before user uses it
        iniFileFound = FileExists(this.iniFileName)
        if (!iniFileFound) {
            ShowMessage("Could not find alternate settings file " + chr(13) + chr(13) + "  " + this.iniFileName + chr(13) + chr(13) + "Using standard settings file in Windows directory instead.")
            this.iniFileName = kDefaultIniFileName
            iniFileFound = FileExists(this.windowsDirectory() + "\\" + this.iniFileName)
            this.iniFileName = this.windowsDirectory() + "\\" + this.iniFileName
        }
    } else {
        iniFileFound = FileExists(this.windowsDirectory() + "\\" + this.iniFileName)
        this.iniFileName = this.windowsDirectory() + "\\" + this.iniFileName
    }
    if (iniFileFound && this.useIniFile) {
        this.getProfileInformation()
    } else {
        this.defaultOptions()
    }
}

storeProfileInformation(): void {
    let section: string
    let iniFile: TIniFile
    let saveNumber: float
    
    iniFile = UNRESOLVED.TIniFile.create(this.iniFileName)
    try {
        //FIX unresolved WITH expression: iniFile
        // files
        section = "Files"
        UNRESOLVED.writeString(section, "Log file", this.options.logFileName)
        UNRESOLVED.writeString(section, "Agent character file", this.options.agentCharacterFileName)
        UNRESOLVED.writeString(section, "Extra media directory", this.options.extraMediaDirectory)
        UNRESOLVED.writeString(section, "Most recent session", this.options.mostRecentSession)
        UNRESOLVED.writeString(section, "Most recent world", this.options.mostRecentWorld)
        // player options
        section = "Player"
        UNRESOLVED.writeString(section, "Speak", ufilesupport.boolToStr(this.options.playerSpeak))
        UNRESOLVED.writeString(section, "Play sounds", ufilesupport.boolToStr(this.options.playerPlaySounds))
        UNRESOLVED.writeString(section, "Play music", ufilesupport.boolToStr(this.options.playerPlayMusic))
        UNRESOLVED.writeString(section, "Show transcript", ufilesupport.boolToStr(this.options.showTranscript))
        UNRESOLVED.writeString(section, "Show pictures", ufilesupport.boolToStr(this.options.showPictures))
        UNRESOLVED.writeString(section, "Say options after look", ufilesupport.boolToStr(this.options.sayOptionsAfterLook))
        UNRESOLVED.writeString(section, "Use voice to undo", ufilesupport.boolToStr(this.options.useVoiceToUndo))
        UNRESOLVED.writeString(section, "Use voice to redo", ufilesupport.boolToStr(this.options.useVoiceToRedo))
        UNRESOLVED.writeString(section, "Show variables", ufilesupport.boolToStr(this.options.showVariables))
        UNRESOLVED.writeString(section, "Update editor after command done", ufilesupport.boolToStr(this.options.updateEditorAfterCommandDone))
        UNRESOLVED.writeString(section, "Suppress agent not present warning", ufilesupport.boolToStr(this.options.suppressAgentNotPresentWarning))
        UNRESOLVED.writeString(section, "Player font size", IntToStr(this.options.playerFontSize))
        UNRESOLVED.writeString(section, "Player font name", this.options.playerFontName)
        // editor options
        section = "Editor"
        UNRESOLVED.writeString(section, "Background color for selected items", IntToStr(this.options.selectedItemColor))
        UNRESOLVED.writeString(section, "Text color for selected items", IntToStr(this.options.selectedTextColor))
        UNRESOLVED.writeString(section, "Text color for commands in map", IntToStr(this.options.commandTextColorInMap))
        UNRESOLVED.writeString(section, "Show commands in map", ufilesupport.boolToStr(this.options.showCommandsInMap))
        UNRESOLVED.writeString(section, "Show command prefix in map", ufilesupport.boolToStr(this.options.showCommandPrefixInMap))
        UNRESOLVED.writeString(section, "Table font name", this.options.tableFontName)
        UNRESOLVED.writeString(section, "Table font size", IntToStr(this.options.tableFontSize))
        UNRESOLVED.writeString(section, "Map font name", this.options.mapFontName)
        UNRESOLVED.writeString(section, "Map font size", IntToStr(this.options.mapFontSize))
        UNRESOLVED.writeString(section, "Browser font name", this.options.browserFontName)
        UNRESOLVED.writeString(section, "Browser font size", IntToStr(this.options.browserFontSize))
        UNRESOLVED.writeString(section, "Show rule editor", ufilesupport.boolToStr(this.options.showRuleEditor))
        UNRESOLVED.writeString(section, "Show button bar", ufilesupport.boolToStr(this.options.showButtonBar))
        UNRESOLVED.writeString(section, "Browse by (context, rule, reply, move, requirements, changes)", IntToStr(this.options.browseBy))
        UNRESOLVED.writeString(section, "Page showing (table, map, browser)", IntToStr(this.options.pageShowing))
        UNRESOLVED.writeString(section, "Button symbols", ufilesupport.boolToStr(this.options.buttonSymbols))

        // windows
        section = "Windows"
        UNRESOLVED.writeString(section, "Player window position", ufilesupport.rectToString(this.options.consoleWindowRect))
        UNRESOLVED.writeString(section, "Editor window position", ufilesupport.rectToString(this.options.editorWindowRect))
        UNRESOLVED.writeString(section, "Log file window position", ufilesupport.rectToString(this.options.logFileWindowRect))
        UNRESOLVED.writeString(section, "Player horizontal splitter", IntToStr(this.options.consoleBottomHeight))
        UNRESOLVED.writeString(section, "Player vertical splitter", IntToStr(this.options.consoleRightWidth))
        UNRESOLVED.writeString(section, "Editor top splitter", IntToStr(this.options.editorPanelEditorHeight))
        UNRESOLVED.writeString(section, "Editor bottom splitter", IntToStr(this.options.editorPanelRequirementsChangesHeight))
        UNRESOLVED.writeString(section, "Editor browser splitter", IntToStr(this.options.editorPanelFirstListWidth))
        UNRESOLVED.writeString(section, "Picture window position", ufilesupport.rectToString(this.options.pictureWindowRect))
    } finally {
        iniFile.free
    }
}

getProfileInformation(): void {
    let section: string
    let iniFile: TIniFile
    let timeString: string
    let readNumber: float
    
    iniFile = UNRESOLVED.TIniFile.create(this.iniFileName)
    try {
        //FIX unresolved WITH expression: iniFile
        // files
        section = "Files"
        this.options.logFileName = UNRESOLVED.readString(section, "Log file", ExtractFilePath(delphi_compatability.Application.exeName) + kDefaultLogFileName)
        this.options.agentCharacterFileName = UNRESOLVED.readString(section, "Agent character file", ExtractFilePath(delphi_compatability.Application.exeName) + kDefaultAgentCharacterFileName)
        this.options.extraMediaDirectory = UNRESOLVED.readString(section, "Extra media directory", "")
        this.options.mostRecentSession = UNRESOLVED.readString(section, "Most recent session", "")
        this.options.mostRecentWorld = UNRESOLVED.readString(section, "Most recent world", "")
        // player options
        section = "Player"
        this.options.playerSpeak = ufilesupport.strToBool(UNRESOLVED.readString(section, "Speak", "true"))
        this.options.playerPlaySounds = ufilesupport.strToBool(UNRESOLVED.readString(section, "Play sounds", "true"))
        this.options.playerPlayMusic = ufilesupport.strToBool(UNRESOLVED.readString(section, "Play music", "true"))
        this.options.showTranscript = ufilesupport.strToBool(UNRESOLVED.readString(section, "Show transcript", "true"))
        this.options.showPictures = ufilesupport.strToBool(UNRESOLVED.readString(section, "Show pictures", "true"))
        this.options.sayOptionsAfterLook = ufilesupport.strToBool(UNRESOLVED.readString(section, "Say options after look", "true"))
        this.options.useVoiceToUndo = ufilesupport.strToBool(UNRESOLVED.readString(section, "Use voice to undo", "false"))
        this.options.useVoiceToRedo = ufilesupport.strToBool(UNRESOLVED.readString(section, "Use voice to redo", "false"))
        this.options.showVariables = ufilesupport.strToBool(UNRESOLVED.readString(section, "Show variables", "false"))
        this.options.updateEditorAfterCommandDone = ufilesupport.strToBool(UNRESOLVED.readString(section, "Update editor after command done", "true"))
        this.options.suppressAgentNotPresentWarning = ufilesupport.strToBool(UNRESOLVED.readString(section, "Suppress agent not present warning", "false"))
        this.options.playerFontSize = StrToInt(UNRESOLVED.readString(section, "Player font size", "8"))
        this.options.playerFontName = UNRESOLVED.readString(section, "Player font name", "Arial")
        // editor options
        section = "Editor"
        this.options.selectedItemColor = StrToInt(UNRESOLVED.readString(section, "Background color for selected items", IntToStr(delphi_compatability.clYellow)))
        this.options.selectedTextColor = StrToInt(UNRESOLVED.readString(section, "Text color for selected items", IntToStr(delphi_compatability.clBlack)))
        this.options.commandTextColorInMap = StrToInt(UNRESOLVED.readString(section, "Text color for commands in map", IntToStr(delphi_compatability.clBlue)))
        this.options.showCommandsInMap = ufilesupport.strToBool(UNRESOLVED.readString(section, "Show commands in map", "true"))
        this.options.showCommandPrefixInMap = ufilesupport.strToBool(UNRESOLVED.readString(section, "Show command prefix in map", "false"))
        this.options.tableFontName = UNRESOLVED.readString(section, "Table font name", "Arial")
        this.options.tableFontSize = StrToInt(UNRESOLVED.readString(section, "Table font size", "8"))
        this.options.mapFontName = UNRESOLVED.readString(section, "Map font name", "Arial")
        this.options.mapFontSize = StrToInt(UNRESOLVED.readString(section, "Map font size", "8"))
        this.options.browserFontName = UNRESOLVED.readString(section, "Browser font name", "Arial")
        this.options.browserFontSize = StrToInt(UNRESOLVED.readString(section, "Browser font size", "8"))
        this.options.showRuleEditor = ufilesupport.strToBool(UNRESOLVED.readString(section, "Show rule editor", "true"))
        this.options.showButtonBar = ufilesupport.strToBool(UNRESOLVED.readString(section, "Show button bar", "true"))
        this.options.browseBy = StrToInt(UNRESOLVED.readString(section, "Browse by (context, rule, reply, move, requirements, changes)", "0"))
        this.options.pageShowing = StrToInt(UNRESOLVED.readString(section, "Page showing (table, map, browser)", "0"))
        this.options.buttonSymbols = ufilesupport.strToBool(UNRESOLVED.readString(section, "Button symbols", "true"))
        // windows
        section = "Windows"
        this.options.consoleWindowRect = ufilesupport.stringToRect(UNRESOLVED.readString(section, "Player window position", "150 20 200 420"))
        this.options.editorWindowRect = ufilesupport.stringToRect(UNRESOLVED.readString(section, "Editor window position", "40 30 560 400"))
        this.options.logFileWindowRect = ufilesupport.stringToRect(UNRESOLVED.readString(section, "Log file window position", "50 50 550 400"))
        this.options.consoleBottomHeight = StrToInt(UNRESOLVED.readString(section, "Player horizontal splitter", "200"))
        this.options.consoleRightWidth = StrToInt(UNRESOLVED.readString(section, "Player vertical splitter", "200"))
        this.options.editorPanelEditorHeight = StrToInt(UNRESOLVED.readString(section, "Editor top splitter", "150"))
        this.options.editorPanelRequirementsChangesHeight = StrToInt(UNRESOLVED.readString(section, "Editor bottom splitter", "100"))
        this.options.editorPanelFirstListWidth = StrToInt(UNRESOLVED.readString(section, "Editor browser splitter", "200"))
        this.options.pictureWindowRect = ufilesupport.stringToRect(UNRESOLVED.readString(section, "Picture window position", "200 200 200 200"))
    } finally {
        iniFile.free
    }
}

defaultOptions(): void {
    // this is for first-time use, when there is no ini file
    // files
    this.options.logFileName = ExtractFilePath(delphi_compatability.Application.exeName) + kDefaultLogFileName
    this.options.agentCharacterFileName = ExtractFilePath(delphi_compatability.Application.exeName) + kDefaultAgentCharacterFileName
    this.options.extraMediaDirectory = ""
    this.options.mostRecentSession = ""
    this.options.mostRecentWorld = ""
    // player options
    this.options.playerSpeak = true
    this.options.playerPlaySounds = true
    this.options.playerPlayMusic = true
    this.options.showTranscript = true
    this.options.showPictures = true
    this.options.sayOptionsAfterLook = true
    this.options.useVoiceToUndo = false
    this.options.useVoiceToRedo = false
    this.options.showVariables = false
    this.options.updateEditorAfterCommandDone = true
    this.options.suppressAgentNotPresentWarning = false
    this.options.playerFontSize = 8
    this.options.playerFontName = "Arial"
    // editor options
    this.options.selectedItemColor = delphi_compatability.clYellow
    this.options.selectedTextColor = delphi_compatability.clBlack
    this.options.commandTextColorInMap = delphi_compatability.clBlue
    this.options.showCommandsInMap = true
    this.options.showCommandPrefixInMap = false
    this.options.tableFontName = "Arial"
    this.options.tableFontSize = 8
    this.options.mapFontName = "Arial"
    this.options.mapFontSize = 8
    this.options.browserFontName = "Arial"
    this.options.browserFontSize = 8
    this.options.showRuleEditor = true
    this.options.showButtonBar = true
    this.options.browseBy = usworld.kRuleContext
    this.options.pageShowing = 0
    this.options.buttonSymbols = false
    // windows
    this.options.consoleWindowRect = Rect(150, 20, 200, 420)
    this.options.editorWindowRect = Rect(40, 30, 560, 400)
    this.options.logFileWindowRect = Rect(50, 50, 550, 400)
    this.options.consoleBottomHeight = 200
    this.options.consoleRightWidth = 200
    this.options.editorPanelEditorHeight = 150
    this.options.editorPanelRequirementsChangesHeight = 100
    this.options.editorPanelFirstListWidth = 200
    this.options.pictureWindowRect = Rect(200, 200, 200, 200)
}

loadIni(section: string, theField: string, theDefault: string): string {
    let result = ""
    let iniFile: TIniFile
    
    iniFile = UNRESOLVED.TIniFile.create(this.iniFileName)
    try {
        result = iniFile.ReadString(section, theField, theDefault)
    } finally {
        iniFile.free
    }
    return result
}

*/
