import * as m from "mithril"
import { TWorld } from "./TWorld"
import { TSCommandList } from "./TSCommandList"
import { TSRule, TSRuleField } from "./TSRule"
import { Color, ScrollIntoViewDirection, int } from "./common"
import { TSDraggableObject } from "./TSDraggableObject"

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

*/

// const
const kUnsavedWorldFileName = "untitled"
const kUnsavedSessionFileName = "untitled"
const kWorldExtension = "wld"
const kSessionExtension = "ses"

export interface TranscriptLine {
    text: string
    color: Color
}

export interface DemoEntry {
    name: string
    description: string
}

export interface DemoConfig {
    demoWorldFiles: DemoEntry[]
}

export interface ConsoleFormAPI {
    addLineToTranscript: (text: string, color: Color) => void
    scrollTranscriptEndIntoView: () => void
}

export interface RuleEditorAPI {
    scrollGridSelectionsIntoView: (direction: ScrollIntoViewDirection) => void
    selectEditorField: (field: TSRuleField) => void
    lastChoice: TSDraggableObject | null
    lastCommand: TSRule | null
}

export interface ChangeLogAPI {
    addToLog: (text: string) => void
}

export interface SpeechSystemAPI {
    lastSaidTextWithMacros: string
    speakText: (text: string) => void
    sayTextWithMacros: (text: string) => void
    checkForSayOptionsMacro: () => void
    listenForAvailableCommands: () => void
    stripMacros: (textWithMacros: string) => string
}

// This is a seperate interface for testability
export interface TSDomain {
    world: TWorld
    sessionCommandList: TSCommandList
    worldCommandList: TSCommandList

    sessionChangeCount: int
    worldChangeCount: int
    isWorldFileLoaded: boolean

    updateForNewOrLoadedWorld(fileName: string, isWorldFileLoaded: boolean): void;

    editedRule: TSRule | null
    lastSingleRuleIndex: number

    currentEditorView: string
    currentEditorWizard: string

    setOrganizeByField: (newValue: TSRuleField) => null

    transcript: TranscriptLine[]

    worldFileName: string
    sessionFileName: string

    demoConfig: DemoConfig

    loadTestWorld: (name: string) => Promise<void>

    showCommandPrefixInMap: boolean

    newSession: () => void

    consoleForm: ConsoleFormAPI
    ruleEditorForm: RuleEditorAPI
    changeLogForm: ChangeLogAPI
    speechSystem: SpeechSystemAPI
}

export class TSApplication implements TSDomain {
    world: TWorld
    sessionCommandList: TSCommandList
    worldCommandList: TSCommandList

    sessionChangeCount = 0
    worldChangeCount = 0
    isWorldFileLoaded = false

    editedRule: TSRule | null = null
    lastSingleRuleIndex = 0

    currentEditorView = "table"
    currentEditorWizard = "context"

    setOrganizeByField: (newValue: TSRuleField) => null

    transcript: TranscriptLine[] = []

    worldFileName = ""
    sessionFileName = ""

    demoConfig: DemoConfig

    showCommandPrefixInMap = false

    dataPath = "./data/"

    // TODO: Fix these
    consoleForm: ConsoleFormAPI
    ruleEditorForm: RuleEditorAPI
    changeLogForm: ChangeLogAPI
    speechSystem: SpeechSystemAPI

    constructor() {
        this.world = new TWorld()
        this.sessionCommandList = new TSCommandList(this)
        this.worldCommandList = new TSCommandList(this)

        // TODO: Fix these
        this.consoleForm = {
            addLineToTranscript: (text: string, color: number) => this.transcript.push({text, color}),
            scrollTranscriptEndIntoView: () => null,
        }

        this.ruleEditorForm = {
            selectEditorField: (fieldIndex: number) => null,
            scrollGridSelectionsIntoView: () => null,
            lastChoice: null,
            lastCommand: null
        }

        this.changeLogForm = {
            addToLog: (text: string) => null
        }

        this.speechSystem = {
            lastSaidTextWithMacros: "TEST",
            stripMacros: (text: string) => text,
            sayTextWithMacros: (text: string) => null,
            listenForAvailableCommands: () => null,
            checkForSayOptionsMacro: () => null,
            speakText: (text: string) => null,
        }
    }

    // TODO: consolidate world loading
    async loadTestWorld(fileName: string) {
        if (!this.demoConfig) {
            this.demoConfig = <DemoConfig>await m.request(this.dataPath + "demoConfig.json")
                .catch(error => {
                    console.log("error loading demoConfig.json", error)
                    alert("Something went wrong loading demoConfig.json from the server")
                    const result: DemoConfig = { demoWorldFiles: [] }
                    return result
                })
        }
    
        const worldContent = await m.request(this.dataPath + fileName + ".wld", {deserialize: (text) => text})
            .catch(error => {
                console.log("error loading a world file", fileName, error)
                alert("Something went wrong loading the world file \"" + fileName + "\" from the server")
                return ""
            })

        if (!worldContent) return
    
        this.world.resetVariablesAndRules()
        const loaded = this.world.loadWorldFromFileContents(worldContent)
        if (!loaded) throw new Error("Failed to load")

        this.updateForNewOrLoadedWorld(fileName, true)

        /* Thinking about running first rule on startup -- but this is not good enough in any case:
        if (domain.world.rules.length) {
            domain.transcript.push({text: "> " + domain.world.rules[0].command.phrase, color: Color.clBlue})
            domain.transcript.push({text: domain.world.rules[0].reply, color: Color.clBlack})
        }
        */
        m.redraw()
    }

    /*
    mapView: TSMapView = new TSMapView()
    options: DomainOptionsStructure = new DomainOptionsStructure()
    iniFileName: string = ""
    sessionOrWorldStartupFileName: string = ""
    playerOnly: boolean = false
    useIniFile: boolean = false

    startTimeThisSession: TDateTime = new TDateTime()
    
    create(): void {
        TObject.prototype.create.call(this)
        this.world = usworld.TWorld().Create()
        this.sessionCommandList = uscommands.TSCommandList().create()
        this.sessionCommandList.setNewUndoLimit(1000)
        this.worldCommandList = uscommands.TSCommandList().create()
        this.worldCommandList.setNewUndoLimit(1000)
        this.worldFileName = kUnsavedWorldFileName + "." + kWorldExtension
        this.sessionFileName = kUnsavedSessionFileName + "." + kSessionExtension
        this.sessionChangeCount = 0
        this.worldChangeCount = 0
        this.isWorldFileLoaded = false
        this.mapView = usmapview.TSMapView.create
        this.sessionOrWorldStartupFileName = ""
        this.playerOnly = false
        this.useIniFile = true
        this.iniFileName = kDefaultIniFileName
        this.readCommandLine()
        this.readIniFile()
        this.startTimeThisSession = UNRESOLVED.Now
    }
    
    readCommandLine(): void {
        let i: int
        
        if (UNRESOLVED.ParamCount > 0) {
            for (i = 1; i <= UNRESOLVED.ParamCount; i++) {
                if (uppercase(UNRESOLVED.ParamStr(i)) === "/I=") {
                    this.useIniFile = false
                } else if (uppercase(UNRESOLVED.ParamStr(i)) === "/I") {
                    this.useIniFile = false
                } else if (UNRESOLVED.pos("/I=", uppercase(UNRESOLVED.ParamStr(i))) === 1) {
                    this.iniFileName = UNRESOLVED.copy(UNRESOLVED.ParamStr(i), 4, len(UNRESOLVED.ParamStr(i)))
                } else if (UNRESOLVED.pos("/I", uppercase(UNRESOLVED.ParamStr(i))) === 1) {
                    this.iniFileName = UNRESOLVED.copy(UNRESOLVED.ParamStr(i), 3, len(UNRESOLVED.ParamStr(i)))
                } else if (UNRESOLVED.pos("/P", uppercase(UNRESOLVED.ParamStr(i))) === 1) {
                    this.playerOnly = true
                } else if ((this.sessionOrWorldStartupFileName === "") && (UNRESOLVED.pos("/", uppercase(UNRESOLVED.ParamStr(i))) !== 1)) {
                    this.sessionOrWorldStartupFileName = UNRESOLVED.ParamStr(i)
                } else {
                    ShowMessage("Improper parameter string " + UNRESOLVED.ParamStr(i))
                }
            }
        }
    }
    
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
    
    loadFileAtStartupAndInitializeForms(): void {
        let Year: byte
        let Month: byte
        let Day: byte
        
        if (this.sessionOrWorldStartupFileName !== "") {
            usconsoleform.ConsoleForm.openSessionOrWorldFile(UNRESOLVED.ParamStr(1))
        } else {
            if ((this.options.mostRecentSession !== "") && (FileExists(this.options.mostRecentSession))) {
                usconsoleform.ConsoleForm.openSessionOrWorldFile(this.options.mostRecentSession)
            } else if ((this.options.mostRecentWorld !== "") && (FileExists(this.options.mostRecentWorld))) {
                usconsoleform.ConsoleForm.openSessionOrWorldFile(this.options.mostRecentWorld)
            }
        }
        if (this.playerOnly) {
            usconsoleform.ConsoleForm.playerOnly()
        }
        usconsoleform.ConsoleForm.updateForRegistrationChange()
        usconsoleform.ConsoleForm.updateTitles()
        usruleeditorform.RuleEditorForm.MapPaintBoxChanged()
        usruleeditorform.RuleEditorForm.updateViews()
        usruleeditorform.RuleEditorForm.editRule(null)
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

    updateForNewOrLoadedWorld(fileName: string, isWorldFileLoaded: boolean): void {
        this.worldCommandList.clear()
        if (fileName) {
            this.worldFileName = fileName
        } else {
            this.worldFileName = kUnsavedWorldFileName + "." + kWorldExtension
        }
        this.worldChangeCount = 0
        this.isWorldFileLoaded = isWorldFileLoaded

        this.editedRule = null
        this.lastSingleRuleIndex = 0

        this.newSession()
    }

    newSession(): void {
        this.world.newSession()
        this.sessionCommandList.clear()
        this.transcript.length = 0
        this.transcript.push({text: "Starting: " + this.worldFileName, color: Color.clGreen})

        this.sessionFileName = kUnsavedSessionFileName + "." + kSessionExtension
        this.sessionChangeCount = 0
    }

    /*
    
    loadSession(fileName: string): void {
        this.sessionCommandList.clear()
        this.sessionFileName = fileName
        this.sessionChangeCount = 0
        if (this.world.loadSessionFromFile(fileName, ExtractFileName(this.worldFileName))) {
            this.options.mostRecentWorld = this.worldFileName
            this.options.mostRecentSession = fileName
        }
    }
    
    saveSession(fileName: string): void {
        this.world.saveSessionToFile(fileName, ExtractFileName(this.worldFileName))
        this.options.mostRecentWorld = this.worldFileName
    }
    
    isSessionFileChanged(): boolean {
        return this.sessionChangeCount !== 0
    }
    
    loadWorld(fileName: string): void {
        this.sessionCommandList.clear()
        this.sessionFileName = kUnsavedSessionFileName + "." + kSessionExtension
        this.sessionChangeCount = 0
        this.worldCommandList.clear()
        this.worldFileName = fileName
        this.worldChangeCount = 0
        this.world.resetVariablesAndRules()
        this.world.focus = null
        this.world.previousFocus = null
        usruleeditorform.RuleEditorForm.lastChoice = null
        usruleeditorform.RuleEditorForm.previousChoice = null
        if (this.world.loadWorldFromFile(fileName)) {
            this.isWorldFileLoaded = true
            this.options.mostRecentWorld = this.worldFileName
            this.options.mostRecentSession = ""
        }
    }
    
    mergeWorld(fileName: string): void {
        // don't clear things
        this.world.loadWorldFromFile(fileName)
        this.worldChangeCount += 1
    }
    
    saveWorld(fileName: string): void {
        this.world.saveWorldToFile(fileName, usworld.kSaveAllRules)
    }
    
    isWorldFileChanged(): boolean {
        return this.worldChangeCount !== 0
    }
    
    resetWorldChangeCount(): void {
        this.worldChangeCount = 0
    }
    
    worldChangeDone(): void {
        this.worldChangeCount += 1
        usruleeditorform.RuleEditorForm.updateMenus()
    }
    
    worldChangeUndone(): void {
        this.worldChangeCount -= 1
        usruleeditorform.RuleEditorForm.updateMenus()
    }
    
    */
}

