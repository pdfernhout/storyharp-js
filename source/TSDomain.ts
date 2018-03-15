import * as m from "mithril"
import { TWorld } from "./TWorld"
import { TSCommandList } from "./TSCommandList"
import { TSRule, TSRuleField } from "./TSRule"
import { Color, ScrollIntoViewDirection, int, makeFileNameWithoutWldExtension } from "./common"
import { TSDraggableObject } from "./TSDraggableObject"
import { KfCommandChangeType, KfCommand } from "./KfCommand"
import { LinkWizardData, newLinkWizardData } from "./LinkWizardView"
import { ContextWizardData, newContextWizardData } from "./ContextWizardView"
import { CommandWizardData, newCommandWizardData } from "./CommandWizardView"
import { MapViewState, newMapViewState } from "./TSMapView"
import { doCommand } from "./ConsoleForm";
import { PendingTableScroll } from "./RuleTableView"
import { TPoint } from "./TPoint"

// const
const kUnsavedWorldFileName = "untitled"
const kUnsavedSessionFileName = "untitled"
const kWorldExtension = "wld"
const kSessionExtension = "ses"

export interface TranscriptLine {
    uuid: number
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
    doCommand: (domain: TSDomain, commandPhrase: string) => void
}

export interface RuleEditorAPI {
    scrollGridSelectionsIntoView: (direction: ScrollIntoViewDirection) => void
    selectEditorField: (field: TSRuleField) => void
    lastChoice: TSDraggableObject | null
    previousChoice: TSDraggableObject | null
    // TODO: Use lastCommand to track current rule from console to editor
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

export type FormName = "about" | "console" | "files" | "ruleEditor"
export type EditorName = "table" | "map" | "browser" | "wizards"
export type WizardName = "context" | "command" | "link"

// This is a seperate interface for testability
export interface TSDomain {
    world: TWorld
    sessionCommandList: TSCommandList
    worldCommandList: TSCommandList

    sessionChangeCount: int
    worldChangeCount: int
    isWorldFileLoaded: boolean

    updateForNewOrLoadedWorld(fileName: string, isWorldFileLoaded: boolean): void
    isWorldFileChanged(): boolean
    resetWorldChangeCount(): void

    editedRule: TSRule | null
    lastSingleRuleIndex: number

    activeForm: FormName
    currentEditorView: EditorName
    currentEditorWizard: WizardName

    editRule: (rule: TSRule | null, scrollDirection?: ScrollIntoViewDirection) => void

    browseBy: TSRuleField
    setOrganizeByField: (newValue: TSRuleField) => void

    transcript: TranscriptLine[]

    worldFileName: string
    sessionFileName: string

    demoConfig: DemoConfig

    loadWorldFromServerData: (name: string) => Promise<boolean>

    showCommandPrefixInMap: boolean

    newSession: () => void

    linkWizardData: LinkWizardData
    contextWizardData: ContextWizardData
    commandWizardData: CommandWizardData
    showWizardHelp: boolean

    mapViewState: MapViewState

    pendingTableScroll: PendingTableScroll | null
    pendingMapScroll: boolean
    pendingBrowserScroll: boolean

    dataPath: string

    consoleForm: ConsoleFormAPI
    ruleEditorForm: RuleEditorAPI
    changeLogForm: ChangeLogAPI
    speechSystem: SpeechSystemAPI
}

// Transcript lines are given UUIDs to pss as keys to Mithril -- they are only unique per application run
let nextTranscriptLineUUID = 1

export class TSApplication implements TSDomain {
    world: TWorld
    sessionCommandList: TSCommandList
    worldCommandList: TSCommandList

    sessionChangeCount = 0
    worldChangeCount = 0
    isWorldFileLoaded = false

    editedRule: TSRule | null = null
    lastSingleRuleIndex = 0

    activeForm: FormName = "console"
    currentEditorView: EditorName = "table"
    currentEditorWizard: WizardName = "context"

    editRule(rule: TSRule | null, scrollDirection: ScrollIntoViewDirection = ScrollIntoViewDirection.kFromTop) {
        this.editedRule = rule
        if (rule) {
            // TODO: Improve scrolling behavior
            // Don't scroll for table and browser forms because scrolling shifts
            // even if item visible -- which is jumpy if click on rule.
            // But this means undo/redo commands don't track properly.

            if (this.currentEditorView !== "table") {
                this.pendingTableScroll = {
                    rule: rule,
                    direction: scrollDirection,
                }
            }

            if (this.currentEditorView !== "browser") {
                this.pendingBrowserScroll = true
            }

            // the map does not scroll if item is visible
            this.pendingMapScroll = true
        }
    }

    setOrganizeByField(newValue: TSRuleField) {
        this.browseBy = newValue
        this.pendingBrowserScroll = true
    }

    transcript: TranscriptLine[] = []

    worldFileName = kUnsavedWorldFileName + "." + kWorldExtension
    sessionFileName = kUnsavedSessionFileName + "." + kSessionExtension

    demoConfig: DemoConfig

    showCommandPrefixInMap = false

    linkWizardData: LinkWizardData
    contextWizardData: ContextWizardData
    commandWizardData: CommandWizardData
    showWizardHelp = true

    mapViewState: MapViewState

    browseBy = TSRuleField.kRuleContext

    pendingTableScroll: PendingTableScroll | null = null
    pendingMapScroll: boolean = false
    pendingBrowserScroll: boolean = false

    dataPath = "./data/"

    // TODO: Rearchitect these so not called "Form"; rethink how domain is used to transfer this state
    consoleForm: ConsoleFormAPI
    ruleEditorForm: RuleEditorAPI
    changeLogForm: ChangeLogAPI
    speechSystem: SpeechSystemAPI

    constructor() {
        this.world = new TWorld(this.goodPosition.bind(this))

        this.sessionCommandList = new TSCommandList(this)
        this.sessionCommandList.setNewUndoLimit(1000)

        this.worldCommandList = new TSCommandList(this)
        this.worldCommandList.setNewUndoLimit(1000)
        this.worldCommandList.notifyProcedure = this.commandChangedNotification.bind(this)

        this.linkWizardData = newLinkWizardData()
        this.contextWizardData = newContextWizardData()
        this.commandWizardData = newCommandWizardData()

        this.mapViewState = newMapViewState()

        // TODO: Rearchitect these so not called "Form"; rethink how domain is used to transfer this state
        this.consoleForm = {
            addLineToTranscript: (text: string, color: number) => this.transcript.push({uuid: nextTranscriptLineUUID++, text, color}),
            scrollTranscriptEndIntoView: () => null,
            doCommand: doCommand
        }

        this.ruleEditorForm = {
            selectEditorField: (fieldIndex: number) => null,
            scrollGridSelectionsIntoView: (direction: ScrollIntoViewDirection) => {
                this.pendingTableScroll = {
                    rule: null,
                    direction,
                }
            },
            lastChoice: null,
            previousChoice: null,            
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

    async loadWorldFromServerData(fileName: string) {
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

        if (!worldContent) return false
    
        this.world.resetVariablesAndRules()
        const loaded = this.world.loadWorldFromFileContents(worldContent)
        if (!loaded) throw new Error("Failed to load")

        this.updateForNewOrLoadedWorld(fileName, true)
        m.redraw()

        return true
    }

    goodPosition(): TPoint {
        let result = new TPoint()
        if (this.ruleEditorForm.lastChoice !== null) {
            if (this.ruleEditorForm.previousChoice !== null) {
                //var
                //  	mapBoundsRect: TRect
                //    selection: TSDraggableObject; 
                result = new TPoint(
                    (this.ruleEditorForm.previousChoice.position.X + this.ruleEditorForm.lastChoice.position.X) / 2,
                    (this.ruleEditorForm.previousChoice.position.Y + this.ruleEditorForm.lastChoice.position.Y) / 2 + 30)
            } else {
                result = new TPoint(this.ruleEditorForm.lastChoice.position.X, this.ruleEditorForm.lastChoice.position.Y + 30)
            }
        } else {
            // mapBoundsRect := domain.world.boundsRect
            //    result.x := (mapBoundsRect.left - mapBoundsRect.right) div 2
            //    result.y := mapBoundsRect.bottom + 30;  
            result = new TPoint(
                Math.round(this.mapViewState.viewportSize.X / 2 - this.mapViewState.scroll.X),
                Math.round(this.mapViewState.viewportSize.Y / 2 - this.mapViewState.scroll.Y)
            )
        }
        result.X = result.X + Math.round(Math.random() * 200) - 100
        result.Y = result.Y + Math.round(Math.random() * 200) - 100
        //if (domain <> nil) and (domain.world <> nil) then
        //    begin
        //    selection := domain.world.firstSelectedObject
        //    if selection <> nil then
        //      begin
        //      result.x := selection.position.x
        //      result.y := selection.position.y + 30
        //      end
        //    end
        //  result := Point(MapScrollBarHorizontal.position + MapImage.width div 2, MapScrollBarVertical.position +  MapImage.height div 2)
        //  //result.x := result.x + random(200) - 100
        //  //result.y := result.y + random(200) - 100
        //result := Point(MapScrollBarHorizontal.position + MapImage.width div 2, MapScrollBarVertical.position +  MapImage.height div 2)
        //  if (domain <> nil) and (domain.world <> nil) then
        //    begin
        //    selection := domain.world.firstSelectedObject
        //    if selection <> nil then
        //      begin
        //      result.x := selection.position.x
        //      result.y := selection.position.y
        //      end
        //    end
        //  result.x := result.x + random(200) - 100
        //  result.y := result.y + random(200) - 100;   
        return result
    }

    /* TODO: Use or remove

    options: DomainOptionsStructure = new DomainOptionsStructure()
    iniFileName: string = ""
    sessionOrWorldStartupFileName: string = ""
    playerOnly: boolean = false
    useIniFile: boolean = false

    startTimeThisSession: TDateTime = new TDateTime()
    
    create(): void {

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
*/

    updateForNewOrLoadedWorld(fileName: string, isWorldFileLoaded: boolean): void {
        this.worldCommandList.clear()
        if (fileName) {
            this.worldFileName = fileName
            // TODO: use or remove: this.options.mostRecentWorld = this.worldFileName
        } else {
            this.worldFileName = kUnsavedWorldFileName + "." + kWorldExtension
            // TODO: use or remove: this.options.mostRecentSession = ""
        }
        this.worldChangeCount = 0
        this.isWorldFileLoaded = isWorldFileLoaded

        this.editRule(null)
        this.lastSingleRuleIndex = 0

        this.ruleEditorForm.lastChoice = null
        this.ruleEditorForm.previousChoice = null

        this.newSession()
    }

    newSession(): void {
        this.world.newSession()
        this.sessionCommandList.clear()
        this.transcript.length = 0
        this.consoleForm.addLineToTranscript("Starting: " + makeFileNameWithoutWldExtension(this.worldFileName), Color.clGreen)

        this.sessionFileName = kUnsavedSessionFileName + "." + kSessionExtension
        this.sessionChangeCount = 0

        if (this.world.rules.length > 0) {
            // TODO: This used to call doCommand in the speechSystem -- but made change -- consider other ramifications?
            this.consoleForm.doCommand(this, this.world.rules[0].command.phrase)
        }
    }

    /* TODO: Use or remove
    
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
    
    mergeWorld(fileName: string): void {
        // don't clear things
        this.world.loadWorldFromFile(fileName)
        this.worldChangeCount += 1
    }
    
    saveWorld(fileName: string): void {
        this.world.saveWorldToFile(fileName, usworld.kSaveAllRules)
    }
    
    */

    commandChangedNotification(command: KfCommand, state: KfCommandChangeType): void {
        switch (state) {
            case KfCommandChangeType.commandDone:
                this.worldChangeDone()
                break
            case KfCommandChangeType.commandUndone:
                this.worldChangeUndone()
                break
            }
    }

    isWorldFileChanged(): boolean {
        return this.worldChangeCount !== 0
    }
    
    resetWorldChangeCount(): void {
        this.worldChangeCount = 0
    }
    
    worldChangeDone(): void {
        this.worldChangeCount += 1
    }
    
    worldChangeUndone(): void {
        this.worldChangeCount -= 1
    }
}
