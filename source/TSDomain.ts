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
import { doCommand, parseTextWithMacros, SegmentType } from "./ConsoleForm";
import { PendingTableScroll } from "./RuleTableView"
import { TPoint } from "./TPoint"
import { addToLog } from "./LogView"
import { TSVariable } from "./TSVariable"

// At the dawn of the third millenium,
// the laws of space and time keep humans close to Sol.
// Most of them live in billions of space habitats called 'gardens'.
// These are their stories...

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

export interface SpeechSystemAPI {
    lastSaidTextWithMacros: string
    speakText: (text: string) => void
    sayTextWithMacros: (text: string) => void
    checkForSayOptionsMacro: () => void
    listenForAvailableCommands: () => void
    stripMacros: (textWithMacros: string) => string
    haltSpeechAndSoundAndMusic: () => void
}

export type FormName = "about" | "console" | "demos" | "ruleEditor" | "file"
export type EditorName = "table" | "map" | "browser" | "wizards" | "log"
export type WizardName = "context" | "command" | "link"

let musicPlayed: HTMLAudioElement | null = null
let soundPlayed: HTMLAudioElement | null = null

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

    editRule: (rule: TSRule | null, scrollDirection?: ScrollIntoViewDirection, force?: boolean) => void

    browseBy: TSRuleField
    setOrganizeByField: (newValue: TSRuleField, selectedVariable: TSVariable | null) => void

    transcript: TranscriptLine[]

    worldFileName: string
    sessionFileName: string

    demoConfig: DemoConfig

    loadWorldFromServerData: (name: string) => Promise<boolean>

    showCommandPrefixInMap: boolean

    newSession: () => void
    isSessionFileChanged: () => boolean

    linkWizardData: LinkWizardData
    contextWizardData: ContextWizardData
    commandWizardData: CommandWizardData
    showWizardHelp: boolean

    mapViewState: MapViewState

    pendingTableScroll: PendingTableScroll | null
    pendingMapScroll: boolean
    pendingBrowserScroll: boolean
    pendingBrowserScrollSelectedVariable: TSVariable | null

    dataPath: string

    addToLog: (text: string) => void

    consoleForm: ConsoleFormAPI
    ruleEditorForm: RuleEditorAPI
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

    editRule(rule: TSRule | null, scrollDirection: ScrollIntoViewDirection = ScrollIntoViewDirection.kFromTop, force = false) {
        this.editedRule = rule
        if (rule) {
            // Don't normally scroll for table and browser forms when visible because scrolling shifts table
            // even if item visible -- which is jumpy if click on rule.
            // Use force flag so undo/redo commands track properly even if editor is visible.

            if (force || this.currentEditorView !== "table") {
                this.pendingTableScroll = {
                    rule: rule,
                    direction: scrollDirection,
                }
            }

            if (force || this.currentEditorView !== "browser") {
                this.pendingBrowserScroll = true
            }

            // the map does not scroll if item is visible
            this.pendingMapScroll = true
        }
    }

    setOrganizeByField(newValue: TSRuleField, selectedVariable: TSVariable | null) {
        this.browseBy = newValue
        this.pendingBrowserScrollSelectedVariable = selectedVariable
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
    pendingBrowserScrollSelectedVariable: TSVariable | null

    dataPath = "./data/"

    addToLog = addToLog

    // TODO: Rearchitect these so not called "Form"; rethink how domain is used to transfer this state
    consoleForm: ConsoleFormAPI
    ruleEditorForm: RuleEditorAPI
    speechSystem: SpeechSystemAPI

    constructor() {
        this.world = new TWorld(this.goodPosition.bind(this))

        this.sessionCommandList = new TSCommandList(this)
        this.sessionCommandList.setNewUndoLimit(1000)
        this.sessionCommandList.notifyProcedure = this.sessionCommandChangedNotification.bind(this)

        this.worldCommandList = new TSCommandList(this)
        this.worldCommandList.setNewUndoLimit(1000)
        this.worldCommandList.notifyProcedure = this.worldCommandChangedNotification.bind(this)

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

        this.speechSystem = {
            lastSaidTextWithMacros: "TEST",
            stripMacros: (text: string) => text,
            sayTextWithMacros: (text: string) => {
                // TODO: Move this into a function elsewhere
                const segments = parseTextWithMacros(text)
                for (let segment of segments) {
                    if (segment.type === SegmentType.speakSound) {
                        // TODO: let sounds play over each other
                        if (soundPlayed) {
                            soundPlayed.pause()
                            soundPlayed = null
                        }
                        // Filter out old files from demos
                        if (segment.text && (segment.text.startsWith("http") || segment.text.startsWith("/"))) {
                            soundPlayed = new Audio(segment.text)
                            soundPlayed.play()
                        }
                    } else if (segment.type === SegmentType.speakMusic) {
                        // Only one music can play at a time
                        if (musicPlayed) {
                            musicPlayed.pause()
                            musicPlayed = null
                        }
                        // Filter out old files from demos
                        if (segment.text && (segment.text.startsWith("http") || segment.text.startsWith("/"))) {
                            musicPlayed = new Audio(segment.text)
                            musicPlayed.play()
                        }
                    } else if (segment.type === SegmentType.speakText) {
                        // TODO: need scheduling to interweave sound and TTS
                        const synth = window.speechSynthesis
                        if (synth) {
                            const sentences = segment.text.match( /[^\.!\?]+[\.!\?]+/g)
                            if (sentences) {
                                for (let sentence of sentences) { 
                                    const utterance = new SpeechSynthesisUtterance(sentence)
                                    synth.speak(utterance)
                                }
                            }
                        }
                    } else {
                        // image
                    }
                }
            },
            listenForAvailableCommands: () => null,
            checkForSayOptionsMacro: () => null,
            speakText: (text: string) => null,
            haltSpeechAndSoundAndMusic: () => {
                if (window.speechSynthesis && window.speechSynthesis.speaking) window.speechSynthesis.cancel()
                if (musicPlayed) musicPlayed.pause()
                if (soundPlayed) soundPlayed.pause()
            },
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
        this.addToLog("--- world change: " + fileName)
        this.speechSystem.haltSpeechAndSoundAndMusic()
        this.worldCommandList.clear()
        this.resetWorldChangeCount()
        if (fileName) {
            this.worldFileName = fileName
            // TODO: use or remove: this.options.mostRecentWorld = this.worldFileName
        } else {
            this.worldFileName = kUnsavedWorldFileName + "." + kWorldExtension
            // TODO: use or remove: this.options.mostRecentSession = ""
        }
        this.isWorldFileLoaded = isWorldFileLoaded

        this.lastSingleRuleIndex = 0
        this.ruleEditorForm.lastChoice = null
        this.ruleEditorForm.previousChoice = null

        const ruleToEdit = this.world.rules.length ? this.world.rules[0] : null
        this.editRule(ruleToEdit)
        if (ruleToEdit) {
            ruleToEdit.selected = true
        }

        this.newSession()

        // Reset map partially
        // this.mapViewState = newMapViewState()
        this.mapViewState.scroll.X = 0
        this.mapViewState.scroll.Y = 0

        // TODO keep track of most recent world: this.options.mostRecentWorld = fileName
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
    
    */

    isSessionFileChanged(): boolean {
        return this.sessionChangeCount !== 0
    }

    sessionCommandChangedNotification(command: KfCommand, state: KfCommandChangeType): void {
        switch (state) {
            case KfCommandChangeType.commandDone:
                this.sessionChangeCount += 1
                break
            case KfCommandChangeType.commandUndone:
                this.sessionChangeCount -= 1
                break
            default:
                throw new Error("sessionCommandChangedNotification: unexpected case")
        }
    }

   worldCommandChangedNotification(command: KfCommand, state: KfCommandChangeType): void {
    switch (state) {
        case KfCommandChangeType.commandDone:
            this.worldChangeDone()
            break
        case KfCommandChangeType.commandUndone:
            this.worldChangeUndone()
            break
        default:
            throw new Error("worldCommandChangedNotification: unexpected case")
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
