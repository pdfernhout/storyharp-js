// unit usruleeditorform

from conversion_common import *
import uspictureform
import usjavawriter
import uspreferences
import uregister
import usabout
import uschangelog
import uslinkwizard
import uscommandwizard
import uscontextwizard
import ufilesupport
import usconsoleform
import usmodelchanges
import usmapview
import ucommand
import usfocuscommands
import uscommands
import usworld
import usdomain
import quickfillcombobox
import delphi_compatability

// var
let RuleEditorForm: TRuleEditorForm


// const
const kFromBottom = true
const kFromTop = false
const kPlaySoundMacroStart = "sound "
const kPlayMusicMacroStart = "music "
const kShowPictureMacroStart = "picture "


// const
const kMinListHeight = 115
const kMinRestHeight = 24 * 4 + 4 * 5
const kMinRequirementsChangesHeight = 60
const kSplitterHeight = 3


//$R *.DFM
// ----------------------------------------------------------- @Local functions
function localIntMin(a: int, b: int): int {
    let result = 0
    result = a
    if (b < a) {
        result = b
    }
    return result
}

function localIntMax(a: int, b: int): int {
    let result = 0
    result = a
    if (b > a) {
        result = b
    }
    return result
}

function setCanvasColorsForSelection(canvas: TCanvas, selected: boolean, focused: boolean, isCommandInMap: boolean): void {
    canvas.Brush.Color = delphi_compatability.clWindow
    canvas.Font.Color = UNRESOLVED.clWindowText
    canvas.Font.Style = {}
    if (focused && selected) {
        canvas.Brush.Color = usdomain.domain.options.selectedItemColor
        canvas.Font.Color = usdomain.domain.options.selectedTextColor
        canvas.Font.Style = {UNRESOLVED.fsBold, }
    } else if (focused) {
        canvas.Font.Style = {UNRESOLVED.fsBold, }
    } else if (selected) {
        canvas.Brush.Color = usdomain.domain.options.selectedItemColor
        canvas.Font.Color = usdomain.domain.options.selectedTextColor
    }
    if (isCommandInMap) {
        canvas.Font.Color = usdomain.domain.options.commandTextColorInMap
    }
}

// const
const kTopBottomBorderSize = 120


// const
const kLeftRightBorderSize = 120


// const
const kGap = 4
const kGap2 = kGap * 2
const kGap3 = kGap * 3



//INDEX (search for at symbol and name)
//Local
//Creation/destruction
//File menu
//Edit menu
//Rule menu
//Display menu
//Tools menu
//Help menu
//Button bar
//Updating
//Events
//Commands
//Table
//Map
//Browser
//Resizing
export class TRuleEditorForm {
    PanelEditor: TPanel = new TPanel()
    WaveFileOpenDialog: TOpenDialog = new TOpenDialog()
    ImageList: TImageList = new TImageList()
    SplitterEdit: TSplitter = new TSplitter()
    PanelRequirementsChanges: TPanel = new TPanel()
    RequirementsSpeedButton: TSpeedButton = new TSpeedButton()
    ChangesSpeedButton: TSpeedButton = new TSpeedButton()
    RequirementsListBox: TListBox = new TListBox()
    ChangesListBox: TListBox = new TListBox()
    RequirementsEdit: TEdit = new TEdit()
    ChangesEdit: TEdit = new TEdit()
    PanelRest: TPanel = new TPanel()
    Label5: TLabel = new TLabel()
    CommandSpeedButton: TSpeedButton = new TSpeedButton()
    MoveSpeedButton: TSpeedButton = new TSpeedButton()
    ContextSpeedButton: TSpeedButton = new TSpeedButton()
    ContextEdit: TEdit = new TEdit()
    CommandEdit: TEdit = new TEdit()
    ReplyMemo: TMemo = new TMemo()
    MoveEdit: TEdit = new TEdit()
    SplitterRequirementsChanges: TSplitter = new TSplitter()
    MainMenu1: TMainMenu = new TMainMenu()
    MenuEdit: TMenuItem = new TMenuItem()
    MenuEditCut: TMenuItem = new TMenuItem()
    MenuEditCopy: TMenuItem = new TMenuItem()
    MenuEditPaste: TMenuItem = new TMenuItem()
    MenuEditUndo: TMenuItem = new TMenuItem()
    MenuEditRedo: TMenuItem = new TMenuItem()
    N1: TMenuItem = new TMenuItem()
    MenuRule: TMenuItem = new TMenuItem()
    MenuRuleNew: TMenuItem = new TMenuItem()
    MenuRuleDelete: TMenuItem = new TMenuItem()
    MenuRuleDuplicate: TMenuItem = new TMenuItem()
    MenuBrowseByContext: TMenuItem = new TMenuItem()
    MenuBrowseByCommand: TMenuItem = new TMenuItem()
    MenuBrowseByMove: TMenuItem = new TMenuItem()
    MenuBrowseByRequirements: TMenuItem = new TMenuItem()
    MenuBrowseByChanges: TMenuItem = new TMenuItem()
    MenuFile: TMenuItem = new TMenuItem()
    MenuFileNewWorld: TMenuItem = new TMenuItem()
    MenuFileSaveWorld: TMenuItem = new TMenuItem()
    MenuFileSaveWorldAs: TMenuItem = new TMenuItem()
    MenuFileMergeWithWorld: TMenuItem = new TMenuItem()
    N3: TMenuItem = new TMenuItem()
    MenuRuleRaise: TMenuItem = new TMenuItem()
    MenuRuleLower: TMenuItem = new TMenuItem()
    MenuMaps: TMenuItem = new TMenuItem()
    MenuHelp: TMenuItem = new TMenuItem()
    MenuMapsQuickContexts: TMenuItem = new TMenuItem()
    MenuFileExit: TMenuItem = new TMenuItem()
    MenuFileOpenWorld: TMenuItem = new TMenuItem()
    MenuHelpContents: TMenuItem = new TMenuItem()
    MenuHelpEditingWorlds: TMenuItem = new TMenuItem()
    N6: TMenuItem = new TMenuItem()
    MenuHelpAbout: TMenuItem = new TMenuItem()
    FontDialog: TFontDialog = new TFontDialog()
    MenuMapsShowCommands: TMenuItem = new TMenuItem()
    MenuHelpRegister: TMenuItem = new TMenuItem()
    EditPopupMenu: TPopupMenu = new TPopupMenu()
    PopupCut: TMenuItem = new TMenuItem()
    PopupCopy: TMenuItem = new TMenuItem()
    PopupPaste: TMenuItem = new TMenuItem()
    MenuMapQuickCommands: TMenuItem = new TMenuItem()
    MenuMapLinkWizard: TMenuItem = new TMenuItem()
    MenuEditLogFile: TMenuItem = new TMenuItem()
    Wizards1: TMenuItem = new TMenuItem()
    N7: TMenuItem = new TMenuItem()
    N8: TMenuItem = new TMenuItem()
    N9: TMenuItem = new TMenuItem()
    MenuHelpTutorial: TMenuItem = new TMenuItem()
    AfterRegisterMenuSeparator: TMenuItem = new TMenuItem()
    N10: TMenuItem = new TMenuItem()
    MenuFileExport: TMenuItem = new TMenuItem()
    N11: TMenuItem = new TMenuItem()
    PanelTop: TPanel = new TPanel()
    ListPages: TPageControl = new TPageControl()
    TabSheetTable: TTabSheet = new TTabSheet()
    RuleGrid: TDrawGrid = new TDrawGrid()
    TabSheetBrowse: TTabSheet = new TTabSheet()
    PanelLists: TPanel = new TPanel()
    SplitterLists: TSplitter = new TSplitter()
    PanelFirstList: TPanel = new TPanel()
    firstListBoxImage: TImage = new TImage()
    firstListBoxLabel: TLabel = new TLabel()
    FirstListBox: TListBox = new TListBox()
    PanelSecondList: TPanel = new TPanel()
    SecondListBoxImage: TImage = new TImage()
    SecondListBoxLabel: TLabel = new TLabel()
    SecondListBox: TListBox = new TListBox()
    TabSheetMap: TTabSheet = new TTabSheet()
    PanelMap: TPanel = new TPanel()
    MapImage: TImage = new TImage()
    MapScrollBarHorizontal: TScrollBar = new TScrollBar()
    MapScrollBarVertical: TScrollBar = new TScrollBar()
    PanelButtonBar: TPanel = new TPanel()
    NewRuleButton: TSpeedButton = new TSpeedButton()
    DuplicateRuleButton: TSpeedButton = new TSpeedButton()
    DeleteRuleButton: TSpeedButton = new TSpeedButton()
    MoveUpButton: TSpeedButton = new TSpeedButton()
    MoveDownButton: TSpeedButton = new TSpeedButton()
    RuleNumberLabel: TLabel = new TLabel()
    MenuDisplayShowButtonBar: TMenuItem = new TMenuItem()
    MenuToolsSearch: TMenuItem = new TMenuItem()
    N2: TMenuItem = new TMenuItem()
    FindDialog: TFindDialog = new TFindDialog()
    N12: TMenuItem = new TMenuItem()
    MenuEditInsertSound: TMenuItem = new TMenuItem()
    insertSound: TSpeedButton = new TSpeedButton()
    MediaPlayer: TMediaPlayer = new TMediaPlayer()
    MenuOptionsShowRuleEditor: TMenuItem = new TMenuItem()
    InsertMusicButton: TSpeedButton = new TSpeedButton()
    MenuEditInsertMusic: TMenuItem = new TMenuItem()
    N13: TMenuItem = new TMenuItem()
    MenuEditPreferences: TMenuItem = new TMenuItem()
    replyPicture: TImage = new TImage()
    N5: TMenuItem = new TMenuItem()
    MenuRuleTestReply: TMenuItem = new TMenuItem()
    MenuHelpBasicConcepts: TMenuItem = new TMenuItem()
    N4: TMenuItem = new TMenuItem()
    MenuWorldSwitchToPlayer: TMenuItem = new TMenuItem()
    MapPopupMenu: TPopupMenu = new TPopupMenu()
    PopupNewContext: TMenuItem = new TMenuItem()
    PopupNewLink: TMenuItem = new TMenuItem()
    PopupNewCommand: TMenuItem = new TMenuItem()
    MenuToolsGenerateJava: TMenuItem = new TMenuItem()
    N14: TMenuItem = new TMenuItem()
    MenuEditInsertPicture: TMenuItem = new TMenuItem()
    rule: TSRule = new TSRule()
    selectionInformation: TSelectionInformation = new TSelectionInformation()
    organizeByField: int = 0
    wasLoaded: boolean = false
    lastSaveProceeded: boolean = false
    lastClickAtLeft: boolean = false
    ignoreNextEnter: boolean = false
    indexEdited: int = 0
    lastCommand: TSRule = new TSRule()
    lastSingleRuleIndex: int = 0
    lastBrowserSingleRuleIndex: int = 0
    loopMusic: boolean = false
    buttonSymbols: boolean = false
    startingUp: boolean = false
    actionInProgress: boolean = false
    previousChoice: TSDraggableObject = new TSDraggableObject()
    lastChoice: TSDraggableObject = new TSDraggableObject()
    mapSelectionInProgress: boolean = false
    mapSelectionRect: TRect = new TRect()
    lastMapMouseDownPosition: TPoint = new TPoint()
    numNewContextsMadeByPopupMenuThisSession: int = 0
    numNewCommandsMadeByPopupMenuThisSession: int = 0
    TRuleEditorForm.prototype = new TForm()
    TRuleEditorForm.prototype.constructor = TRuleEditorForm
    
    // ------------------------------------------------------ @Creation/destruction
    FormCreate(Sender: TObject): void {
        this.numNewContextsMadeByPopupMenuThisSession = 1
        this.numNewCommandsMadeByPopupMenuThisSession = 1
        usdomain.domain.setFormSize(this, usdomain.domain.options.editorWindowRect)
        if (usdomain.domain.options.editorPanelEditorHeight > 0) {
            this.PanelEditor.Height = usdomain.domain.options.editorPanelEditorHeight
            this.PanelEditor.Top = this.ClientHeight - this.PanelEditor.Height
        }
        if (usdomain.domain.options.editorPanelRequirementsChangesHeight > 0) {
            this.PanelRequirementsChanges.Height = usdomain.domain.options.editorPanelRequirementsChangesHeight
            this.PanelRequirementsChanges.Top = this.PanelEditor.ClientHeight - this.PanelRequirementsChanges.Height
        }
        switch (usdomain.domain.options.pageShowing) {
            case usdomain.kPageTable:
                this.ListPages.ActivePage = this.TabSheetTable
                break
            case usdomain.kPageMap:
                this.ListPages.ActivePage = this.TabSheetMap
                break
            case usdomain.kPageBrowser:
                this.ListPages.ActivePage = this.TabSheetBrowse
                break
        if (usdomain.domain.options.editorPanelFirstListWidth > 0) {
            this.PanelFirstList.Width = usdomain.domain.options.editorPanelFirstListWidth
        }
        usdomain.domain.worldCommandList.notifyProcedure = this.commandChangedNotification
        this.startingUp = true
        this.updateForChangeToDomainOptions()
        this.startingUp = false
    }
    
    switchToPage(newPage: TTabSheet): boolean {
        let result = false
        result = false
        if (this.ListPages.ActivePage === newPage) {
            return result
        }
        this.ListPages.ActivePage = newPage
        this.ListPagesChange(this)
        result = true
        return result
    }
    
    FormActivate(Sender: TObject): void {
        if (delphi_compatability.Application.terminated) {
            return
        }
        if (this.FirstListBox.Items.Count === 0) {
            //  self.updateForRuleChange;
            //	self.loadFirstListBox;
            //	self.loadSecondListBox;
            this.setOrganizeByField(this.organizeByField)
            //if secondListBox.items.count > 0 then
        }
        this.wasLoaded = true
        if (usdomain.domain.world.rules.Count + 1 >= 2) {
            this.RuleGrid.RowCount = usdomain.domain.world.rules.Count + 1
        }
    }
    
    commitChangesToRule(): void {
        this.RequirementsEdit.Hide()
        this.ChangesEdit.Hide()
        if (this.rule === null) {
            return
        }
        if (this.ContextEdit.Text !== this.rule.context.phrase) {
            usdomain.domain.worldCommandList.ruleFieldChange(this.rule, usworld.kRuleContext, this.ContextEdit.Text)
        }
        if (this.CommandEdit.Text !== this.rule.command.phrase) {
            usdomain.domain.worldCommandList.ruleFieldChange(this.rule, usworld.kRuleCommand, this.CommandEdit.Text)
        }
        if (this.ReplyMemo.Text !== this.rule.reply) {
            usdomain.domain.worldCommandList.ruleFieldChange(this.rule, usworld.kRuleReply, this.ReplyMemo.Text)
        }
        if (this.MoveEdit.Text !== this.rule.move.phrase) {
            usdomain.domain.worldCommandList.ruleFieldChange(this.rule, usworld.kRuleMove, this.MoveEdit.Text)
        }
        if (this.logicalStatementForListBox(this.RequirementsListBox) !== this.rule.decompileRequirements()) {
            usdomain.domain.worldCommandList.ruleFieldChange(this.rule, usworld.kRuleRequirements, this.logicalStatementForListBox(this.RequirementsListBox))
        }
        if (this.logicalStatementForListBox(this.ChangesListBox) !== this.rule.decompileChanges()) {
            usdomain.domain.worldCommandList.ruleFieldChange(this.rule, usworld.kRuleChanges, this.logicalStatementForListBox(this.ChangesListBox))
        }
    }
    
    setButtonGlyphs(): void {
        if (usconsoleform.ConsoleForm === null) {
            return
        }
        this.ContextSpeedButton.Glyph = usconsoleform.ConsoleForm.ContextButton.Glyph
        this.CommandSpeedButton.Glyph = usconsoleform.ConsoleForm.CommandButton.Glyph
        this.MoveSpeedButton.Glyph = usconsoleform.ConsoleForm.MoveButton.Glyph
        this.RequirementsSpeedButton.Glyph = usconsoleform.ConsoleForm.RequirementsButton.Glyph
        this.ChangesSpeedButton.Glyph = usconsoleform.ConsoleForm.ChangesButton.Glyph
    }
    
    WMEraseBkgnd(m: TWMEraseBkgnd): void {
        // since we're going to be painting the whole form, handling this
        // message will suppress the uneccessary repainting of the background
        // which can result in flicker.
        m.Result = UNRESOLVED.LRESULT(false)
    }
    
    FormDeactivate(Sender: TObject): void {
        //	ShowMessage('deactivate');
        // unfinished self.makeChangeCommandIfNeeded;
        this.commitChangesToRule()
    }
    
    // ----------------------------------------------------------------- @File menu
    MenuFileNewWorldClick(Sender: TObject): void {
        this.commitChangesToRule()
        if (!this.askForSaveWorldAndProceed()) {
            return
        }
        if (!usconsoleform.ConsoleForm.askForSaveSessionAndProceed()) {
            return
        }
        this.rule = null
        usdomain.domain.newWorld()
        usconsoleform.ConsoleForm.locationCacheValid = false
        usconsoleform.ConsoleForm.clearTranscript()
        this.updateForRuleChange()
        this.updateViews()
        this.editRule(null)
        this.previousChoice = null
        this.lastChoice = null
        usconsoleform.ConsoleForm.speechSystem.haltSpeechAndSound()
        // turns it off
        usconsoleform.ConsoleForm.speechSystem.speakSound("music")
    }
    
    MenuFileOpenWorldClick(Sender: TObject): void {
        let fileNameWithPath: string
        
        this.commitChangesToRule()
        if (!this.askForSaveWorldAndProceed()) {
            return
        }
        if (!usconsoleform.ConsoleForm.askForSaveSessionAndProceed()) {
            return
        }
        fileNameWithPath = ufilesupport.getFileOpenInfo(ufilesupport.kFileTypeWorld, usdomain.domain.worldFileName, "Choose a world file", ufilesupport.kOtherExtNotOK)
        if (fileNameWithPath === "") {
            return
        }
        this.openWorldFile(fileNameWithPath)
        usdomain.domain.world.setInitialFocus()
        usdomain.domain.world.updateAvailable()
        this.updateViews()
        usconsoleform.ConsoleForm.updateTitles()
        usconsoleform.ConsoleForm.speechSystem.haltSpeechAndSound()
        // turns it off
        usconsoleform.ConsoleForm.speechSystem.speakSound("music")
        if (usdomain.domain.world.rules.Count > 0) {
            usconsoleform.ConsoleForm.speechSystem.doCommand(usworld.TSRule(usdomain.domain.world.rules[0]).command.phrase)
        }
    }
    
    WMDropFiles(Msg: TWMDropFiles): void {
        let CFileName: char[] /* MAX_PATH + 1 */
        
        try {
            if (UNRESOLVED.DragQueryFile(Msg.Drop, 0, CFileName, UNRESOLVED.MAX_PATH) > 0) {
                if (UNRESOLVED.pos(".WLD", uppercase(CFileName)) <= 0) {
                    return
                }
                this.commitChangesToRule()
                if (!this.askForSaveWorldAndProceed()) {
                    return
                }
                if (!usconsoleform.ConsoleForm.askForSaveSessionAndProceed()) {
                    return
                }
                this.openWorldFile(CFileName)
                usdomain.domain.world.setInitialFocus()
                usdomain.domain.world.updateAvailable()
                this.updateViews()
                usconsoleform.ConsoleForm.updateTitles()
                usconsoleform.ConsoleForm.speechSystem.haltSpeechAndSound()
                // turns it off
                usconsoleform.ConsoleForm.speechSystem.speakSound("music")
                if (usdomain.domain.world.rules.Count > 0) {
                    usconsoleform.ConsoleForm.speechSystem.doCommand(usworld.TSRule(usdomain.domain.world.rules[0]).command.phrase)
                }
                Msg.Result = 0
            }
        } finally {
            UNRESOLVED.DragFinish(Msg.Drop)
        }
    }
    
    //kludge for popup edits...
    openWorldFile(fileNameWithPath: string): void {
        try {
            ufilesupport.startWaitMessage("Opening " + ExtractFileName(fileNameWithPath))
            usdomain.domain.loadWorld(fileNameWithPath)
        } catch (Exception E) {
            ufilesupport.stopWaitMessage()
            ShowMessage(E.message)
            ShowMessage("Could not load file " + fileNameWithPath)
            usdomain.domain.newWorld()
            usconsoleform.ConsoleForm.clearTranscript()
            this.updateViews()
            return
        }
        ufilesupport.stopWaitMessage()
        //repeated for new
        usconsoleform.ConsoleForm.locationCacheValid = false
        usconsoleform.ConsoleForm.clearTranscript()
        this.rule = null
        this.updateForRuleChange()
        this.MapPaintBoxChanged()
        this.adjustScrollBars()
        this.updateViews()
        this.editRule(null)
    }
    
    MenuFileSaveWorldClick(Sender: TObject): void {
        let fileInfo: SaveFileNamesStructure
        
        this.commitChangesToRule()
        if (UNRESOLVED.pos(uppercase(usdomain.kUnsavedWorldFileName), uppercase(ExtractFileName(usdomain.domain.worldFileName))) > 0) {
            this.MenuFileSaveWorldAsClick(this)
            return
        }
        this.lastSaveProceeded = ufilesupport.getFileSaveInfo(ufilesupport.kFileTypeWorld, ufilesupport.kDontAskForFileName, usdomain.domain.worldFileName, fileInfo)
        if (!this.lastSaveProceeded) {
            return
        }
        try {
            ufilesupport.startFileSave(fileInfo)
            usdomain.domain.saveWorld(fileInfo.tempFile)
            fileInfo.writingWasSuccessful = true
        } finally {
            this.lastSaveProceeded = ufilesupport.cleanUpAfterFileSave(fileInfo)
        }
        usdomain.domain.resetWorldChangeCount()
        usdomain.domain.worldCommandList.clear()
        usdomain.domain.options.mostRecentWorld = fileInfo.newFile
        this.updateMenus()
    }
    
    MenuFileSaveWorldAsClick(Sender: TObject): void {
        let fileInfo: SaveFileNamesStructure
        
        this.commitChangesToRule()
        this.lastSaveProceeded = ufilesupport.getFileSaveInfo(ufilesupport.kFileTypeWorld, ufilesupport.kAskForFileName, usdomain.domain.worldFileName, fileInfo)
        if (!this.lastSaveProceeded) {
            return
        }
        try {
            ufilesupport.startFileSave(fileInfo)
            usdomain.domain.saveWorld(fileInfo.tempFile)
            fileInfo.writingWasSuccessful = true
        } finally {
            this.lastSaveProceeded = ufilesupport.cleanUpAfterFileSave(fileInfo)
        }
        usdomain.domain.worldFileName = fileInfo.newFile
        usdomain.domain.options.mostRecentWorld = fileInfo.newFile
        usdomain.domain.resetWorldChangeCount()
        usdomain.domain.worldCommandList.clear()
        this.updateMenus()
        usconsoleform.ConsoleForm.updateTitles()
    }
    
    askForSaveWorldAndProceed(): boolean {
        let result = false
        let messageBoxResult: int
        
        result = true
        if (!usdomain.domain.isWorldFileChanged()) {
            return result
        }
        messageBoxResult = MessageDialog("Save changes to world " + ExtractFileName(usdomain.domain.worldFileName) + "?", mtConfirmation, mbYesNoCancel, 0)
        switch (messageBoxResult) {
            case delphi_compatability.IDCANCEL:
                result = false
                break
            case delphi_compatability.IDYES:
                this.MenuFileSaveWorldClick(this)
                result = this.lastSaveProceeded
                break
            case delphi_compatability.IDNO:
                result = true
                break
            default:
                ShowMessage("Error with save request dialog.")
                result = true
                break
        return result
    }
    
    MenuFileMergeWithWorldClick(Sender: TObject): void {
        let fileNameWithPath: string
        let oldRuleCount: int
        let oldVariablesCount: int
        let i: int
        let newRulesCommand: TSNewRulesCommand
        let rule: TSRule
        let variable: TSVariable
        
        this.commitChangesToRule()
        fileNameWithPath = ufilesupport.getFileOpenInfo(ufilesupport.kFileTypeWorld, usdomain.domain.worldFileName, "Choose a world file to merge into this one", ufilesupport.kOtherExtNotOK)
        if (fileNameWithPath === "") {
            return
        }
        oldRuleCount = usdomain.domain.world.rules.Count
        oldVariablesCount = usdomain.domain.world.variables.Count
        newRulesCommand = uscommands.TSNewRulesCommand().create()
        newRulesCommand.creator = "merging " + ExtractFileName(fileNameWithPath)
        try {
            ufilesupport.startWaitMessage("Opening " + ExtractFileName(fileNameWithPath))
            usdomain.domain.mergeWorld(fileNameWithPath)
        } catch (Exception E) {
            ufilesupport.stopWaitMessage()
            ShowMessage(E.message)
            ShowMessage("Could not correctly merge in file " + fileNameWithPath)
            for (i = oldRuleCount; i <= usdomain.domain.world.rules.Count - 1; i++) {
                // clear out those new rules and variables
                rule = usworld.TSRule(usdomain.domain.world.rules[i])
                usdomain.domain.world.rules.Remove(rule)
                rule.free
            }
            for (i = oldVariablesCount; i <= usdomain.domain.world.variables.Count - 1; i++) {
                variable = usworld.TSVariable(usdomain.domain.world.variables[i])
                usdomain.domain.world.variables.Remove(variable)
                variable.free
            }
            newRulesCommand.free
            this.updateViews()
            return
        }
        ufilesupport.stopWaitMessage()
        usconsoleform.ConsoleForm.locationCacheValid = false
        usdomain.domain.world.deselectAllExcept(null)
        for (i = oldRuleCount; i <= usdomain.domain.world.rules.Count - 1; i++) {
            // select new items
            rule = usworld.TSRule(usdomain.domain.world.rules[i])
            newRulesCommand.addRule(rule)
            rule.selected = true
        }
        for (i = oldVariablesCount; i <= usdomain.domain.world.variables.Count - 1; i++) {
            variable = usworld.TSVariable(usdomain.domain.world.variables[i])
            variable.selected = true
        }
        usdomain.domain.worldCommandList.doCommand(newRulesCommand)
        if (usdomain.domain.world.rules.Count > 0) {
            this.editRule(usdomain.domain.world.rules[usdomain.domain.world.rules.Count - 1])
        } else {
            this.editRule(null)
        }
        this.scrollGridSelectionsIntoView(kFromTop)
        this.updateForRuleChange()
        this.updateViews()
    }
    
    MenuFileExitClick(Sender: TObject): void {
        this.commitChangesToRule()
        if (!this.askForSaveWorldAndProceed()) {
            return
        }
        if (!usconsoleform.ConsoleForm.askForSaveSessionAndProceed()) {
            return
        }
        usconsoleform.ConsoleForm.cleanUpBeforeExit()
        delphi_compatability.Application.Terminate()
    }
    
    // ----------------------------------------------------------------- @Edit menu
    MenuEditUndoClick(Sender: TObject): void {
        this.commitChangesToRule()
        usdomain.domain.worldCommandList.undoLast()
    }
    
    MenuEditRedoClick(Sender: TObject): void {
        this.commitChangesToRule()
        usdomain.domain.worldCommandList.redoLast()
    }
    
    MenuEditCutClick(Sender: TObject): void {
        let clip: string
        let key: byte
        let Shift: TShiftState
        
        if (this.rule === null) {
            // should not commit rule because may be in floating edit
            return
        }
        clip = ""
        key = delphi_compatability.VK_DELETE
        Shift = {}
        if (this.ActiveControl === this.RequirementsListBox) {
            if (this.RequirementsListBox.ItemIndex < this.RequirementsListBox.Items.Count) {
                if (this.RequirementsListBox.ItemIndex < 0) {
                    return
                }
                clip = trim(this.RequirementsListBox.Items[this.RequirementsListBox.ItemIndex])
                if (UNRESOLVED.pos("~", clip) === 1) {
                    clip = UNRESOLVED.copy(clip, 2, len(clip))
                }
                key = this.ListBoxKeyUp(this.RequirementsListBox, key, Shift)
            }
        } else if (this.ActiveControl === this.ChangesListBox) {
            if (this.ChangesListBox.ItemIndex < this.ChangesListBox.Items.Count) {
                if (this.ChangesListBox.ItemIndex < 0) {
                    return
                }
                clip = trim(this.ChangesListBox.Items[this.ChangesListBox.ItemIndex])
                if (UNRESOLVED.pos("~", clip) === 1) {
                    clip = UNRESOLVED.copy(clip, 2, len(clip))
                }
                key = this.ListBoxKeyUp(this.ChangesListBox, key, Shift)
            }
        } else if (this.ActiveControl instanceof delphi_compatability.TMemo) {
            clip = (this.ActiveControl).selText
            (this.ActiveControl).selText = ""
        } else if (this.ActiveControl instanceof delphi_compatability.TEdit) {
            clip = (this.ActiveControl).selText
            (this.ActiveControl).selText = ""
        }
        UNRESOLVED.Clipboard.setTextBuf(clip)
    }
    
    MenuEditCopyClick(Sender: TObject): void {
        let clip: string
        
        if (this.rule === null) {
            // should not commit rule because may be in floating edit
            return
        }
        clip = ""
        if (this.ActiveControl === this.RequirementsListBox) {
            if (this.RequirementsListBox.ItemIndex < 0) {
                return
            }
            if (this.RequirementsListBox.ItemIndex < this.RequirementsListBox.Items.Count) {
                clip = trim(this.RequirementsListBox.Items[this.RequirementsListBox.ItemIndex])
            }
            if (UNRESOLVED.pos("~", clip) === 1) {
                clip = UNRESOLVED.copy(clip, 2, len(clip))
            }
        } else if (this.ActiveControl === this.ChangesListBox) {
            if (this.ChangesListBox.ItemIndex < 0) {
                return
            }
            if (this.ChangesListBox.ItemIndex < this.ChangesListBox.Items.Count) {
                clip = trim(this.ChangesListBox.Items[this.ChangesListBox.ItemIndex])
            }
            if (UNRESOLVED.pos("~", clip) === 1) {
                clip = UNRESOLVED.copy(clip, 2, len(clip))
            }
        } else if (this.ActiveControl instanceof delphi_compatability.TMemo) {
            clip = (this.ActiveControl).selText
        } else if (this.ActiveControl instanceof delphi_compatability.TEdit) {
            clip = (this.ActiveControl).selText
        }
        UNRESOLVED.Clipboard.setTextBuf(clip)
    }
    
    MenuEditPasteClick(Sender: TObject): void {
        let MyHandle: THandle
        let TextPtr: "UNFINISHED_FIX_THIS_PCHAR_INIT"
        let clip: string
        let edit: TEdit
        
        if (this.rule === null) {
            // should not commit rule because may be in floating edit
            return
        }
        clip = ""
        UNRESOLVED.ClipBoard.Open
        try {
            MyHandle = UNRESOLVED.Clipboard.GetAsHandle(UNRESOLVED.CF_TEXT)
            TextPtr = UNRESOLVED.GlobalLock(MyHandle)
            clip = UNRESOLVED.StrPas(TextPtr)
            UNRESOLVED.GlobalUnlock(MyHandle)
        } finally {
            UNRESOLVED.Clipboard.Close
        }
        if (this.ActiveControl === this.RequirementsListBox) {
            if (this.RequirementsListBox.Items.Count < 1) {
                return
            }
            this.indexEdited = this.RequirementsListBox.ItemIndex
            if (this.indexEdited < 0) {
                this.indexEdited = this.RequirementsListBox.Items.Count - 1
            }
            if (this.indexEdited > this.RequirementsListBox.Items.Count - 1) {
                this.indexEdited = this.RequirementsListBox.Items.Count - 1
            }
            edit = this.RequirementsEdit
            edit.Text = clip
            this.ListBoxEditExit(edit)
        } else if (this.ActiveControl === this.ChangesListBox) {
            if (this.ChangesListBox.Items.Count < 1) {
                return
            }
            this.indexEdited = this.ChangesListBox.ItemIndex
            if (this.indexEdited < 0) {
                this.indexEdited = this.ChangesListBox.Items.Count - 1
            }
            if (this.indexEdited > this.ChangesListBox.Items.Count - 1) {
                this.indexEdited = this.ChangesListBox.Items.Count - 1
            }
            edit = this.ChangesEdit
            edit.Text = clip
            this.ListBoxEditExit(edit)
        } else if (this.ActiveControl instanceof delphi_compatability.TMemo) {
            (this.ActiveControl).selText = clip
        } else if (this.ActiveControl instanceof delphi_compatability.TEdit) {
            // PDF PORT - removed unneeded parens
            (this.ActiveControl).selText = clip
        } else {
            UNRESOLVED.Beep
        }
    }
    
    MenuEditPreferencesClick(Sender: TObject): void {
        this.commitChangesToRule()
        if (uspreferences.PreferencesForm === null) {
            return
        }
        uspreferences.PreferencesForm.options = usdomain.domain.options
        if (uspreferences.PreferencesForm.ShowModal() === mrOK) {
            usdomain.domain.options = uspreferences.PreferencesForm.options
            this.updateForChangeToDomainOptions()
        }
    }
    
    // ----------------------------------------------------------------- @Rule menu
    MenuRuleNewClick(Sender: TObject): void {
        let newRule: TSRule
        let variable: TSvariable
        let newRulesCommand: TSNewRulesCommand
        
        this.commitChangesToRule()
        newRulesCommand = uscommands.TSNewRulesCommand().create()
        newRule = usdomain.domain.world.newRule()
        newRulesCommand.addRule(newRule)
        //
        //  if ListPages.ActivePage = TabSheetTable then
        //    begin
        //
        //    end
        // 	else if ListPages.ActivePage = TabSheetBrowse then
        //    begin
        //    end
        //	else if ListPages.ActivePage = TabSheetMap then
        //    begin
        //  	if lastChoice is TSVariable then
        //    	newRule.setContext(TSVariable(lastChoice).phrase)
        //  	else if lastChoice is TSRule then
        //    	newRule.setContext(TSRule(lastChoice).context.phrase);
        //    end;
        //    
        variable = usdomain.domain.world.firstSelectedVariable()
        if (variable !== null) {
            newRule.setContext(variable.phrase)
        } else if (this.rule !== null) {
            newRule.setContext(this.rule.context.phrase)
        }
        usdomain.domain.world.deselectAllExcept(newRule)
        newRule.selected = true
        usdomain.domain.worldCommandList.doCommand(newRulesCommand)
        this.editRule(newRule)
        if (trim(newRule.context.phrase) !== "") {
            this.ActiveControl = this.CommandEdit
        } else {
            this.ActiveControl = this.ContextEdit
        }
    }
    
    MenuRuleDuplicateClick(Sender: TObject): void {
        let newRule: TSRule
        let newRulesCommand: TSNewRulesCommand
        
        this.commitChangesToRule()
        if (this.rule === null) {
            return
        }
        newRulesCommand = uscommands.TSNewRulesCommand().create()
        newRulesCommand.creator = "duplicating"
        newRule = usdomain.domain.world.newRule()
        newRulesCommand.addRule(newRule)
        newRule.setContext(this.rule.context.phrase)
        newRule.setCommand(this.rule.command.phrase)
        newRule.setReply(this.rule.reply)
        newRule.setMove(this.rule.move.phrase)
        newRule.setRequirements(this.rule.requirementsString)
        newRule.setChanges(this.rule.changesString)
        usdomain.domain.world.deselectAllExcept(newRule)
        newRule.selected = true
        usdomain.domain.worldCommandList.doCommand(newRulesCommand)
        this.editRule(newRule)
        this.updateForRuleChange()
    }
    
    MenuRuleDeleteClick(Sender: TObject): void {
        this.commitChangesToRule()
        if ((this.rule !== null) && (this.rule.selected)) {
            this.editRule(null)
        }
        usdomain.domain.world.deleteSelectedRules()
        this.previousChoice = null
        this.lastChoice = null
        this.updateForRuleChange()
    }
    
    // -------------------------------------------------------------- @Display menu
    MenuMapsShowCommandsClick(Sender: TObject): void {
        this.commitChangesToRule()
        usdomain.domain.options.showCommandsInMap = !usdomain.domain.options.showCommandsInMap
        this.MenuMapsShowCommands.checked = usdomain.domain.options.showCommandsInMap
        if (!this.switchToPage(this.TabSheetMap)) {
            this.MapPaintBoxChanged()
        }
    }
    
    MenuMapFontClick(Sender: TObject): void {
        this.switchToPage(this.TabSheetMap)
        this.FontDialog.Font = this.MapImage.Canvas.Font
        if (this.FontDialog.Execute()) {
            this.MapImage.Canvas.Font = this.FontDialog.Font
            this.MapPaintBoxChanged()
            //canvas.
            this.FirstListBox.Font = this.FontDialog.Font
            //canvas.
            this.SecondListBox.Font = this.FontDialog.Font
            //canvas.
            this.RuleGrid.Font = this.FontDialog.Font
        }
    }
    
    MenuDisplayShowButtonBarClick(Sender: TObject): void {
        this.commitChangesToRule()
        usdomain.domain.options.showButtonBar = !usdomain.domain.options.showButtonBar
        this.MenuDisplayShowButtonBar.checked = usdomain.domain.options.showButtonBar
        this.PanelButtonBar.Visible = usdomain.domain.options.showButtonBar
    }
    
    MenuOptionsShowRuleEditorClick(Sender: TObject): void {
        this.commitChangesToRule()
        usdomain.domain.options.showRuleEditor = !usdomain.domain.options.showRuleEditor
        this.MenuOptionsShowRuleEditor.checked = usdomain.domain.options.showRuleEditor
        if (usdomain.domain.options.showRuleEditor) {
            this.PanelEditor.Height = usdomain.domain.options.editorPanelEditorHeight
            if (this.PanelEditor.Height === 1) {
                this.PanelEditor.Height = 2
            }
            this.PanelEditor.Enabled = true
        } else {
            usdomain.domain.options.editorPanelEditorHeight = this.PanelEditor.Height
            this.PanelEditor.Height = 1
            this.PanelEditor.Enabled = false
        }
        this.Resize()
    }
    
    // ---------------------------------------------------------------- @Tools menu
    MenuToolsSearchClick(Sender: TObject): void {
        this.commitChangesToRule()
        this.FindDialog.execute
    }
    
    FindDialogFind(Sender: TObject): void {
        if (this.FindDialog.findText !== "") {
            this.searchForAndSelectRule(this.FindDialog.findText, !(delphi_compatability.TFindOption.frMatchCase in this.FindDialog.options), delphi_compatability.TFindOption.frDown in this.FindDialog.options)
        }
    }
    
    MenuMapsQuickContextsClick(Sender: TObject): void {
        this.commitChangesToRule()
        this.switchToPage(this.TabSheetMap)
        if (uscontextwizard.ContextWizardForm.initialize()) {
            uscontextwizard.ContextWizardForm.ShowModal()
        }
    }
    
    MenuMapLinkWizardClick(Sender: TObject): void {
        this.commitChangesToRule()
        this.switchToPage(this.TabSheetMap)
        if (uslinkwizard.LinkWizardForm.initialize()) {
            uslinkwizard.LinkWizardForm.ShowModal()
        }
    }
    
    MenuMapQuickCommandsClick(Sender: TObject): void {
        this.commitChangesToRule()
        this.switchToPage(this.TabSheetMap)
        if (uscommandwizard.CommandWizardForm.initialize()) {
            uscommandwizard.CommandWizardForm.ShowModal()
        }
    }
    
    MenuEditLogFileClick(Sender: TObject): void {
        this.commitChangesToRule()
        uschangelog.ChangeLogForm.loadChangeLog()
        uschangelog.ChangeLogForm.Show()
        uschangelog.ChangeLogForm.Invalidate()
        uschangelog.ChangeLogForm.Refresh()
        uschangelog.ChangeLogForm.scrollLogEndIntoView()
    }
    
    // ----------------------------------------------------------------- @Help menu
    MenuHelpRegisterClick(Sender: TObject): void {
        this.commitChangesToRule()
        usconsoleform.ConsoleForm.MenuHelpRegisterClick(Sender)
    }
    
    MenuHelpAboutClick(Sender: TObject): void {
        this.commitChangesToRule()
        usconsoleform.ConsoleForm.MenuHelpAboutClick(Sender)
    }
    
    // ---------------------------------------------------------------- @Button bar
    MenuRuleRaiseClick(Sender: TObject): void {
        this.commitChangesToRule()
        this.switchToPage(this.TabSheetTable)
        usdomain.domain.world.raiseSelectedRules()
    }
    
    MenuRuleLowerClick(Sender: TObject): void {
        this.commitChangesToRule()
        this.switchToPage(this.TabSheetTable)
        usdomain.domain.world.lowerSelectedRules()
    }
    
    NewRuleButtonClick(Sender: TObject): void {
        this.MenuRuleNewClick(this)
    }
    
    DuplicateRuleButtonClick(Sender: TObject): void {
        this.MenuRuleDuplicateClick(this)
    }
    
    DeleteRuleButtonClick(Sender: TObject): void {
        this.MenuRuleDeleteClick(this)
    }
    
    MoveUpButtonClick(Sender: TObject): void {
        this.MenuRuleRaiseClick(this)
    }
    
    MoveDownButtonClick(Sender: TObject): void {
        this.MenuRuleLowerClick(this)
    }
    
    // ------------------------------------------------------------------ @Updating
    updateForChangeToDomainOptions(): void {
        if (usdomain.domain.options.tableFontName !== "") {
            // table -- also set default row height for table
            this.RuleGrid.Font.Name = usdomain.domain.options.tableFontName
        }
        if (usdomain.domain.options.tableFontSize > 0) {
            this.RuleGrid.Font.Size = usdomain.domain.options.tableFontSize
        }
        this.RuleGrid.Canvas.Font = this.RuleGrid.Font
        this.RuleGrid.DefaultRowHeight = this.RuleGrid.Canvas.TextHeight("W") + 2
        if (usdomain.domain.options.mapFontName !== "") {
            // map
            this.MapImage.Canvas.Font.Name = usdomain.domain.options.mapFontName
        }
        if (usdomain.domain.options.mapFontSize > 0) {
            this.MapImage.Canvas.Font.Size = usdomain.domain.options.mapFontSize
        }
        if (usdomain.domain.options.browserFontName !== "") {
            // browser
            this.FirstListBox.Font.Name = usdomain.domain.options.browserFontName
        }
        if (usdomain.domain.options.browserFontSize > 0) {
            this.FirstListBox.Font.Size = usdomain.domain.options.browserFontSize
        }
        this.SecondListBox.Font = this.FirstListBox.Font
        // options menu
        this.MenuDisplayShowButtonBar.checked = usdomain.domain.options.showButtonBar
        this.PanelButtonBar.Visible = usdomain.domain.options.showButtonBar
        if (this.MenuOptionsShowRuleEditor.checked !== usdomain.domain.options.showRuleEditor) {
            this.MenuOptionsShowRuleEditor.checked = usdomain.domain.options.showRuleEditor
            if (usdomain.domain.options.showRuleEditor) {
                this.PanelEditor.Height = 2
                this.PanelEditor.Enabled = true
            } else {
                this.PanelEditor.Height = 1
                this.PanelEditor.Enabled = false
            }
            this.Resize()
        }
        this.MenuMapsShowCommands.checked = usdomain.domain.options.showCommandsInMap
        if ((usdomain.domain.options.buttonSymbols !== this.buttonSymbols) || (this.startingUp)) {
            this.buttonSymbols = usdomain.domain.options.buttonSymbols
            usconsoleform.ConsoleForm.setButtonGlyphs(usdomain.domain.options.buttonSymbols)
            this.setButtonGlyphs()
            this.Invalidate()
            // replyPicture.visible := not buttonSymbols;
            this.setOrganizeByField(usdomain.domain.options.browseBy)
        }
        if (usdomain.domain.options.browseBy !== this.organizeByField) {
            this.setOrganizeByField(usdomain.domain.options.browseBy)
        }
        // updating
        this.updateForRuleChange()
        this.MapPaintBoxChanged()
        this.adjustScrollBars()
        this.updateViews()
    }
    
    updateMenus(): void {
        // unfinished - maybe want better save as testing
        this.MenuFileSaveWorld.enabled = usdomain.domain.isWorldFileChanged()
        this.MenuFileSaveWorldAs.enabled = true
        this.updateMenusForUndoRedo()
    }
    
    updateViews(): void {
        if (delphi_compatability.Application.terminated) {
            return
        }
        this.updateMenus()
        usconsoleform.ConsoleForm.updateViews()
        uspictureform.PictureForm.updateViews()
    }
    
    updateForRuleChange(): void {
        this.RuleGrid.RowCount = usdomain.domain.world.rules.Count + 1
        if (this.RuleGrid.RowCount > 1) {
            this.RuleGrid.FixedRows = 1
        }
        this.RuleGrid.Invalidate()
        usconsoleform.ConsoleForm.updateVariables()
        usconsoleform.ConsoleForm.VariablesListBox.Invalidate()
        this.MapPaintBoxChanged()
        this.setOrganizeByField(this.organizeByField)
        //if (domain.world.focus = nil) or (domain.world.focus = domain.world.emptyEntry) then
        //  domain.world.setInitialFocus;
        usdomain.domain.world.updateAvailable()
        usconsoleform.ConsoleForm.updateViews()
        this.updateRuleNumberLabel()
    }
    
    updateMenusForUndoRedo(): void {
        if (usdomain.domain.worldCommandList.isUndoEnabled()) {
            this.MenuEditUndo.enabled = true
            this.MenuEditUndo.caption = "&Undo " + usdomain.domain.worldCommandList.undoDescription()
        } else {
            this.MenuEditUndo.enabled = false
            this.MenuEditUndo.caption = "Can't undo"
        }
        if (usdomain.domain.worldCommandList.isRedoEnabled()) {
            this.MenuEditRedo.enabled = true
            this.MenuEditRedo.caption = "&Redo " + usdomain.domain.worldCommandList.redoDescription()
        } else {
            this.MenuEditRedo.enabled = false
            this.MenuEditRedo.caption = "Can't redo"
        }
    }
    
    // -------------------------------------------------------------------- @Events
    ListBoxMouseUp(Sender: TObject, Button: TMouseButton, Shift: TShiftState, X: int, Y: int): void {
        let desiredStateWrapper: TSDesiredStateVariableWrapper
        let wrapperObject: TObject
        let listBox: TListBox
        let oldIndex: int
        let oldTopIndex: int
        
        this.lastClickAtLeft = false
        listBox = Sender
        if ((listBox.ItemIndex < 0) || (listBox.ItemIndex >= listBox.Items.Count)) {
            return
        }
        if (listBox.ItemIndex === listBox.Items.Count - 1) {
            this.ListBoxDblClick(Sender)
        } else if (X < 10) {
            oldIndex = listBox.ItemIndex
            oldTopIndex = listBox.TopIndex
            this.lastClickAtLeft = true
            wrapperObject = listBox.Items.Objects[listBox.ItemIndex]
            desiredStateWrapper = wrapperObject
            // temporarily invert to get new display string
            desiredStateWrapper.invertDesiredState()
            listBox.Items[listBox.ItemIndex] = desiredStateWrapper.displayString()
            listBox.Items.Objects[listBox.ItemIndex] = desiredStateWrapper
            // restore to current state
            desiredStateWrapper.invertDesiredState()
            // now create command for undo
            this.commitChangesToRule()
            listBox.ItemIndex = oldIndex
            listBox.TopIndex = oldTopIndex
        }
    }
    
    ListBoxDragOver(Sender: TObject, Source: TObject, X: int, Y: int, State: TDragState, Accept: boolean): void {
        //
        return Accept
    }
    
    EditDragDrop(Sender: TObject, Source: TObject, X: int, Y: int): void {
        if (Source === this.MapImage) {
            if (this.lastChoice === null) {
                return
            }
            if (this.lastChoice instanceof usworld.TSRule) {
                (Sender).Text = usworld.TSRule(this.lastChoice).command.phrase
            } else {
                (Sender).Text = usworld.TSVariable(this.lastChoice).phrase
            }
        } else if (Source === this.FirstListBox) {
            if (this.FirstListBox.ItemIndex < 0) {
                return
            }
            (Sender).Text = this.FirstListBox.Items[this.FirstListBox.ItemIndex]
        }
        this.commitChangesToRule()
    }
    
    listBoxNewStatement(listBox: TListBox, newStatement: string): void {
        if (listBox === this.RequirementsListBox) {
            this.rule.setRequirements(newStatement)
            this.fillListBox(listBox, this.rule.requirements)
        } else if (listBox === this.ChangesListBox) {
            this.rule.setChanges(newStatement)
            this.fillListBox(listBox, this.rule.changes)
        }
        //not undoable
        usdomain.domain.worldChangeDone()
    }
    
    ListBoxDragDrop(Sender: TObject, Source: TObject, X: int, Y: int): void {
        let theText: string
        let listBox: TListBox
        
        listBox = Sender
        if (Source === this.MapImage) {
            if (this.lastChoice === null) {
                return
            }
            if (this.lastChoice instanceof usworld.TSRule) {
                theText = usworld.TSRule(this.lastChoice).command.phrase
            } else {
                theText = usworld.TSVariable(this.lastChoice).phrase
            }
            listBox.Items.InsertObject(listBox.Items.Count - 1, theText, null)
            this.commitChangesToRule()
        } else if (Source === this.FirstListBox) {
            if (this.FirstListBox.ItemIndex < 0) {
                return
            }
            theText = this.FirstListBox.Items[this.FirstListBox.ItemIndex]
            listBox.Items.InsertObject(listBox.Items.Count - 1, theText, null)
            this.commitChangesToRule()
        } else {
            return
        }
    }
    
    ListBoxKeyUp(Sender: TObject, Key: byte, Shift: TShiftState): void {
        let listBox: TListBox
        
        listBox = Sender
        if ((Key === delphi_compatability.VK_DELETE) || (Key === delphi_compatability.VK_BACK)) {
            if (listBox.ItemIndex < 0) {
                return Key
            }
            if (listBox.ItemIndex >= listBox.Items.Count - 1) {
                return Key
            }
            listBox.Items.Delete(listBox.ItemIndex)
            this.commitChangesToRule()
            Key = 0
        } else if ((Key === delphi_compatability.VK_RETURN)) {
            Key = 0
            if (!this.ignoreNextEnter) {
                this.ListBoxDblClick(Sender)
            }
            this.ignoreNextEnter = false
        }
        return Key
    }
    
    positionEditForListBox(edit: TEdit, listBox: TListBox): void {
        let tildeWidth: int
        
        edit.Top = listBox.Top + 2 + (listBox.ItemIndex - listBox.TopIndex) * listBox.ItemHeight
        tildeWidth = listBox.Canvas.TextWidth("~")
        edit.Left = listBox.Left + 2 + tildeWidth
        edit.Width = listBox.Width - 4 - tildeWidth
        edit.Height = listBox.ItemHeight + 2
        edit.Show()
        this.FocusControl(edit)
    }
    
    ListBoxDblClick(Sender: TObject): void {
        let desiredStateWrapper: TSDesiredStateVariableWrapper
        let listBox: TListBox
        let edit: TEdit
        let wrapperObject: TObject
        
        if (this.lastClickAtLeft) {
            return
        }
        this.indexEdited = -1
        listBox = Sender
        if (Sender === this.RequirementsListBox) {
            edit = this.RequirementsEdit
        } else if (Sender === this.ChangesListBox) {
            edit = this.ChangesEdit
        } else {
            return
        }
        if ((listBox.ItemIndex < 0) || (listBox.ItemIndex >= listBox.Items.Count)) {
            return
        }
        if (listBox.ItemIndex < listBox.Items.Count - 1) {
            wrapperObject = listBox.Items.Objects[listBox.ItemIndex]
            desiredStateWrapper = wrapperObject
            edit.Text = desiredStateWrapper.variable.phrase
        } else {
            edit.Text = ""
        }
        this.indexEdited = listBox.ItemIndex
        this.positionEditForListBox(edit, listBox)
    }
    
    ListBoxEditExit(Sender: TObject): void {
        let oldIndex: int
        let desiredStateWrapper: TSDesiredStateVariableWrapper
        let edit: TEdit
        let listBox: TListBox
        let oldTopIndex: int
        
        edit = Sender
        edit.Hide()
        if (edit === this.RequirementsEdit) {
            listBox = this.RequirementsListBox
        } else if (edit === this.ChangesEdit) {
            listBox = this.ChangesListBox
        } else {
            return
        }
        if ((this.indexEdited < 0) || (this.indexEdited > listBox.Items.Count - 1)) {
            return
        }
        if ((this.indexEdited === listBox.Items.Count - 1)) {
            if ((trim(edit.Text) !== "")) {
                listBox.Items[listBox.Items.Count - 1] = edit.Text
                oldIndex = this.indexEdited
                oldTopIndex = listBox.TopIndex
                listBox.Items.Add("")
                this.commitChangesToRule()
                //listBox.itemIndex := index;
                //if index - oldTopIndex + 1 > (listBox.clientHeight + listBox.itemHeight - 1) div listBox.itemHeight then
                //	listBox.TopIndex := oldTopIndex + 1
                //else
                listBox.ItemIndex = oldIndex
                listBox.TopIndex = oldTopIndex
            }
        } else {
            oldIndex = this.indexEdited
            oldTopIndex = listBox.TopIndex
            desiredStateWrapper = listBox.Items.Objects[oldIndex]
            if (desiredStateWrapper !== null) {
                listBox.Items[oldIndex] = desiredStateWrapper.displayLeader() + edit.Text
            }
            this.commitChangesToRule()
            listBox.ItemIndex = oldIndex
            listBox.TopIndex = oldTopIndex
        }
        this.indexEdited = -1
        edit.Text = ""
    }
    
    ListBoxEditKeyPress(Sender: TObject, Key: char): void {
        let edit: TEdit
        let listBox: TListBox
        let desiredStateWrapper: TSDesiredStateVariableWrapper
        
        edit = Sender
        if (edit === this.RequirementsEdit) {
            listBox = this.RequirementsListBox
        } else if (edit === this.ChangesEdit) {
            listBox = this.ChangesListBox
        } else {
            return Key
        }
        if ((Key === String.fromCharCode(13))) {
            //enter
            Key = String.fromCharCode(0)
            this.FocusControl(listBox)
            this.ignoreNextEnter = true
        } else if (Key === String.fromCharCode(27)) {
            //escape
            Key = String.fromCharCode(0)
            desiredStateWrapper = null
            if ((this.indexEdited >= 0) && (this.indexEdited < listBox.Items.Count)) {
                desiredStateWrapper = listBox.Items.Objects[this.indexEdited]
            }
            if (desiredStateWrapper !== null) {
                edit.Text = desiredStateWrapper.variable.phrase
            } else {
                edit.Text = ""
            }
            this.FocusControl(listBox)
        }
        //
        //  if Key = Char(1) then
        //    begin
        //    if (ExtraChangesEntry.items.count = 0) or
        //    		(NewExtraChanges.Text = ExtraChangesEntry.items[0]) then
        //    	begin
        //    	NewExtraChanges.Text := '';
        //  		ResponseEditorForm.FocusControl(ExtraChangesEntry);
        //    	Key := Char(0);
        //    	end;
        //    end;
        // 
        return Key
    }
    
    ListBoxExit(Sender: TObject): void {
        (Sender).itemIndex = -1
    }
    
    // ------------------------------------------------------------------ @Commands
    commandChangedNotification(command: KfCommand, state: KfCommandChangeType): void {
        switch (state) {
            case ucommand.KfCommandChangeType.commandDone:
                usdomain.domain.worldChangeDone()
                break
            case ucommand.KfCommandChangeType.commandUndone:
                usdomain.domain.worldChangeUndone()
                break
    }
    
    trackLastCommand(): void {
        this.editRule(this.lastCommand)
        this.updateForRuleChange()
    }
    
    EditEnterCommit(Sender: TObject): void {
        this.commitChangesToRule()
    }
    
    selectEditorField(field: int): void {
        if (this.rule === null) {
            return
        }
        switch (field) {
            case usworld.kRuleContext:
                this.ContextEdit.SetFocus()
                this.ContextEdit.SelStart = 0
                this.ContextEdit.SelLength = len(this.ContextEdit.Text)
                break
            case usworld.kRuleCommand:
                this.CommandEdit.SetFocus()
                this.CommandEdit.SelStart = 0
                this.CommandEdit.SelLength = len(this.CommandEdit.Text)
                break
            case usworld.kRuleReply:
                this.ReplyMemo.SetFocus()
                this.ReplyMemo.SelStart = 0
                this.ReplyMemo.SelLength = len(this.ReplyMemo.Text)
                break
            case usworld.kRuleMove:
                this.MoveEdit.SetFocus()
                this.MoveEdit.SelStart = 0
                this.MoveEdit.SelLength = len(this.MoveEdit.Text)
                break
            case usworld.kRuleRequirements:
                this.RequirementsListBox.SetFocus()
                if (this.RequirementsListBox.Items.Count > 0) {
                    this.RequirementsListBox.ItemIndex = 0
                }
                break
            case usworld.kRuleChanges:
                this.ChangesListBox.SetFocus()
                if (this.ChangesListBox.Items.Count > 0) {
                    this.ChangesListBox.ItemIndex = 0
                }
                break
    }
    
    updateForFieldChange(fieldType: int): void {
        if (this.rule === null) {
            return
        }
        switch (fieldType) {
            case usworld.kRuleContext:
                this.ContextEdit.Text = this.rule.context.phrase
                break
            case usworld.kRuleCommand:
                this.CommandEdit.Text = this.rule.command.phrase
                break
            case usworld.kRuleReply:
                this.ReplyMemo.Text = this.rule.reply
                break
            case usworld.kRuleMove:
                this.MoveEdit.Text = this.rule.move.phrase
                break
            case usworld.kRuleRequirements:
                this.fillListBox(this.RequirementsListBox, this.rule.requirements)
                break
            case usworld.kRuleChanges:
                this.fillListBox(this.ChangesListBox, this.rule.changes)
                break
    }
    
    editRule(rule: TSRule): void {
        this.commitChangesToRule()
        this.rule = rule
        this.loadAllRuleFields()
    }
    
    loadAllRuleFields(): void {
        let newIndex: int
        
        usdomain.domain.beginUpdate()
        if (this.rule !== null) {
            // if cacheInvalid then fillChoices;
            this.ContextEdit.Text = this.rule.context.phrase
            this.CommandEdit.Text = this.rule.command.phrase
            this.MoveEdit.Text = this.rule.move.phrase
            this.ReplyMemo.Text = this.rule.reply
            this.fillListBox(this.RequirementsListBox, this.rule.requirements)
            this.fillListBox(this.ChangesListBox, this.rule.changes)
        } else {
            this.ContextEdit.Text = ""
            this.CommandEdit.Text = ""
            this.MoveEdit.Text = ""
            this.ReplyMemo.Text = ""
            this.RequirementsListBox.Clear()
            this.ChangesListBox.Clear()
        }
        this.updateRuleNumberLabel()
        this.setEnabledForControl(this.ContextEdit, this.rule !== null)
        this.setEnabledForControl(this.CommandEdit, this.rule !== null)
        this.setEnabledForControl(this.MoveEdit, this.rule !== null)
        this.setEnabledForControl(this.ReplyMemo, this.rule !== null)
        this.setEnabledForControl(this.RequirementsListBox, this.rule !== null)
        this.setEnabledForControl(this.ChangesListBox, this.rule !== null)
        //RecordSelectionInformation(selectionInformation, self.ActiveControl as TWinControl);
        usdomain.domain.endUpdate()
        newIndex = this.SecondListBox.Items.IndexOfObject(this.rule)
        if (this.SecondListBox.ItemIndex !== newIndex) {
            this.SecondListBox.ItemIndex = newIndex
        }
    }
    
    updateRuleNumberLabel(): void {
        if (this.rule !== null) {
            this.RuleNumberLabel.Caption = "#" + IntToStr(usdomain.domain.world.rules.IndexOf(this.rule) + 1)
        } else {
            this.RuleNumberLabel.Caption = ""
        }
    }
    
    setEnabledForControl(control: TWinControl, enable: boolean): void {
        let aColor: TColor
        
        if (control === null) {
            return
        }
        if (enable) {
            aColor = delphi_compatability.clWindow
        } else {
            aColor = UNRESOLVED.clBtnFace
        }
        if (control instanceof delphi_compatability.TEdit) {
            (control).Color = aColor
        } else if (control instanceof delphi_compatability.TMemo) {
            (control).Color = aColor
        } else if (control instanceof delphi_compatability.TListBox) {
            (control).Color = aColor
        }
        control.Enabled = enable
    }
    
    // --------------------------------------------------------------------- @Table
    RuleGridDrawCell(Sender: TObject, Col: int, Row: int, Rect: TRect, State: TGridDrawState): void {
        let cString: char[] /* 256 + 1 */
        let selected: boolean
        let focused: boolean
        let ruleString: string
        let rule: TSRule
        
        rule = null
        if ((Row > 0) && (Row <= usdomain.domain.world.rules.Count)) {
            rule = usworld.TSRule(usdomain.domain.world.rules[Row - 1])
        }
        if (rule === null) {
            if (Row !== 0) {
                return
            }
            // draw first row headers
            this.RuleGrid.Canvas.Brush.Color = delphi_compatability.clLtGray
            this.RuleGrid.Canvas.Font.Color = UNRESOLVED.clWindowText
            this.RuleGrid.Canvas.Font.Style = {}
            this.RuleGrid.Canvas.FillRect(Rect)
            ruleString = usworld.TSRule.headerForField(Col)
            Rect.left = Rect.left + 5
            Rect.right = Rect.right - 2
            UNRESOLVED.StrPLCopy(cString, ruleString, 255)
            UNRESOLVED.DrawText(this.RuleGrid.Canvas.Handle, cString, len(cString), Rect, delphi_compatability.DT_LEFT || delphi_compatability.DT_NOPREFIX)
            return
        }
        // draw rule cell text
        selected = rule.selected
        focused = rule === this.rule
        setCanvasColorsForSelection(this.RuleGrid.Canvas, selected, focused, false)
        this.RuleGrid.Canvas.FillRect(Rect)
        ruleString = rule.getTextForField(Col)
        UNRESOLVED.StrPLCopy(cString, ruleString, 255)
        Rect.left = Rect.left + 5
        Rect.right = Rect.right - 2
        UNRESOLVED.DrawText(this.RuleGrid.Canvas.Handle, cString, len(cString), Rect, delphi_compatability.DT_LEFT || delphi_compatability.DT_NOPREFIX)
    }
    
    RuleGridMouseUp(Sender: TObject, Button: TMouseButton, Shift: TShiftState, X: int, Y: int): void {
        //	if ssCtrl in Shift then
        // 	begin
        //	RuleGrid.beginDrag(false);
        //		RuleGrid.FGridState := gsNormal;
        //		end;
        //  self.commitChangesToRule;
        //  if RuleGrid.row = 0 then exit;
        //  if RuleGrid.row > domain.world.rules.count then exit;
        //  self.editRule(TSRule(domain.world.rules[RuleGrid.row - 1])); 
    }
    
    RuleGridMouseDown(Sender: TObject, Button: TMouseButton, Shift: TShiftState, X: int, Y: int): void {
        let rule: TSRule
        let mustRedraw: boolean
        let index: int
        let i: int
        
        if (this.RuleGrid.Row === 0) {
            //if ssCtrl in Shift then
            // 	begin
            //	RuleGrid.beginDrag(true);
            //		RuleGrid.FGridState := gsNormal;
            //	end;
            return
        }
        if (this.RuleGrid.Row > usdomain.domain.world.rules.Count) {
            return
        }
        //RuleGrid.beginDrag(false);
        this.commitChangesToRule()
        index = this.RuleGrid.Row - 1
        rule = usworld.TSRule(usdomain.domain.world.rules[index])
        mustRedraw = false
        if ((delphi_compatability.TShiftStateEnum.ssShift in Shift)) {
            if ((this.lastSingleRuleIndex >= 0) && (this.lastSingleRuleIndex <= usdomain.domain.world.rules.Count - 1) && (this.lastSingleRuleIndex !== index)) {
                // shift
                usdomain.domain.world.deselectAllExcept(rule)
                if (this.lastSingleRuleIndex < index) {
                    for (i = this.lastSingleRuleIndex; i <= index; i++) {
                        usworld.TSRule(usdomain.domain.world.rules[i]).selected = true
                    }
                } else if (this.lastSingleRuleIndex > index) {
                    for (i = this.lastSingleRuleIndex; i >= index; i--) {
                        usworld.TSRule(usdomain.domain.world.rules[i]).selected = true
                    }
                }
                mustRedraw = true
            }
            // control
        } else if ((delphi_compatability.TShiftStateEnum.ssCtrl in Shift)) {
            rule.selected = !rule.selected
            // some sort of bug in updating occasionally otherwise
            mustRedraw = true
            // just click
        } else {
            if (!rule.selected) {
                mustRedraw = usdomain.domain.world.deselectAllExcept(rule)
                rule.selected = true
                this.lastSingleRuleIndex = index
            } else {
                // do nothing except maybe drag...
            }
        }
        if (rule.selected && (this.rule !== rule) && !(delphi_compatability.TShiftStateEnum.ssCtrl in Shift) && !(delphi_compatability.TShiftStateEnum.ssShift in Shift)) {
            this.editRule(rule)
            mustRedraw = true
        }
        if (mustRedraw) {
            this.RuleGrid.Invalidate()
        }
    }
    
    ListPagesChange(Sender: TObject): void {
        if (this.ListPages.ActivePage === this.TabSheetTable) {
            if ((this.rule === null) || !this.rule.selected) {
                // pass
            } else {
                //domain.world.deselectAllExcept(nil) pdf change - maybe contexts selected in map
                this.scrollGridSelectionsIntoView(kFromTop)
                this.RuleGrid.Invalidate()
            }
        } else if (this.ListPages.ActivePage === this.TabSheetMap) {
            this.MapPaintBoxChanged()
            this.scrollMapSelectionIntoView()
        } else if (this.ListPages.ActivePage === this.TabSheetBrowse) {
            this.setOrganizeByField(this.organizeByField)
        }
    }
    
    EditDragOver(Sender: TObject, Source: TObject, X: int, Y: int, State: TDragState, Accept: boolean): void {
        //
        return Accept
    }
    
    TabSheetMapDragOver(Sender: TObject, Source: TObject, X: int, Y: int, State: TDragState, Accept: boolean): void {
        //
        return Accept
    }
    
    TabSheetMapDragDrop(Sender: TObject, Source: TObject, X: int, Y: int): void {
        //
    }
    
    scrollGridSelectionsIntoView(direction: boolean): void {
        let rule: TSRule
        let firstSelectedRuleIndex: int
        let i: int
        
        firstSelectedRuleIndex = -1
        if (direction === kFromBottom) {
            for (i = usdomain.domain.world.rules.Count - 1; i >= 0; i--) {
                rule = usworld.TSRule(usdomain.domain.world.rules[i])
                if (!rule.selected) {
                    continue
                }
                firstSelectedRuleIndex = i
                break
            }
        } else {
            for (i = 0; i <= usdomain.domain.world.rules.Count - 1; i++) {
                rule = usworld.TSRule(usdomain.domain.world.rules[i])
                if (!rule.selected) {
                    continue
                }
                firstSelectedRuleIndex = i
                break
            }
        }
        if (firstSelectedRuleIndex === -1) {
            return
        }
        // to account for header
        firstSelectedRuleIndex += 1
        if ((this.RuleGrid.TopRow <= firstSelectedRuleIndex) && (this.RuleGrid.TopRow + this.RuleGrid.VisibleRowCount > firstSelectedRuleIndex)) {
            return
        }
        if (direction === kFromBottom) {
            this.RuleGrid.TopRow = localIntMax(1, firstSelectedRuleIndex - this.RuleGrid.VisibleRowCount + 1)
        } else {
            this.RuleGrid.TopRow = localIntMax(1, firstSelectedRuleIndex)
        }
    }
    
    //graphs
    // ----------------------------------------------------------------------- @Map
    currentGraphView(): TSMapView {
        let result = new TSMapView()
        result = usdomain.domain.mapView
        return result
    }
    
    mapChangedNotification(command: KfCommand, state: KfCommandChangeType): void {
        this.MapPaintBoxChanged()
    }
    
    MapPaintBoxChanged(): void {
        let mapView: TSMapView
        let displayOptions: TSVariableDisplayOptions
        let i: int
        
        if (this.ListPages.ActivePage !== this.TabSheetMap) {
            return
        }
        mapView = this.currentGraphView()
        if (mapView === null) {
            return
        }
        for (i = 0; i <= 5; i++) {
            displayOptions[i] = false
        }
        displayOptions[usworld.kRuleContext] = true
        displayOptions[usworld.kRuleMove] = true
        displayOptions[usworld.kRuleCommand] = this.MenuMapsShowCommands.checked
        // clBtnFace;
        this.MapImage.Picture.Bitmap.Canvas.Brush.Color = delphi_compatability.clWhite
        this.MapImage.Picture.Bitmap.Canvas.FillRect(Rect(0, 0, this.MapImage.Picture.Bitmap.Width, this.MapImage.Picture.Bitmap.Height))
        mapView.scroll = Point(-this.MapScrollBarHorizontal.Position, -this.MapScrollBarVertical.Position)
        mapView.displayOn(this.MapImage.Picture.Bitmap.Canvas, displayOptions, this.lastChoice, this.previousChoice)
        this.MapImage.Invalidate()
    }
    
    ListPagesDragOver(Sender: TObject, Source: TObject, X: int, Y: int, State: TDragState, Accept: boolean): void {
        //
        return Accept
    }
    
    MapListChange(Sender: TObject): void {
        this.lastChoice = null
        this.previousChoice = null
        this.MapPaintBoxChanged()
        this.adjustScrollBars()
    }
    
    lastChoiceText(): string {
        let result = ""
        result = ""
        if (this.lastChoice === null) {
            return result
        }
        if (this.lastChoice instanceof usworld.TSVariable) {
            result = this.lastChoice.displayName()
            return result
        }
        result = (this.lastChoice).command.phrase
        return result
    }
    
    makeChoice(choice: TSDraggableObject, multiSelect: boolean): boolean {
        let result = false
        //whether must redraw
        result = false
        if (multiSelect) {
            if (choice !== null) {
                choice.selected = !choice.selected
            }
            result = true
        } else {
            if ((choice === null) || !choice.selected) {
                result = usdomain.domain.world.deselectAllExcept(choice)
                if (choice !== null) {
                    choice.selected = true
                }
            } else {
                // do nothing except maybe drag...
            }
        }
        if (this.lastChoice === choice) {
            return result
        }
        result = true
        if ((choice !== null) && choice.selected) {
            this.previousChoice = this.lastChoice
            this.lastChoice = choice
            if (this.previousChoice instanceof usworld.TSRule) {
                this.previousChoice = choice
            }
            if (this.lastChoice instanceof usworld.TSRule) {
                this.previousChoice = choice
            }
        } else if ((choice !== null) && !choice.selected) {
            if (this.previousChoice === choice) {
                this.previousChoice = null
            }
            if (this.lastChoice === choice) {
                this.lastChoice = null
            }
        }
        return result
    }
    
    XorRect(canvas: TCanvas, rect: TRect): void {
        let oldMode: TPenMode
        
        oldMode = canvas.Pen.Mode
        canvas.Brush.Color = delphi_compatability.clNone
        canvas.Pen.Mode = delphi_compatability.TFPPenMode.pmXor
        canvas.Pen.Color = delphi_compatability.clWhite
        canvas.Pen.Style = delphi_compatability.TFPPenStyle.psDot
        //FIX unresolved WITH expression: Rect
        //canvas.Pen.width := 2;
        canvas.Rectangle(this.Left, this.Top, UNRESOLVED.right, UNRESOLVED.bottom)
        //canvas.Pen.width := 1;
        canvas.Pen.Mode = oldMode
        canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid
    }
    
    MapImageMouseDown(Sender: TObject, Button: TMouseButton, Shift: TShiftState, X: int, Y: int): void {
        let newCommand: TSMapDragCommand
        let draggedNode: TSDraggableObject
        let mapView: TSMapView
        let displayOptions: TSVariableDisplayOptions
        let i: int
        let multipleSelect: boolean
        let showString: string
        let textSize: TPoint
        let centerPosition: TPoint
        
        if (delphi_compatability.Application.terminated) {
            return
        }
        this.commitChangesToRule()
        this.lastMapMouseDownPosition = Point(X + this.MapScrollBarHorizontal.Position, Y + this.MapScrollBarVertical.Position)
        mapView = this.currentGraphView()
        if (mapView === null) {
            return
        }
        this.FocusControl(this.PanelMap)
        for (i = 0; i <= 5; i++) {
            displayOptions[i] = false
        }
        displayOptions[usworld.kRuleContext] = true
        displayOptions[usworld.kRuleMove] = true
        displayOptions[usworld.kRuleCommand] = this.MenuMapsShowCommands.checked
        draggedNode = mapView.nearestNode(Point(X + this.MapScrollBarHorizontal.Position, Y + this.MapScrollBarVertical.Position), displayOptions)
        if (Button === delphi_compatability.TMouseButton.mbRight) {
            if (draggedNode !== null) {
                showString = draggedNode.displayName()
                centerPosition = Point(draggedNode.center().X - this.MapScrollBarHorizontal.Position, draggedNode.center().Y - this.MapScrollBarVertical.Position)
            } else {
                showString = "new item"
                centerPosition = Point(X, Y)
            }
            this.MapImage.Canvas.Brush.Color = delphi_compatability.clAqua
            this.MapImage.Canvas.Pen.Style = delphi_compatability.TFPPenStyle.psSolid
            this.MapImage.Canvas.Font.Style = {UNRESOLVED.fsBold, }
            textSize = Point(this.MapImage.Canvas.TextWidth(showString), this.MapImage.Canvas.TextHeight("W"))
            this.MapImage.Canvas.Rectangle(centerPosition.X - textSize.X / 2 - 2, centerPosition.Y - textSize.Y / 2 - 2, centerPosition.X + textSize.X / 2 + 2, centerPosition.Y + textSize.Y / 2 + 2)
            this.MapImage.Canvas.TextOut(centerPosition.X - textSize.X / 2, centerPosition.Y - textSize.Y / 2, showString)
            this.MapImage.Canvas.Font.Style = {}
        }
        if (Button !== delphi_compatability.TMouseButton.mbLeft) {
            return
        }
        multipleSelect = (delphi_compatability.TShiftStateEnum.ssShift in Shift)
        this.mapSelectionInProgress = false
        if (draggedNode === null) {
            this.makeChoice(null, multipleSelect)
            this.mapSelectionRect = Rect(X, Y, X, Y)
            this.XorRect(this.MapImage.Canvas, this.mapSelectionRect)
            this.mapSelectionInProgress = true
            return
        }
        //MapPaintBoxChanged;
        this.makeChoice(draggedNode, multipleSelect)
        if ((delphi_compatability.TShiftStateEnum.ssCtrl in Shift)) {
            this.MapPaintBoxChanged()
            this.MapImage.BeginDrag(true)
            return
        }
        if (!multipleSelect) {
            this.MapImageDblClick(Sender)
        }
        this.MapPaintBoxChanged()
        // finds selected nodes in domain
        newCommand = uscommands.TSMapDragCommand().create()
        newCommand.notifyProcedure = this.mapChangedNotification
        this.actionInProgress = usdomain.domain.worldCommandList.mouseDown(newCommand, Point(X, Y))
    }
    
    MapImageMouseMove(Sender: TObject, Shift: TShiftState, X: int, Y: int): void {
        if (this.actionInProgress) {
            usdomain.domain.worldCommandList.mouseMove(Point(X, Y))
        } else if (this.mapSelectionInProgress) {
            this.XorRect(this.MapImage.Canvas, this.mapSelectionRect)
            this.mapSelectionRect.Right = X
            this.mapSelectionRect.Bottom = Y
            this.XorRect(this.MapImage.Canvas, this.mapSelectionRect)
        }
    }
    
    MapImageMouseUp(Sender: TObject, Button: TMouseButton, Shift: TShiftState, X: int, Y: int): void {
        if (this.actionInProgress) {
            usdomain.domain.worldCommandList.mouseUp(Point(X, Y))
            this.actionInProgress = false
            this.adjustScrollBars()
        } else if (this.mapSelectionInProgress) {
            this.XorRect(this.MapImage.Canvas, this.mapSelectionRect)
            this.mapSelectionInProgress = false
            if (!(delphi_compatability.TShiftStateEnum.ssCtrl in Shift)) {
                usdomain.domain.world.deselectAllExcept(null)
            }
            this.mapSelectionRect = Rect(this.mapSelectionRect.Left + this.MapScrollBarHorizontal.Position, this.mapSelectionRect.Top + this.MapScrollBarVertical.Position, this.mapSelectionRect.Right + this.MapScrollBarHorizontal.Position, this.mapSelectionRect.Bottom + this.MapScrollBarVertical.Position)
            usdomain.domain.world.selectInRectangle(this.mapSelectionRect)
            this.MapPaintBoxChanged()
        }
    }
    
    MapImageDblClick(Sender: TObject): void {
        if (this.lastChoice === null) {
            //var
            //row: integer;
            //count: integer;
            //rule: TSRule;
            //ruleIndex: integer; 
            return
        }
        if (this.lastChoice instanceof usworld.TSRule) {
            this.editRule(this.lastChoice)
        }
        //
        //  else
        //    begin
        //    count := 1;
        //    ruleIndex := domain.world.rules.indexOf(self.rule);
        //    while (count <= domain.world.rules.count) do
        //      begin
        //      row := (count + ruleIndex) mod domain.world.rules.count;
        //      rule := domain.world.rules[row];
        //      // unfinished - need to check requirements & changes
        //      if (rule.context.phrase = self.lastChoiceText) or
        //      		(rule.command.phrase = self.lastChoiceText) or
        //          (rule.move.phrase = self.lastChoiceText) then
        //        begin
        //      	self.editRule(rule);
        //        exit;
        //        end;
        //      inc(count);
        //      end;
        //    end;
        //    
    }
    
    searchForAndSelectRule(aText: string, ignoreCase: boolean, goDown: boolean): void {
        let row: int
        let count: int
        let rule: TSRule
        let ruleIndex: int
        let match: boolean
        let matchText: string
        
        count = 1
        ruleIndex = usdomain.domain.world.rules.IndexOf(this.rule)
        if (ignoreCase) {
            matchText = lowercase(aText)
        } else {
            matchText = aText
        }
        while ((count <= usdomain.domain.world.rules.Count)) {
            if (goDown) {
                row = (ruleIndex + count) % usdomain.domain.world.rules.Count
            } else {
                row = ((usdomain.domain.world.rules.Count * 2) + (ruleIndex - count)) % usdomain.domain.world.rules.Count
            }
            rule = usdomain.domain.world.rules[row]
            if (ignoreCase) {
                // unfinished - need to check requirements & changes
                match = (UNRESOLVED.pos(matchText, lowercase(rule.context.phrase)) > 0) || (UNRESOLVED.pos(matchText, lowercase(rule.command.phrase)) > 0) || (UNRESOLVED.pos(matchText, lowercase(rule.reply)) > 0) || (UNRESOLVED.pos(matchText, lowercase(rule.move.phrase)) > 0) || (UNRESOLVED.pos(matchText, lowercase(rule.requirementsString)) > 0) || (UNRESOLVED.pos(matchText, lowercase(rule.changesString)) > 0)
            } else {
                match = (UNRESOLVED.pos(matchText, rule.context.phrase) > 0) || (UNRESOLVED.pos(matchText, rule.command.phrase) > 0) || (UNRESOLVED.pos(matchText, rule.reply) > 0) || (UNRESOLVED.pos(matchText, rule.move.phrase) > 0) || (UNRESOLVED.pos(matchText, rule.requirementsString) > 0) || (UNRESOLVED.pos(matchText, rule.changesString) > 0)
            }
            if (match) {
                usdomain.domain.world.deselectAllExcept(rule)
                this.editRule(rule)
                this.updateForRuleChange()
                rule.selected = true
                this.scrollGridSelectionsIntoView(kFromBottom)
                this.MapPaintBoxChanged()
                this.scrollMapSelectionIntoView()
                return
            }
            count += 1
        }
        ShowMessage("Search string \"" + aText + "\" not found.")
    }
    
    // accomodates growth
    // accomodates growth
    adjustScrollBars(): void {
        let top: int
        let bottom: int
        let left: int
        let right: int
        let graphView: TSMapView
        let xPosition: int
        let yPosition: int
        let mapBoundsRect: TRect
        
        graphView = this.currentGraphView()
        if (graphView !== null) {
            mapBoundsRect = usdomain.domain.world.boundsRect()
            top = mapBoundsRect.Top
            left = mapBoundsRect.Left
            bottom = mapBoundsRect.Bottom
            right = mapBoundsRect.Right
            if (bottom < this.MapImage.Height) {
                bottom = this.MapImage.Height
            }
            if (right < this.MapImage.Width) {
                right = this.MapImage.Width
            }
        } else {
            top = 0
            bottom = this.MapImage.Height
            left = 0
            right = this.MapImage.Width
        }
        left = left - kLeftRightBorderSize
        right = right + kLeftRightBorderSize
        top = top - kTopBottomBorderSize
        bottom = bottom + kTopBottomBorderSize
        right = right - this.MapImage.Width
        bottom = bottom - this.MapImage.Height
        xPosition = this.MapScrollBarHorizontal.Position
        yPosition = this.MapScrollBarVertical.Position
        if (xPosition < left) {
            //if xPosition < left then xPosition := left;
            //  if xPosition > right then xPosition := right;
            //  if yPosition < top then yPosition := top;
            //  if yPosition > bottom then yPosition := bottom;
            left = xPosition
        }
        if (xPosition > right) {
            right = xPosition
        }
        if (yPosition < top) {
            top = yPosition
        }
        if (yPosition > bottom) {
            bottom = yPosition
        }
        this.MapScrollBarHorizontal.SetParams(xPosition, left, right)
        this.MapScrollBarHorizontal.LargeChange = this.MapImage.Width
        //LocalIntMax(1, LocalIntMin((right - left) div 10, MapPaintBox.width));
        this.MapScrollBarVertical.SetParams(yPosition, top, bottom)
        this.MapScrollBarVertical.LargeChange = this.MapImage.Height
        //LocalIntMax(1, LocalIntMin((bottom - top) div 10, MapPaintBox.height));
    }
    
    MapScrollBarVerticalChange(Sender: TObject): void {
        this.MapPaintBoxChanged()
    }
    
    MapScrollBarHorizontalChange(Sender: TObject): void {
        this.MapPaintBoxChanged()
    }
    
    goodPosition(): TPoint {
        let result = new TPoint()
        if (this.lastChoice !== null) {
            if (this.previousChoice !== null) {
                //var
                //  	mapBoundsRect: TRect;
                //    selection: TSDraggableObject; 
                result = Point((this.previousChoice.position.X + this.lastChoice.position.X) / 2, (this.previousChoice.position.Y + this.lastChoice.position.Y) / 2 + 30)
            } else {
                result = Point(this.lastChoice.position.X, this.lastChoice.position.Y + 30)
            }
        } else {
            // mapBoundsRect := domain.world.boundsRect;
            //    result.x := (mapBoundsRect.left - mapBoundsRect.right) div 2;
            //    result.y := mapBoundsRect.bottom + 30;  
            result = Point(this.MapScrollBarHorizontal.Position + this.MapImage.Width / 2, this.MapScrollBarVertical.Position + this.MapImage.Height / 2)
        }
        result.X = result.X + UNRESOLVED.random(200) - 100
        result.Y = result.Y + UNRESOLVED.random(200) - 100
        //if (domain <> nil) and (domain.world <> nil) then
        //    begin
        //    selection := domain.world.firstSelectedObject;
        //    if selection <> nil then
        //      begin
        //      result.x := selection.position.x;
        //      result.y := selection.position.y + 30;
        //      end;
        //    end;
        //  result := Point(MapScrollBarHorizontal.position + MapImage.width div 2, MapScrollBarVertical.position +  MapImage.height div 2);
        //  //result.x := result.x + random(200) - 100;
        //  //result.y := result.y + random(200) - 100;
        //result := Point(MapScrollBarHorizontal.position + MapImage.width div 2, MapScrollBarVertical.position +  MapImage.height div 2);
        //  if (domain <> nil) and (domain.world <> nil) then
        //    begin
        //    selection := domain.world.firstSelectedObject;
        //    if selection <> nil then
        //      begin
        //      result.x := selection.position.x;
        //      result.y := selection.position.y;
        //      end;
        //    end;
        //  result.x := result.x + random(200) - 100;
        //  result.y := result.y + random(200) - 100;   
        return result
    }
    
    // ------------------------------------------------------------------- @Browser
    loadFirstListBox(): void {
        let i: int
        let variable: TSVariable
        
        this.FirstListBox.Clear()
        this.firstListBoxLabel.Caption = "All " + lowercase(usworld.TSRule.headerForField(this.organizeByField))
        if (this.firstListBoxLabel.Caption[len(this.firstListBoxLabel.Caption)] !== "s") {
            this.firstListBoxLabel.Caption = this.firstListBoxLabel.Caption + "s"
        }
        for (i = 0; i <= usdomain.domain.world.variables.Count - 1; i++) {
            variable = usdomain.domain.world.variables[i]
            if (variable.hasUseagesForField(this.organizeByField)) {
                this.FirstListBox.Items.AddObject(variable.phrase, variable)
            }
        }
    }
    
    loadSecondListBox(): void {
        let i: int
        let variable: TSVariable
        let rule: TSRule
        let displayFieldType: int
        let selectedItemString: string
        
        this.SecondListBox.Clear()
        if (this.organizeByField === usworld.kRuleCommand) {
            displayFieldType = usworld.kRuleContext
            this.SecondListBoxImage.Picture.Bitmap = this.ContextSpeedButton.Glyph
        } else {
            displayFieldType = usworld.kRuleCommand
            this.SecondListBoxImage.Picture.Bitmap = this.CommandSpeedButton.Glyph
        }
        selectedItemString = lowercase(usworld.TSRule.headerForField(this.organizeByField))
        if (selectedItemString[len(selectedItemString)] === "s") {
            // remove plural 's' for singular use
            selectedItemString = UNRESOLVED.copy(selectedItemString, 1, len(selectedItemString) - 1)
        }
        this.SecondListBoxLabel.Caption = usworld.TSRule.headerForField(displayFieldType) + "s with selected " + selectedItemString
        if (this.FirstListBox.ItemIndex < 0) {
            return
        }
        variable = UNRESOLVED.TObject(this.FirstListBox.Items.Objects[this.FirstListBox.ItemIndex])
        for (i = 0; i <= usdomain.domain.world.rules.Count - 1; i++) {
            rule = usdomain.domain.world.rules[i]
            if (rule.usesVariableFor(variable, this.organizeByField)) {
                this.SecondListBox.Items.AddObject(rule.variableForField(displayFieldType).phrase, rule)
            }
        }
    }
    
    FirstListBoxDrawItem(Control: TWinControl, index: int, Rect: TRect, State: TOwnerDrawState): void {
        let i: int
        let focused: boolean
        let rule: TSRule
        let variable: TSVariable
        
        if ((index < 0) || (index > this.FirstListBox.Items.Count - 1)) {
            return
        }
        focused = false
        if (delphi_compatability.TOwnerDrawStateType.odSelected in State) {
            for (i = 0; i <= this.SecondListBox.Items.Count - 1; i++) {
                rule = UNRESOLVED.TObject(this.SecondListBox.Items.Objects[i])
                focused = (rule === this.rule) && rule.selected
                if (focused) {
                    break
                }
            }
        }
        variable = UNRESOLVED.TObject(this.FirstListBox.Items.Objects[index])
        if (variable === null) {
            return
        }
        this.drawBrowserListBoxItem(this.FirstListBox, variable.phrase, index, Rect, delphi_compatability.TOwnerDrawStateType.odSelected in State, focused)
    }
    
    SecondListBoxDrawItem(Control: TWinControl, index: int, Rect: TRect, State: TOwnerDrawState): void {
        let selected: boolean
        let focused: boolean
        let rule: TSRule
        let displayFieldType: int
        let displayString: string
        
        if ((index < 0) || (index > this.SecondListBox.Items.Count - 1)) {
            return
        }
        rule = UNRESOLVED.TObject(this.SecondListBox.Items.Objects[index])
        selected = rule.selected
        focused = rule === this.rule
        if (this.organizeByField === usworld.kRuleCommand) {
            displayFieldType = usworld.kRuleContext
        } else {
            displayFieldType = usworld.kRuleCommand
        }
        displayString = rule.variableForField(displayFieldType).phrase
        this.drawBrowserListBoxItem(this.SecondListBox, displayString, index, Rect, selected, focused)
    }
    
    drawBrowserListBoxItem(Control: TWinControl, displayString: string, index: int, Rect: TRect, selected: boolean, focused: boolean): void {
        let listBox: TListBox
        
        if (delphi_compatability.Application.terminated) {
            //cText: array[0..255] of Char;
            return
        }
        listBox = Control
        if (listBox === null) {
            return
        }
        if ((index < 0) || (index > listBox.Items.Count - 1)) {
            return
        }
        setCanvasColorsForSelection(listBox.Canvas, selected, focused, false)
        listBox.Canvas.FillRect(Rect)
        //strPCopy(cText, displayString);
        // margin for text 
        Rect.left = Rect.left + 2
        UNRESOLVED.DrawText(listBox.Canvas.Handle, displayString, len(displayString), Rect, delphi_compatability.DT_LEFT)
    }
    
    FirstListBoxMouseDown(Sender: TObject, Button: TMouseButton, Shift: TShiftState, X: int, Y: int): void {
        this.FirstListBox.Invalidate()
        this.loadSecondListBox()
    }
    
    SecondListBoxMouseDown(Sender: TObject, Button: TMouseButton, Shift: TShiftState, X: int, Y: int): void {
        let rule: TSRule
        let shiftRule: TSRule
        let index: int
        let i: int
        
        if (this.SecondListBox.ItemIndex < 0) {
            return
        }
        index = this.SecondListBox.ItemAtPos(Point(X, Y), true)
        if (index < 0) {
            usdomain.domain.world.deselectAllExcept(null)
            this.FirstListBox.Invalidate()
            this.SecondListBox.Invalidate()
            return
        }
        rule = UNRESOLVED.TObject(this.SecondListBox.Items.Objects[this.SecondListBox.ItemIndex])
        if (rule === null) {
            return
        }
        if ((delphi_compatability.TShiftStateEnum.ssShift in Shift)) {
            if ((this.lastBrowserSingleRuleIndex >= 0) && (this.lastBrowserSingleRuleIndex <= this.SecondListBox.Items.Count - 1) && (this.lastSingleRuleIndex !== index)) {
                // shift
                usdomain.domain.world.deselectAllExcept(rule)
                if (this.lastBrowserSingleRuleIndex < index) {
                    for (i = this.lastBrowserSingleRuleIndex; i <= index; i++) {
                        shiftRule = UNRESOLVED.TObject(this.SecondListBox.Items.Objects[i])
                        shiftRule.selected = true
                    }
                } else if (this.lastBrowserSingleRuleIndex > index) {
                    for (i = this.lastBrowserSingleRuleIndex; i >= index; i--) {
                        shiftRule = UNRESOLVED.TObject(this.SecondListBox.Items.Objects[i])
                        shiftRule.selected = true
                    }
                }
            }
            // control
        } else if ((delphi_compatability.TShiftStateEnum.ssCtrl in Shift)) {
            // just click
            rule.selected = !rule.selected
        } else {
            if (!rule.selected) {
                usdomain.domain.world.deselectAllExcept(rule)
                rule.selected = true
                this.lastBrowserSingleRuleIndex = index
            } else {
                // do nothing except maybe drag...
            }
        }
        if (rule.selected && (this.rule !== rule) && !(delphi_compatability.TShiftStateEnum.ssCtrl in Shift) && !(delphi_compatability.TShiftStateEnum.ssShift in Shift)) {
            this.editRule(rule)
        }
        this.FirstListBox.Invalidate()
        this.SecondListBox.Invalidate()
    }
    
    setOrganizeByField(newValue: int): void {
        let variable: TSVariable
        
        if ((newValue < 0) || (newValue > usworld.kLastRuleField)) {
            return
        }
        usdomain.domain.options.browseBy = newValue
        this.MenuBrowseByContext.checked = newValue === usworld.kRuleContext
        this.MenuBrowseByCommand.checked = newValue === usworld.kRuleCommand
        this.MenuBrowseByMove.checked = newValue === usworld.kRuleMove
        this.MenuBrowseByRequirements.checked = newValue === usworld.kRuleRequirements
        this.MenuBrowseByChanges.checked = newValue === usworld.kRuleChanges
        if (newValue === usworld.kRuleContext) {
            this.firstListBoxImage.Picture.Bitmap = usconsoleform.ConsoleForm.ContextButton.Glyph
        }
        if (newValue === usworld.kRuleCommand) {
            this.firstListBoxImage.Picture.Bitmap = usconsoleform.ConsoleForm.CommandButton.Glyph
        }
        if (newValue === usworld.kRuleMove) {
            this.firstListBoxImage.Picture.Bitmap = usconsoleform.ConsoleForm.MoveButton.Glyph
        }
        if (newValue === usworld.kRuleRequirements) {
            this.firstListBoxImage.Picture.Bitmap = usconsoleform.ConsoleForm.RequirementsButton.Glyph
        }
        if (newValue === usworld.kRuleChanges) {
            this.firstListBoxImage.Picture.Bitmap = usconsoleform.ConsoleForm.ChangesButton.Glyph
        }
        // if organizeByField <> newValue then
        this.organizeByField = newValue
        this.loadFirstListBox()
        if (this.rule !== null) {
            variable = this.rule.variableForFieldWithSelections(this.organizeByField, this.RequirementsListBox.ItemIndex, this.ChangesListBox.ItemIndex)
            this.FirstListBox.ItemIndex = this.FirstListBox.Items.IndexOfObject(variable)
        }
        this.loadSecondListBox()
        this.SecondListBox.ItemIndex = this.SecondListBox.Items.IndexOfObject(this.rule)
    }
    
    firstListBoxImageClick(Sender: TObject): void {
        switch (this.organizeByField) {
            case usworld.kRuleContext:
                this.setOrganizeByField(usworld.kRuleCommand)
                break
            case usworld.kRuleCommand:
                this.setOrganizeByField(usworld.kRuleMove)
                break
            case usworld.kRuleMove:
                this.setOrganizeByField(usworld.kRuleRequirements)
                break
            case usworld.kRuleRequirements:
                this.setOrganizeByField(usworld.kRuleChanges)
                break
            case usworld.kRuleChanges:
                this.setOrganizeByField(usworld.kRuleContext)
                break
    }
    
    fillListBox(listBox: TListBox, list: TList): void {
        let i: int
        let wrapper: TSDesiredStateVariableWrapper
        
        // OK for requirements because is parent class
        listBox.Items.Clear()
        if (this.rule !== null) {
            for (i = 0; i <= list.Count - 1; i++) {
                wrapper = list[i]
                listBox.Items.AddObject(wrapper.displayString(), wrapper)
            }
            listBox.Items.Add("")
        }
    }
    
    logicalStatementForListBox(listBox: TListBox): string {
        let result = ""
        let i: int
        
        result = ""
        for (i = 0; i <= listBox.Items.Count - 2; i++) {
            if (result !== "") {
                // use 2 because last is always blank for adding
                result = result + " & " + trim(listBox.Items[i])
            } else {
                result = trim(listBox.Items[i])
            }
        }
        return result
    }
    
    SpeedButtonClick(Sender: TObject): void {
        this.commitChangesToRule()
        this.switchToPage(this.TabSheetBrowse)
        this.setOrganizeByField((Sender).tag)
    }
    
    // ------------------------------------------------------------------ @Resizing
    FormResize(Sender: TObject): void {
        if (this.PanelEditor.Height !== 1) {
            // var proposedSize: integer;
            //PanelEditor.show; 
            this.PanelEditor.Enabled = true
            if (this.PanelEditor.Height < kMinRestHeight + kMinRequirementsChangesHeight + kSplitterHeight + kSplitterHeight) {
                //PanelTop.height := self.clientHeight - kMinRestHeight - kMinRequirementsChangesHeight - kSplitterHeight - kSplitterHeight;
                this.PanelEditor.Top = this.ClientHeight - kMinRestHeight - kMinRequirementsChangesHeight - kSplitterHeight
                this.PanelEditor.Height = kMinRestHeight + kMinRequirementsChangesHeight + kSplitterHeight
            }
            if (this.PanelEditor.Height > this.ClientHeight - kMinListHeight - kSplitterHeight) {
                this.PanelEditor.Top = kMinListHeight + kSplitterHeight
                this.PanelEditor.Height = this.ClientHeight - kMinListHeight - kSplitterHeight
            }
            if (this.PanelRequirementsChanges.Height > this.PanelEditor.Height - kMinRestHeight - kSplitterHeight) {
                // 	if PanelRest.height < kMinRestHeight then
                this.PanelRequirementsChanges.Height = this.PanelEditor.Height - kMinRestHeight - kSplitterHeight
            }
        }
        if (this.PanelLists.Width - this.PanelFirstList.Width < this.SplitterLists.MinSize) {
            this.PanelFirstList.Width = this.ClientWidth - this.SplitterLists.MinSize
        }
        //if PanelRequirementsChanges.height < SplitterRequirementsChanges.MinSize then
        //		begin
        //    PanelRequirementsChanges.height := SplitterRequirementsChanges.MinSize;
        //  	proposedSize := RuleEditorForm.clientHeight - SplitterRequirementsChanges.MinSize - PanelEditor.top - SplitterRequirementsChanges.height;
        //  	if proposedSize < 200 then
        //  		proposedSize := 200;
        //  	PanelRest.height := proposedSize;
        //  	end;
    }
    
    WMGetMinMaxInfo(MSG: Tmessage): void {
        TForm.prototype.WMGetMinMaxInfo.call(this)
        //FIX unresolved WITH expression: UNRESOLVED.PMinMaxInfo(MSG.lparam).PDF_FIX_POINTER_ACCESS
        UNRESOLVED.ptMinTrackSize.x = 300
        UNRESOLVED.ptMinTrackSize.y = 100 + kMinListHeight + kMinRestHeight + kMinRequirementsChangesHeight + kSplitterHeight + kSplitterHeight
    }
    
    PanelFirstListResize(Sender: TObject): void {
        //FIX unresolved WITH expression: Sender
        this.FirstListBox.Width = this.Width - kGap2
        this.FirstListBox.Height = this.Height - kGap2 - this.FirstListBox.Top
    }
    
    PanelSecondListResize(Sender: TObject): void {
        //FIX unresolved WITH expression: Sender
        this.SecondListBox.Width = this.Width - kGap2
        this.SecondListBox.Height = this.Height - kGap2 - this.SecondListBox.Top
    }
    
    PanelMapResize(Sender: TObject): void {
        this.MapImage.Top = 0
        this.MapImage.Left = 0
        this.MapImage.Width = this.PanelMap.ClientWidth - this.MapScrollBarVertical.Width
        this.MapImage.Height = this.PanelMap.ClientHeight - this.MapScrollBarHorizontal.Height
        this.MapScrollBarHorizontal.Left = 0
        this.MapScrollBarHorizontal.Top = this.MapImage.Height
        this.MapScrollBarHorizontal.Width = this.MapImage.Width
        this.MapScrollBarVertical.Top = 0
        this.MapScrollBarVertical.Height = this.MapImage.Height
        this.MapScrollBarVertical.Left = this.MapImage.Width
        if (this.MapImage.Picture.Bitmap !== null) {
            // could fail - not sure what to do about it...
            this.MapImage.Picture.Bitmap.Width = this.MapImage.Width
            this.MapImage.Picture.Bitmap.Height = this.MapImage.Height
        }
        this.adjustScrollBars()
        this.MapPaintBoxChanged()
    }
    
    PanelRestResize(Sender: TObject): void {
        //FIX unresolved WITH expression: Sender
        //if PanelRest.height < kMinRestHeight then
        //	PanelRest.height := kMinRestHeight;
        this.MoveSpeedButton.Top = this.Height - this.MoveSpeedButton.Height - kGap
        this.MoveEdit.Top = this.MoveSpeedButton.Top
        this.ReplyMemo.Height = this.MoveEdit.Top - this.ReplyMemo.Top - kGap
        this.ContextEdit.Width = this.Width - this.ContextEdit.Left - kGap
        this.CommandEdit.Width = this.ContextEdit.Width
        this.ReplyMemo.Width = this.ContextEdit.Width
        this.MoveEdit.Width = this.ContextEdit.Width
    }
    
    PanelRequirementsChangesResize(Sender: TObject): void {
        // just in case fooling with popup editor window...
        this.commitChangesToRule()
        //FIX unresolved WITH expression: Sender
        this.RequirementsListBox.Height = (this.Height - kGap3) / 2
        this.RequirementsListBox.Width = this.Width - this.RequirementsListBox.Left - kGap
        this.ChangesListBox.Top = this.RequirementsListBox.Top + this.RequirementsListBox.Height + kGap
        this.ChangesSpeedButton.Top = this.ChangesListBox.Top
        this.ChangesListBox.Height = this.RequirementsListBox.Height
        this.ChangesListBox.Width = this.RequirementsListBox.Width
    }
    
    PanelListsResize(Sender: TObject): void {
        //if PanelLists.height < kMinListHeight then
        //  PanelLists.height := kMinListHeight;
    }
    
    SplitterRequirementsChangesMoved(Sender: TObject): void {
        this.FormResize(Sender)
    }
    
    SplitterEditMoved(Sender: TObject): void {
        if (this.PanelEditor.Visible && (this.PanelEditor.Height <= 50)) {
            //PanelEditor.hide;
            this.PanelTop.Height = this.ClientHeight - kSplitterHeight - 1
            this.PanelEditor.Height = 1
            this.PanelEditor.Enabled = false
            this.MenuOptionsShowRuleEditor.checked = false
            usdomain.domain.options.showRuleEditor = false
        } else {
            //if not PanelEditor.visible then
            //      begin
            //    	PanelEditor.show;
            //    	{PanelTop.height := self.clientHeight - 200;
            //      end; 
            this.MenuOptionsShowRuleEditor.checked = true
            usdomain.domain.options.showRuleEditor = true
            if (this.PanelEditor.Height === 1) {
                this.PanelEditor.Height = 2
            }
            this.PanelEditor.Enabled = true
            this.FormResize(Sender)
        }
        usdomain.domain.options.editorPanelEditorHeight = this.PanelEditor.Height
    }
    
    MenuFileExportClick(Sender: TObject): void {
        let fileInfo: SaveFileNamesStructure
        
        this.commitChangesToRule()
        this.lastSaveProceeded = ufilesupport.getFileSaveInfo(ufilesupport.kFileTypeWorld, ufilesupport.kAskForFileName, "export" + "." + usdomain.kWorldExtension, fileInfo)
        if (!this.lastSaveProceeded) {
            return
        }
        try {
            ufilesupport.startFileSave(fileInfo)
            usdomain.domain.world.saveWorldToFile(fileInfo.tempFile, usworld.kSaveOnlySelectedRules)
            fileInfo.writingWasSuccessful = true
        } finally {
            this.lastSaveProceeded = ufilesupport.cleanUpAfterFileSave(fileInfo)
        }
    }
    
    scrollMapSelectionIntoView(): void {
        let intersection: TRect
        let visibleRect: TRect
        let rule: TSRule
        let variable: TSVariable
        let i: int
        let upperLeftObject: TSDraggableObject
        let firstContextVariable: TSVariable
        
        upperLeftObject = null
        visibleRect.Left = this.MapScrollBarHorizontal.Position
        visibleRect.Top = this.MapScrollBarVertical.Position
        visibleRect.Right = visibleRect.Left + this.MapImage.Width
        visibleRect.Bottom = visibleRect.Top + this.MapImage.Height
        for (i = 0; i <= usdomain.domain.world.rules.Count - 1; i++) {
            rule = usworld.TSRule(usdomain.domain.world.rules[i])
            if (rule.selected) {
                delphi_compatability.IntersectRect(intersection, rule.bounds(), visibleRect)
                if (!delphi_compatability.IsRectEmpty(intersection)) {
                    return
                }
                if (upperLeftObject === null) {
                    upperLeftObject = rule
                } else if (upperLeftObject.bounds().Top > rule.bounds().Top) {
                    upperLeftObject = rule
                } else if (upperLeftObject.bounds().Left > rule.bounds().Left) {
                    upperLeftObject = rule
                }
            }
        }
        firstContextVariable = null
        for (i = 0; i <= usdomain.domain.world.variables.Count - 1; i++) {
            variable = usworld.TSVariable(usdomain.domain.world.variables[i])
            if ((firstContextVariable === null) && (variable.hasUseagesForField(usworld.kRuleContext))) {
                firstContextVariable = variable
            }
            if (variable.selected) {
                delphi_compatability.IntersectRect(intersection, variable.bounds(), visibleRect)
                if (!delphi_compatability.IsRectEmpty(intersection)) {
                    return
                }
                if (upperLeftObject === null) {
                    upperLeftObject = variable
                } else if (upperLeftObject.bounds().Top > variable.bounds().Top) {
                    upperLeftObject = variable
                } else if (upperLeftObject.bounds().Left > variable.bounds().Left) {
                    upperLeftObject = variable
                }
            }
        }
        if (upperLeftObject === null) {
            upperLeftObject = firstContextVariable
        }
        if (upperLeftObject === null) {
            return
        }
        this.MapScrollBarHorizontal.Position = localIntMin(localIntMax(upperLeftObject.center().X - this.MapImage.Width / 2, this.MapScrollBarHorizontal.Min), this.MapScrollBarHorizontal.Max)
        this.MapScrollBarVertical.Position = localIntMin(localIntMax(upperLeftObject.center().Y + -this.MapImage.Height / 2, this.MapScrollBarVertical.Min), this.MapScrollBarVertical.Max)
    }
    
    MenuEditInsertSoundClick(Sender: TObject): void {
        let fileNameWithPath: string
        let shortFileName: string
        
        this.commitChangesToRule()
        if (this.ActiveControl !== this.ReplyMemo) {
            ShowMessage("The reply field must be selected to insert a sound file.")
            return
        }
        fileNameWithPath = ufilesupport.getFileOpenInfo(ufilesupport.kFileTypeSound, ufilesupport.kNoSuggestedFile, "Choose a sound file", ufilesupport.kOtherExtOK)
        if (fileNameWithPath === "") {
            return
        }
        shortFileName = ExtractFileName(fileNameWithPath)
        if (UNRESOLVED.pos(".WAV", uppercase(shortFileName)) === len(shortFileName) - 3) {
            shortFileName = UNRESOLVED.copy(shortFileName, 1, len(shortFileName) - 4)
        }
        this.ReplyMemo.SelText = " {" + kPlaySoundMacroStart + shortFileName + "} "
    }
    
    insertSoundClick(Sender: TObject): void {
        this.MenuEditInsertSoundClick(this)
    }
    
    InsertMusicButtonClick(Sender: TObject): void {
        this.MenuEditInsertMusicClick(this)
    }
    
    MenuEditInsertMusicClick(Sender: TObject): void {
        let fileNameWithPath: string
        let shortFileName: string
        
        this.commitChangesToRule()
        if (this.ActiveControl !== this.ReplyMemo) {
            ShowMessage("The reply field must be selected to insert a music file.")
            return
        }
        fileNameWithPath = ufilesupport.getFileOpenInfo(ufilesupport.kFileTypeMusic, ufilesupport.kNoSuggestedFile, "Choose a music file", ufilesupport.kOtherExtOK)
        if (fileNameWithPath === "") {
            return
        }
        shortFileName = ExtractFileName(fileNameWithPath)
        if (UNRESOLVED.pos(".MID", uppercase(shortFileName)) === len(shortFileName) - 3) {
            shortFileName = UNRESOLVED.copy(shortFileName, 1, len(shortFileName) - 4)
        }
        this.ReplyMemo.SelText = " {" + kPlayMusicMacroStart + shortFileName + "} "
    }
    
    MenuEditInsertPictureClick(Sender: TObject): void {
        let fileNameWithPath: string
        let shortFileName: string
        
        this.commitChangesToRule()
        if (this.ActiveControl !== this.ReplyMemo) {
            ShowMessage("The reply field must be selected to insert a picture file.")
            return
        }
        fileNameWithPath = ufilesupport.getFileOpenInfo(ufilesupport.kFileTypeBitmap, ufilesupport.kNoSuggestedFile, "Choose a bitmap file", ufilesupport.kOtherExtNotOK)
        if (fileNameWithPath === "") {
            return
        }
        shortFileName = ExtractFileName(fileNameWithPath)
        if (UNRESOLVED.pos(".BMP", uppercase(shortFileName)) === len(shortFileName) - 3) {
            shortFileName = UNRESOLVED.copy(shortFileName, 1, len(shortFileName) - 4)
        }
        this.ReplyMemo.SelText = " {" + kShowPictureMacroStart + shortFileName + "} "
    }
    
    MediaPlayerNotify(Sender: TObject): void {
        if (this.loopMusic && (this.MediaPlayer.FileName !== "") && (this.MediaPlayer.notifyValue === UNRESOLVED.nvSuccessful)) {
            this.MediaPlayer.Notify = true
            this.MediaPlayer.Play
        }
    }
    
    MenuRuleTestReplyClick(Sender: TObject): void {
        let oldSpeak: boolean
        let oldPlayMusic: boolean
        let oldPlaySounds: boolean
        
        this.commitChangesToRule()
        usconsoleform.ConsoleForm.speechSystem.haltSpeechAndSound()
        if (this.rule === null) {
            return
        }
        // temporarily turn on these options to test the reply
        oldSpeak = usdomain.domain.options.playerSpeak
        oldPlaySounds = usdomain.domain.options.playerPlaySounds
        oldPlayMusic = usdomain.domain.options.playerPlayMusic
        usdomain.domain.options.playerSpeak = true
        usdomain.domain.options.playerPlaySounds = true
        usdomain.domain.options.playerPlayMusic = true
        try {
            usconsoleform.ConsoleForm.speechSystem.sayTextWithMacros(this.rule.reply)
        } finally {
            usdomain.domain.options.playerSpeak = oldSpeak
            usdomain.domain.options.playerPlaySounds = oldPlaySounds
            usdomain.domain.options.playerPlayMusic = oldPlayMusic
        }
    }
    
    replyPictureMouseUp(Sender: TObject, Button: TMouseButton, Shift: TShiftState, X: int, Y: int): void {
        this.MenuRuleTestReplyClick(this)
    }
    
    MenuEditClick(Sender: TObject): void {
        // keep the undo menu up to date...
        this.commitChangesToRule()
    }
    
    ReplyMemoMouseUp(Sender: TObject, Button: TMouseButton, Shift: TShiftState, X: int, Y: int): void {
        // more fine grained tracking of changes to this field...
        this.commitChangesToRule()
    }
    
    FormKeyDown(Sender: TObject, Key: byte, Shift: TShiftState): void {
        if (Key === delphi_compatability.VK_ESCAPE) {
            usconsoleform.ConsoleForm.speechSystem.haltSpeechAndSound()
        } else if ((delphi_compatability.TShiftStateEnum.ssCtrl in Shift) && ((Key === ord("c")) || (Key === ord("C")))) {
            this.MenuEditCopyClick(this)
            Key = 0
        } else if ((delphi_compatability.TShiftStateEnum.ssCtrl in Shift) && ((Key === ord("v")) || (Key === ord("V")))) {
            this.MenuEditPasteClick(this)
            Key = 0
        } else if ((delphi_compatability.TShiftStateEnum.ssCtrl in Shift) && ((Key === ord("x")) || (Key === ord("X")))) {
            this.MenuEditCutClick(this)
            Key = 0
        }
        return Key
    }
    
    MenuHelpContentsClick(Sender: TObject): void {
        this.commitChangesToRule()
        delphi_compatability.Application.HelpCommand(UNRESOLVED.HELP_FINDER, 0)
    }
    
    MenuHelpBasicConceptsClick(Sender: TObject): void {
        this.commitChangesToRule()
        delphi_compatability.Application.HelpJump("A_summary_based_on_definitions")
    }
    
    MenuHelpTutorialClick(Sender: TObject): void {
        this.commitChangesToRule()
        delphi_compatability.Application.HelpJump("Basic_Tutorial")
    }
    
    MenuHelpEditingWorldsClick(Sender: TObject): void {
        this.commitChangesToRule()
        delphi_compatability.Application.HelpJump("Editing_worlds")
    }
    
    MenuWorldSwitchToPlayerClick(Sender: TObject): void {
        this.commitChangesToRule()
        usconsoleform.ConsoleForm.Show()
    }
    
    FormShow(Sender: TObject): void {
        UNRESOLVED.DragAcceptFiles(this.Handle, true)
    }
    
    PopupNewContextClick(Sender: TObject): void {
        let newRule: TSRule
        let newRulesCommand: TSNewRulesCommand
        
        if (delphi_compatability.Application.terminated) {
            return
        }
        this.commitChangesToRule()
        newRulesCommand = uscommands.TSNewRulesCommand().create()
        newRule = usdomain.domain.world.newRule()
        newRulesCommand.addRule(newRule)
        while (usdomain.domain.world.findVariable("new context " + IntToStr(this.numNewContextsMadeByPopupMenuThisSession)) !== null) {
            this.numNewContextsMadeByPopupMenuThisSession += 1
        }
        newRule.setContext("new context " + IntToStr(this.numNewContextsMadeByPopupMenuThisSession))
        newRule.setCommand("look")
        newRule.setReply("There is nothing of interest here.")
        newRule.position = Point(this.lastMapMouseDownPosition.X + 30, this.lastMapMouseDownPosition.Y + 30)
        newRule.context.position = this.lastMapMouseDownPosition
        usdomain.domain.world.deselectAllExcept(newRule)
        newRule.selected = true
        usdomain.domain.worldCommandList.doCommand(newRulesCommand)
        this.editRule(newRule)
        this.ActiveControl = this.ContextEdit
        this.ContextEdit.SelStart = 0
        this.ContextEdit.SelLength = len(this.ContextEdit.Text)
        this.MapPaintBoxChanged()
    }
    
    PopupNewCommandClick(Sender: TObject): void {
        let rule: TSRule
        let newRule: TSRule
        let newRulesCommand: TSNewRulesCommand
        let variable: TSVariable
        let i: int
        let newRuleCount: int
        
        if (delphi_compatability.Application.terminated) {
            return
        }
        this.commitChangesToRule()
        while (usdomain.domain.world.findVariable("new command " + IntToStr(this.numNewCommandsMadeByPopupMenuThisSession)) !== null) {
            this.numNewCommandsMadeByPopupMenuThisSession += 1
        }
        newRule = null
        newRuleCount = 0
        for (i = 0; i <= usdomain.domain.world.variables.Count - 1; i++) {
            variable = usworld.TSVariable(usdomain.domain.world.variables[i])
            if (variable.selected) {
                newRulesCommand = uscommands.TSNewRulesCommand().create()
                newRule = usdomain.domain.world.newRule()
                newRulesCommand.addRule(newRule)
                newRule.setContext(variable.phrase)
                newRule.setCommand("new command " + IntToStr(this.numNewCommandsMadeByPopupMenuThisSession))
                newRule.setReply("Nothing happens.")
                newRule.position = Point(this.lastMapMouseDownPosition.X, this.lastMapMouseDownPosition.Y + 30 * newRuleCount)
                usdomain.domain.worldCommandList.doCommand(newRulesCommand)
                newRuleCount += 1
            }
        }
        for (i = 0; i <= usdomain.domain.world.rules.Count - 1; i++) {
            rule = usworld.TSRule(usdomain.domain.world.rules[i])
            if (rule.selected) {
                newRulesCommand = uscommands.TSNewRulesCommand().create()
                newRule = usdomain.domain.world.newRule()
                newRulesCommand.addRule(newRule)
                newRule.setContext(rule.context.phrase)
                newRule.setCommand("new command " + IntToStr(this.numNewCommandsMadeByPopupMenuThisSession))
                newRule.setReply("Nothing happens.")
                newRule.position = Point(this.lastMapMouseDownPosition.X, this.lastMapMouseDownPosition.Y + 30 * newRuleCount)
                usdomain.domain.worldCommandList.doCommand(newRulesCommand)
                newRuleCount += 1
            }
        }
        if (newRule === null) {
            MessageDialog("To make a new command," + chr(13) + "select at least one context or command" + chr(13) + "and right-click where you want to place the new command.", mtInformation, {mbOK, }, 0)
            this.MapPaintBoxChanged()
            return
        }
        usdomain.domain.world.deselectAllExcept(newRule)
        newRule.selected = true
        this.editRule(newRule)
        this.ActiveControl = this.CommandEdit
        this.CommandEdit.SelStart = 0
        this.CommandEdit.SelLength = len(this.CommandEdit.Text)
        this.MapPaintBoxChanged()
    }
    
    PopupNewLinkClick(Sender: TObject): void {
        let draggableNode: TSDraggableObject
        let contextToMoveTo: TSVariable
        let newRulesCommand: TSNewRulesCommand
        let variable: TSVariable
        let rule: TSRule
        let newRule: TSRule
        let mapView: TSMapView
        let displayOptions: TSVariableDisplayOptions
        let i: int
        let atLeastOneRuleChanged: boolean
        
        if (delphi_compatability.Application.terminated) {
            return
        }
        this.commitChangesToRule()
        mapView = this.currentGraphView()
        if (mapView === null) {
            return
        }
        for (i = 0; i <= 5; i++) {
            displayOptions[i] = false
        }
        displayOptions[usworld.kRuleContext] = true
        displayOptions[usworld.kRuleMove] = true
        displayOptions[usworld.kRuleCommand] = this.MenuMapsShowCommands.checked
        draggableNode = mapView.nearestNode(this.lastMapMouseDownPosition, displayOptions)
        if ((draggableNode === null) || !(draggableNode instanceof usworld.TSVariable)) {
            MessageDialog("To build a link," + chr(13) + "select at least one context or command" + chr(13) + "and right-click on a context.", mtInformation, {mbOK, }, 0)
            this.MapPaintBoxChanged()
            return
        }
        contextToMoveTo = draggableNode
        newRule = null
        for (i = 0; i <= usdomain.domain.world.variables.Count - 1; i++) {
            variable = usworld.TSVariable(usdomain.domain.world.variables[i])
            if (variable.selected) {
                newRulesCommand = uscommands.TSNewRulesCommand().create()
                newRule = usdomain.domain.world.newRule()
                newRulesCommand.addRule(newRule)
                newRule.setContext(variable.phrase)
                newRule.setCommand("move to " + contextToMoveTo.phrase)
                newRule.setReply("You move to " + contextToMoveTo.phrase + ".")
                newRule.setMove(contextToMoveTo.phrase)
                newRule.position.X = (variable.position.X + contextToMoveTo.position.X) / 2
                newRule.position.Y = (variable.position.Y + contextToMoveTo.position.Y) / 2
                usdomain.domain.worldCommandList.doCommand(newRulesCommand)
            }
        }
        atLeastOneRuleChanged = false
        for (i = 0; i <= usdomain.domain.world.rules.Count - 1; i++) {
            rule = usworld.TSRule(usdomain.domain.world.rules[i])
            if (rule.selected) {
                if (contextToMoveTo.phrase !== rule.move.phrase) {
                    usdomain.domain.worldCommandList.ruleFieldChange(rule, usworld.kRuleMove, contextToMoveTo.phrase)
                }
                atLeastOneRuleChanged = true
            }
        }
        if (newRule !== null) {
            usdomain.domain.world.deselectAllExcept(newRule)
            newRule.selected = true
            this.editRule(newRule)
            this.ActiveControl = this.CommandEdit
            this.CommandEdit.SelStart = 0
            this.CommandEdit.SelLength = len(this.CommandEdit.Text)
        }
        if ((newRule !== null) || (atLeastOneRuleChanged)) {
            this.MapPaintBoxChanged()
        } else {
            MessageDialog("To build a link," + chr(13) + "select at least one context or command" + chr(13) + "and right-click on a context.", mtInformation, {mbOK, }, 0)
            this.MapPaintBoxChanged()
        }
    }
    
    MenuToolsGenerateJavaClick(Sender: TObject): void {
        let javaWriter: TSJavaWriter
        let response: int
        
        response = MessageDialog("StoryHarp will compile the world file " + chr(13) + chr(13) + "    " + usdomain.domain.worldFileName + chr(13) + chr(13) + "into a Java applet source code file " + chr(13) + chr(13) + "    " + UNRESOLVED.GetCurrentDir + "\Story.java" + chr(13) + chr(13) + "To produce a Java applet, you will need to compile" + chr(13) + "the Java source with a Java development system." + chr(13) + chr(13) + "See the help system under \"Java\" for details.", mtConfirmation, {mbOK, mbCancel, }, 0)
        if (response !== delphi_compatability.IDOK) {
            return
        }
        javaWriter = usjavawriter.TSJavaWriter.create
        try {
            javaWriter.writeJavaProgram("Story.java")
        } finally {
            javaWriter.free
        }
        MessageDialog("File " + UNRESOLVED.GetCurrentDir + "\Story.java was written.", mtInformation, {mbOK, }, 0)
    }
    
    // ----------------------------------------------------------------------------- *palette stuff 
    GetPalette(): HPALETTE {
        let result = new HPALETTE()
        result = uspictureform.PictureForm.PictureImage.Picture.Bitmap.Palette
        return result
    }
    
    //overriden because paint box will not update correctly
    //makes window take first priority for palettes
    PaletteChanged(Foreground: boolean): boolean {
        let result = false
        let oldPalette: HPALETTE
        let palette: HPALETTE
        let windowHandle: HWnd
        let DC: HDC
        
        result = false
        if (delphi_compatability.Application.terminated) {
            return result
        }
        palette = this.GetPalette()
        if (palette !== 0) {
            DC = this.GetDeviceContext(windowHandle)
            oldPalette = UNRESOLVED.selectPalette(DC, palette, !Foreground)
            if ((UNRESOLVED.realizePalette(DC) !== 0) && (uspictureform.PictureForm !== null) && (uspictureform.PictureForm.PictureImage !== null)) {
                // if palette changed, repaint drawing 
                uspictureform.PictureForm.PictureImage.Invalidate()
            }
            UNRESOLVED.selectPalette(DC, oldPalette, true)
            UNRESOLVED.realizePalette(DC)
            UNRESOLVED.releaseDC(windowHandle, DC)
        }
        result = TForm.prototype.PaletteChanged.call(this, Foreground)
        return result
    }
    
}

