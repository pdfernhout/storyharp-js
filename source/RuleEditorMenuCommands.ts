// TODO: Menu commands -- use or remove

// const
const kPlaySoundMacroStart = "sound "
const kPlayMusicMacroStart = "music "
const kShowPictureMacroStart = "picture "

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
export class RuleEditorMenuCommands {
    /*
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
    

    // ----------------------------------------------------------------- @File menu
    
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
    
    // ----------------------------------------------------------------- @Edit menu
    
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
    
    // ------------------------------------------------------------------ @Commands
    
    trackLastCommand(): void {
        this.editRule(this.lastCommand)
        this.updateForRuleChange()
    }
    
    // -------------- more menus
    
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
    
    */  
}
