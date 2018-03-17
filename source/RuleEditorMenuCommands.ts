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
        let CFileName: char[] // MAX_PATH + 1
        
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
    
    // ------------------------------------------------------------------ @Commands
    
    trackLastCommand(): void {
        this.editRule(this.lastCommand)
        this.updateForRuleChange()
    }
    
    EditEnterCommit(Sender: TObject): void {
        this.commitChangesToRule()
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
 
    // --------------- search

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
    */  
}
