// TODO: Menu commands -- use or remove

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
    
    MediaPlayerNotify(Sender: TObject): void {
        if (this.loopMusic && (this.MediaPlayer.FileName !== "") && (this.MediaPlayer.notifyValue === UNRESOLVED.nvSuccessful)) {
            this.MediaPlayer.Notify = true
            this.MediaPlayer.Play
        }
    }
    */  
}
