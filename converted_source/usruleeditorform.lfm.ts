

import { m } from "mithril"
import { caption } from "common"

export function viewRuleEditorForm() {
    return m("RuleEditorForm.TRuleEditorForm",
        {
        },
        m("div.PanelEditor.TPanel",
            {
            },
            m("div.SplitterRequirementsChanges.TSplitter",
                {
                },
            ),
            m("div.PanelRequirementsChanges.TPanel",
                {
                },
                m("TListBox.RequirementsListBox.TListBox",
                    {
                    },
                ),
                m("button.RequirementsSpeedButton.TSpeedButton",
                    {
                        onclick: SpeedButtonClick,
                        title: "Browse all rules with the selected requirement",
                    },
                    "Requirements",
                ),
                m("TListBox.ChangesListBox.TListBox",
                    {
                    },
                ),
                m("button.ChangesSpeedButton.TSpeedButton",
                    {
                        onclick: SpeedButtonClick,
                        title: "Browse all rules with the selected change",
                    },
                    "Changes",
                ),
                m("input.ChangesEdit.TEdit",
                    {
                    },
                ),
                m("input.RequirementsEdit.TEdit",
                    {
                    },
                ),
            ),
            m("div.PanelRest.TPanel",
                {
                },
                m("button.ContextSpeedButton.TSpeedButton",
                    {
                        onclick: SpeedButtonClick,
                        title: "Browse all rules with this context",
                    },
                    "Context",
                ),
                m("div.RuleNumberLabel.TLabel",
                    {
                        title: "The index of the edited rule in the table",
                    },
                    "#1234",
                ),
                m("input.ContextEdit.TEdit",
                    {
                    },
                ),
                m("button.CommandSpeedButton.TSpeedButton",
                    {
                        onclick: SpeedButtonClick,
                        title: "Browse all rules with this command",
                    },
                    "Command",
                ),
                m("input.CommandEdit.TEdit",
                    {
                    },
                ),
                m("textarea.ReplyMemo.TMemo",
                    {
                    },
                ),
                m("Group.Group.g00000064",
                    m("img.replyPicture.TImage",
                        {
                            title: "Test saying the reply",
                        },
                    ),
                    m("div.Label5.TLabel",
                        {
                        },
                        "Reply",
                    ),
                ),
                m("button.MoveSpeedButton.TSpeedButton",
                    {
                        onclick: SpeedButtonClick,
                        title: "Browse all rules with this move",
                    },
                    "Move",
                ),
                m("input.MoveEdit.TEdit",
                    {
                    },
                ),
            ),
        ),
        m("div.PanelTop.TPanel",
            {
            },
            m("div.PanelButtonBar.TPanel",
                {
                },
                m("button.NewRuleButton.TSpeedButton",
                    {
                        onclick: NewRuleButtonClick,
                        title: "Make a new rule",
                    },
                    "New",
                ),
                m("button.DuplicateRuleButton.TSpeedButton",
                    {
                        onclick: DuplicateRuleButtonClick,
                        title: "Duplicate the rule showing in the rule editor panel",
                    },
                    "Duplicate",
                ),
                m("button.DeleteRuleButton.TSpeedButton",
                    {
                        onclick: DeleteRuleButtonClick,
                        title: "Delete all selected rules",
                    },
                    "Delete",
                ),
                m("button.MoveUpButton.TSpeedButton",
                    {
                        onclick: MoveUpButtonClick,
                        title: "Raise all selected rules",
                    },
                    "Raise",
                ),
                m("button.MoveDownButton.TSpeedButton",
                    {
                        onclick: MoveDownButtonClick,
                        title: "Lower all selected rules",
                    },
                    "Lower",
                ),
                m("button.insertSound.TSpeedButton",
                    {
                        onclick: insertSoundClick,
                        title: "Insert a sound into a reply",
                    },
                    "Sound",
                ),
                m("button.InsertMusicButton.TSpeedButton",
                    {
                        onclick: InsertMusicButtonClick,
                        title: "Insert music into a reply",
                    },
                    "Music",
                ),
            ),
            m("TPageControl.ListPages.TPageControl",
                {
                },
                m("TTabSheet.TabSheetTable.TTabSheet",
                    {
                    },
                    " Table",
                    m("TDrawGrid.RuleGrid.TDrawGrid",
                        {
                        },
                    ),
                ),
                m("TTabSheet.TabSheetMap.TTabSheet",
                    {
                    },
                    "  Map",
                    m("div.PanelMap.TPanel",
                        {
                        },
                        m("Group.Group.g00000000",
                            m("img.MapImage.TImage",
                                {
                                },
                            ),
                            m("TScrollBar.MapScrollBarVertical.TScrollBar",
                                {
                                },
                            ),
                        ),
                        m("TScrollBar.MapScrollBarHorizontal.TScrollBar",
                            {
                            },
                        ),
                    ),
                ),
                m("TTabSheet.TabSheetBrowse.TTabSheet",
                    {
                    },
                    "Browser",
                    m("div.PanelLists.TPanel",
                        {
                        },
                        "PanelLists",
                        m("div.PanelFirstList.TPanel",
                            {
                            },
                            m("Group.Group.g00000002",
                                m("img.firstListBoxImage.TImage",
                                    {
                                        onclick: firstListBoxImageClick,
                                    },
                                ),
                                m("div.firstListBoxLabel.TLabel",
                                    {
                                    },
                                    "Contexts",
                                ),
                            ),
                            m("TListBox.FirstListBox.TListBox",
                                {
                                },
                            ),
                        ),
                        m("div.SplitterLists.TSplitter",
                            {
                            },
                        ),
                        m("div.PanelSecondList.TPanel",
                            {
                            },
                            m("Group.Group.g00000002",
                                m("img.SecondListBoxImage.TImage",
                                    {
                                    },
                                ),
                                m("div.SecondListBoxLabel.TLabel",
                                    {
                                    },
                                    "Commands",
                                ),
                            ),
                            m("TListBox.SecondListBox.TListBox",
                                {
                                },
                            ),
                        ),
                    ),
                ),
            ),
        ),
        m("div.SplitterEdit.TSplitter",
            {
            },
        ),
        m("ul.MapPopupMenu.TPopupMenu",
            {
            },
            m("li.PopupNewContext.TMenuItem",
                {
                    onclick: PopupNewContextClick,
                },
                caption("New &Context"),
            ),
            m("li.PopupNewCommand.TMenuItem",
                {
                    onclick: PopupNewCommandClick,
                },
                caption("New C&ommand"),
            ),
            m("li.PopupNewLink.TMenuItem",
                {
                    onclick: PopupNewLinkClick,
                },
                caption("New &Move"),
            ),
        ),
        m("TOpenDialog.WaveFileOpenDialog.TOpenDialog",
            {
            },
        ),
        m("TMainMenu.MainMenu1.TMainMenu",
            {
            },
            m("li.MenuFile.TMenuItem",
                {
                    onclick: MenuEditClick,
                },
                caption("&World"),
                m("li.MenuFileNewWorld.TMenuItem",
                    {
                        onclick: MenuFileNewWorldClick,
                    },
                    caption("&New"),
                ),
                m("li.MenuFileOpenWorld.TMenuItem",
                    {
                        onclick: MenuFileOpenWorldClick,
                    },
                    caption("&Open..."),
                ),
                m("li.N7.TMenuItem",
                    {
                    },
                    "-",
                ),
                m("li.MenuFileSaveWorld.TMenuItem",
                    {
                        onclick: MenuFileSaveWorldClick,
                    },
                    caption("&Save"),
                ),
                m("li.MenuFileSaveWorldAs.TMenuItem",
                    {
                        onclick: MenuFileSaveWorldAsClick,
                    },
                    caption("Save &As..."),
                ),
                m("li.N4.TMenuItem",
                    {
                    },
                    "-",
                ),
                m("li.MenuWorldSwitchToPlayer.TMenuItem",
                    {
                        onclick: MenuWorldSwitchToPlayerClick,
                    },
                    caption("Switch to &Player..."),
                ),
                m("li.N8.TMenuItem",
                    {
                    },
                    "-",
                ),
                m("li.MenuFileExit.TMenuItem",
                    {
                        onclick: MenuFileExitClick,
                    },
                    caption("E&xit"),
                ),
            ),
            m("li.MenuEdit.TMenuItem",
                {
                    onclick: MenuEditClick,
                },
                caption("&Edit"),
                m("li.MenuEditUndo.TMenuItem",
                    {
                        onclick: MenuEditUndoClick,
                    },
                    caption("&Undo"),
                ),
                m("li.MenuEditRedo.TMenuItem",
                    {
                        onclick: MenuEditRedoClick,
                    },
                    caption("&Redo"),
                ),
                m("li.N1.TMenuItem",
                    {
                    },
                    "-",
                ),
                m("li.MenuEditCut.TMenuItem",
                    {
                        onclick: MenuEditCutClick,
                    },
                    caption("Cu&t                 Ctrl+X"),
                ),
                m("li.MenuEditCopy.TMenuItem",
                    {
                        onclick: MenuEditCopyClick,
                    },
                    caption("&Copy              Ctrl+C"),
                ),
                m("li.MenuEditPaste.TMenuItem",
                    {
                        onclick: MenuEditPasteClick,
                    },
                    caption("&Paste             Ctrl+V"),
                ),
                m("li.N12.TMenuItem",
                    {
                    },
                    "-",
                ),
                m("li.MenuEditInsertSound.TMenuItem",
                    {
                        onclick: MenuEditInsertSoundClick,
                    },
                    caption("Insert &Sound..."),
                ),
                m("li.MenuEditInsertMusic.TMenuItem",
                    {
                        onclick: MenuEditInsertMusicClick,
                    },
                    caption("Insert &Music..."),
                ),
                m("li.MenuEditInsertPicture.TMenuItem",
                    {
                        onclick: MenuEditInsertPictureClick,
                    },
                    caption("Insert P&icture..."),
                ),
                m("li.N13.TMenuItem",
                    {
                    },
                    "-",
                ),
                m("li.MenuEditPreferences.TMenuItem",
                    {
                        onclick: MenuEditPreferencesClick,
                    },
                    caption("&Editor Preferences..."),
                ),
            ),
            m("li.MenuRule.TMenuItem",
                {
                },
                caption("&Rule"),
                m("li.MenuRuleNew.TMenuItem",
                    {
                        onclick: MenuRuleNewClick,
                    },
                    caption("&New Rule"),
                ),
                m("li.MenuRuleDuplicate.TMenuItem",
                    {
                        onclick: MenuRuleDuplicateClick,
                    },
                    caption("&Duplicate"),
                ),
                m("li.MenuRuleDelete.TMenuItem",
                    {
                        onclick: MenuRuleDeleteClick,
                    },
                    caption("D&elete"),
                ),
                m("li.N3.TMenuItem",
                    {
                    },
                    "-",
                ),
                m("li.MenuRuleRaise.TMenuItem",
                    {
                        onclick: MenuRuleRaiseClick,
                    },
                    caption("&Raise"),
                ),
                m("li.MenuRuleLower.TMenuItem",
                    {
                        onclick: MenuRuleLowerClick,
                    },
                    caption("&Lower"),
                ),
                m("li.N11.TMenuItem",
                    {
                    },
                    "-",
                ),
                m("li.MenuFileMergeWithWorld.TMenuItem",
                    {
                        onclick: MenuFileMergeWithWorldClick,
                    },
                    caption("&Import From..."),
                ),
                m("li.MenuFileExport.TMenuItem",
                    {
                        onclick: MenuFileExportClick,
                    },
                    caption("E&xport Selected To..."),
                ),
                m("li.N5.TMenuItem",
                    {
                    },
                    "-",
                ),
                m("li.MenuRuleTestReply.TMenuItem",
                    {
                        onclick: MenuRuleTestReplyClick,
                    },
                    caption("&Test Reply"),
                ),
            ),
            m("li.MenuMaps.TMenuItem",
                {
                },
                caption("&Options"),
                m("li.MenuDisplayShowButtonBar.TMenuItem",
                    {
                        onclick: MenuDisplayShowButtonBarClick,
                    },
                    caption("Show &Button Bar"),
                ),
                m("li.MenuOptionsShowRuleEditor.TMenuItem",
                    {
                        onclick: MenuOptionsShowRuleEditorClick,
                    },
                    caption("Show Rule &Editor"),
                ),
                m("li.MenuMapsShowCommands.TMenuItem",
                    {
                        onclick: MenuMapsShowCommandsClick,
                    },
                    caption("&Show Commands in Map"),
                ),
                m("li.N10.TMenuItem",
                    {
                    },
                    "-",
                ),
                m("li.MenuBrowseByContext.TMenuItem",
                    {
                        onclick: SpeedButtonClick,
                    },
                    caption("Browse by &Context"),
                ),
                m("li.MenuBrowseByCommand.TMenuItem",
                    {
                        onclick: SpeedButtonClick,
                    },
                    caption("Browse by C&ommand"),
                ),
                m("li.MenuBrowseByMove.TMenuItem",
                    {
                        onclick: SpeedButtonClick,
                    },
                    caption("Browse by &Move"),
                ),
                m("li.MenuBrowseByRequirements.TMenuItem",
                    {
                        onclick: SpeedButtonClick,
                    },
                    caption("Browse by &Requirements"),
                ),
                m("li.MenuBrowseByChanges.TMenuItem",
                    {
                        onclick: SpeedButtonClick,
                    },
                    caption("Browse by C&hanges"),
                ),
            ),
            m("li.Wizards1.TMenuItem",
                {
                },
                caption("&Tools"),
                m("li.MenuToolsSearch.TMenuItem",
                    {
                        onclick: MenuToolsSearchClick,
                    },
                    caption("&Find..."),
                ),
                m("li.N2.TMenuItem",
                    {
                    },
                    "-",
                ),
                m("li.MenuMapsQuickContexts.TMenuItem",
                    {
                        onclick: MenuMapsQuickContextsClick,
                    },
                    caption("New &Contexts Wizard..."),
                ),
                m("li.MenuMapLinkWizard.TMenuItem",
                    {
                        onclick: MenuMapLinkWizardClick,
                    },
                    caption("New &Moves Wizard..."),
                ),
                m("li.MenuMapQuickCommands.TMenuItem",
                    {
                        onclick: MenuMapQuickCommandsClick,
                    },
                    caption("New C&ommands Wizard..."),
                ),
                m("li.N9.TMenuItem",
                    {
                    },
                    "-",
                ),
                m("li.MenuToolsGenerateJava.TMenuItem",
                    {
                        onclick: MenuToolsGenerateJavaClick,
                    },
                    "Generate Java...",
                ),
                m("li.N14.TMenuItem",
                    {
                    },
                    "-",
                ),
                m("li.MenuEditLogFile.TMenuItem",
                    {
                        onclick: MenuEditLogFileClick,
                    },
                    caption("&Log File..."),
                ),
            ),
            m("li.MenuHelp.TMenuItem",
                {
                },
                caption("&Help"),
                m("li.MenuHelpContents.TMenuItem",
                    {
                        onclick: MenuHelpContentsClick,
                    },
                    caption("&Help Topics"),
                ),
                m("li.MenuHelpBasicConcepts.TMenuItem",
                    {
                        onclick: MenuHelpBasicConceptsClick,
                    },
                    caption("Basic &Concepts"),
                ),
                m("li.MenuHelpTutorial.TMenuItem",
                    {
                        onclick: MenuHelpTutorialClick,
                    },
                    caption("Authoring &Tutorial"),
                ),
                m("li.MenuHelpEditingWorlds.TMenuItem",
                    {
                        onclick: MenuHelpEditingWorldsClick,
                    },
                    caption("Editing &Worlds"),
                ),
                m("li.N6.TMenuItem",
                    {
                    },
                    "-",
                ),
                m("li.MenuHelpRegister.TMenuItem",
                    {
                        onclick: MenuHelpRegisterClick,
                    },
                    caption("&Register..."),
                ),
                m("li.AfterRegisterMenuSeparator.TMenuItem",
                    {
                    },
                    "-",
                ),
                m("li.MenuHelpAbout.TMenuItem",
                    {
                        onclick: MenuHelpAboutClick,
                    },
                    caption("&About..."),
                ),
            ),
        ),
        m("TImageList.ImageList.TImageList",
            {
            },
        ),
        m("TFontDialog.FontDialog.TFontDialog",
            {
            },
        ),
        m("ul.EditPopupMenu.TPopupMenu",
            {
            },
            m("li.PopupCut.TMenuItem",
                {
                    onclick: MenuEditCutClick,
                },
                caption("Cu&t"),
            ),
            m("li.PopupCopy.TMenuItem",
                {
                    onclick: MenuEditCopyClick,
                },
                caption("&Copy"),
            ),
            m("li.PopupPaste.TMenuItem",
                {
                    onclick: MenuEditPasteClick,
                },
                caption("&Paste"),
            ),
        ),
    )
}

function DeleteRuleButtonClick() { console.log("DeleteRuleButtonClick") }

function DuplicateRuleButtonClick() { console.log("DuplicateRuleButtonClick") }

function InsertMusicButtonClick() { console.log("InsertMusicButtonClick") }

function MenuDisplayShowButtonBarClick() { console.log("MenuDisplayShowButtonBarClick") }

function MenuEditClick() { console.log("MenuEditClick") }

function MenuEditCopyClick() { console.log("MenuEditCopyClick") }

function MenuEditCutClick() { console.log("MenuEditCutClick") }

function MenuEditInsertMusicClick() { console.log("MenuEditInsertMusicClick") }

function MenuEditInsertPictureClick() { console.log("MenuEditInsertPictureClick") }

function MenuEditInsertSoundClick() { console.log("MenuEditInsertSoundClick") }

function MenuEditLogFileClick() { console.log("MenuEditLogFileClick") }

function MenuEditPasteClick() { console.log("MenuEditPasteClick") }

function MenuEditPreferencesClick() { console.log("MenuEditPreferencesClick") }

function MenuEditRedoClick() { console.log("MenuEditRedoClick") }

function MenuEditUndoClick() { console.log("MenuEditUndoClick") }

function MenuFileExitClick() { console.log("MenuFileExitClick") }

function MenuFileExportClick() { console.log("MenuFileExportClick") }

function MenuFileMergeWithWorldClick() { console.log("MenuFileMergeWithWorldClick") }

function MenuFileNewWorldClick() { console.log("MenuFileNewWorldClick") }

function MenuFileOpenWorldClick() { console.log("MenuFileOpenWorldClick") }

function MenuFileSaveWorldAsClick() { console.log("MenuFileSaveWorldAsClick") }

function MenuFileSaveWorldClick() { console.log("MenuFileSaveWorldClick") }

function MenuHelpAboutClick() { console.log("MenuHelpAboutClick") }

function MenuHelpBasicConceptsClick() { console.log("MenuHelpBasicConceptsClick") }

function MenuHelpContentsClick() { console.log("MenuHelpContentsClick") }

function MenuHelpEditingWorldsClick() { console.log("MenuHelpEditingWorldsClick") }

function MenuHelpRegisterClick() { console.log("MenuHelpRegisterClick") }

function MenuHelpTutorialClick() { console.log("MenuHelpTutorialClick") }

function MenuMapLinkWizardClick() { console.log("MenuMapLinkWizardClick") }

function MenuMapQuickCommandsClick() { console.log("MenuMapQuickCommandsClick") }

function MenuMapsQuickContextsClick() { console.log("MenuMapsQuickContextsClick") }

function MenuMapsShowCommandsClick() { console.log("MenuMapsShowCommandsClick") }

function MenuOptionsShowRuleEditorClick() { console.log("MenuOptionsShowRuleEditorClick") }

function MenuRuleDeleteClick() { console.log("MenuRuleDeleteClick") }

function MenuRuleDuplicateClick() { console.log("MenuRuleDuplicateClick") }

function MenuRuleLowerClick() { console.log("MenuRuleLowerClick") }

function MenuRuleNewClick() { console.log("MenuRuleNewClick") }

function MenuRuleRaiseClick() { console.log("MenuRuleRaiseClick") }

function MenuRuleTestReplyClick() { console.log("MenuRuleTestReplyClick") }

function MenuToolsGenerateJavaClick() { console.log("MenuToolsGenerateJavaClick") }

function MenuToolsSearchClick() { console.log("MenuToolsSearchClick") }

function MenuWorldSwitchToPlayerClick() { console.log("MenuWorldSwitchToPlayerClick") }

function MoveDownButtonClick() { console.log("MoveDownButtonClick") }

function MoveUpButtonClick() { console.log("MoveUpButtonClick") }

function NewRuleButtonClick() { console.log("NewRuleButtonClick") }

function PopupNewCommandClick() { console.log("PopupNewCommandClick") }

function PopupNewContextClick() { console.log("PopupNewContextClick") }

function PopupNewLinkClick() { console.log("PopupNewLinkClick") }

function SpeedButtonClick() { console.log("SpeedButtonClick") }

function firstListBoxImageClick() { console.log("firstListBoxImageClick") }

function insertSoundClick() { console.log("insertSoundClick") }
