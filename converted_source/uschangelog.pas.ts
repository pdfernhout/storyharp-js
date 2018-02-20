// unit uschangelog

from conversion_common import *
import usdomain
import delphi_compatability

const uschangelog = uschangelog || {}

// var
let ChangeLogForm: TChangeLogForm


function makeBackupCopy(fileFrom: string, fileTo: string): void {
    let fileFromCString: char[] /* 255 + 1 */
    let fileToCString: char[] /* 255 + 1 */
    
    UNRESOLVED.strPCopy(fileFromCString, fileFrom)
    UNRESOLVED.strPCopy(fileToCString, fileTo)
    //overwrite existing file
    UNRESOLVED.copyFile(fileFromCString, fileToCString, false)
}


export class TChangeLogForm {
    LogContentsRichEdit: TRichEdit = new TRichEdit()
    OpenDialog: TOpenDialog = new TOpenDialog()
    bottomPanel: TPanel = new TPanel()
    CopySelectedTextButton: TButton = new TButton()
    UpdateButton: TButton = new TButton()
    changeLogFile: TButton = new TButton()
    clearLogFile: TButton = new TButton()
    helpButton: TButton = new TButton()
    issuedWarning: boolean = false
    TChangeLogForm.prototype = new TForm()
    TChangeLogForm.prototype.constructor = TChangeLogForm
    
    //$R *.DFM
    FormCreate(Sender: TObject): void {
        usdomain.domain.setFormSize(this, usdomain.domain.options.logFileWindowRect)
    }
    
    fileErrorWarning(): void {
        if (this.issuedWarning) {
            return
        }
        ShowMessage("Could not write to the log file " + usdomain.domain.options.logFileName + String.fromCharCode(13) + String.fromCharCode(10) + "The file may be write protected or in use by another program, or your disk may be full." + String.fromCharCode(13) + String.fromCharCode(10) + "Your changes will not be logged.")
        this.issuedWarning = true
    }
    
    ensureLogFileName(): void {
        if (usdomain.domain.options.logFileName === "") {
            usdomain.domain.options.logFileName = ExtractFilePath(delphi_compatability.Application.exeName) + usdomain.kDefaultLogFileName
        }
    }
    
    addToLog(change: string): void {
        let LogFile: TextFile
        
        if (trim(change) === "") {
            return
        }
        this.ensureLogFileName()
        AssignFile(LogFile, usdomain.domain.options.logFileName)
        try {
            try {
                if (FileExists(usdomain.domain.options.logFileName)) {
                    UNRESOLVED.Append(LogFile)
                } else {
                    Rewrite(LogFile)
                }
            } catch (Exception e) {
                this.fileErrorWarning()
                return
            }
            writeln(LogFile, change)
            Flush(LogFile)
        } finally {
            try {
                CloseFile(LogFile)
            } catch (Exception e) {
                if (!this.issuedWarning) {
                    ShowMessage("Problem closing log file " + usdomain.domain.options.logFileName)
                }
            }
        }
    }
    
    clearLog(): void {
        let LogFile: TextFile
        
        this.ensureLogFileName()
        AssignFile(LogFile, usdomain.domain.options.logFileName)
        try {
            try {
                Rewrite(LogFile)
            } catch (Exception e) {
                this.fileErrorWarning()
                return
            }
        } finally {
            try {
                CloseFile(LogFile)
            } catch (Exception e) {
                if (!this.issuedWarning) {
                    ShowMessage("Problem closing log file " + usdomain.domain.options.logFileName)
                }
            }
        }
        this.loadChangeLog()
    }
    
    CopySelectedTextButtonClick(Sender: TObject): void {
        this.LogContentsRichEdit.CopyToClipboard
        //self.modalResult := 1;   
    }
    
    OKButtonClick(Sender: TObject): void {
        //self.modalResult := 1;
        this.Close()
    }
    
    loadChangeLog(): void {
        this.ensureLogFileName()
        if (!FileExists(usdomain.domain.options.logFileName)) {
            return
        }
        this.LogContentsRichEdit.Text = ""
        this.LogContentsRichEdit.lines.loadFromFile(usdomain.domain.options.logFileName)
        this.Caption = "Log file " + ExtractFileName(usdomain.domain.options.logFileName)
    }
    
    scrollLogEndIntoView(): void {
        this.LogContentsRichEdit.SelStart = len(this.LogContentsRichEdit.Text)
        this.LogContentsRichEdit.SelLength = 0
        this.LogContentsRichEdit.Perform(UNRESOLVED.EM_SCROLLCARET, 0, 0)
    }
    
    UpdateButtonClick(Sender: TObject): void {
        this.loadChangeLog()
        this.scrollLogEndIntoView()
    }
    
    bottomPanelResize(Sender: TObject): void {
        this.CopySelectedTextButton.Left = 4
        this.helpButton.Left = this.bottomPanel.ClientWidth - this.helpButton.Width - 4
        this.changeLogFile.Left = this.helpButton.Left - this.changeLogFile.Width - 4
        this.clearLogFile.Left = this.changeLogFile.Left - this.clearLogFile.Width - 4
        this.UpdateButton.Left = this.clearLogFile.Left - this.UpdateButton.Width - 4
    }
    
    clearLogFileClick(Sender: TObject): void {
        let backupLogFileName: string
        
        if (MessageDialog("Are you sure you want to clear the log file?" + chr(13) + "This is not undoable, but a backup file will be made.", mtConfirmation, {mbYes, mbNo, }, 0) === delphi_compatability.IDNO) {
            return
        }
        backupLogFileName = ChangeFileExt(usdomain.domain.options.logFileName, ".~lo")
        makeBackupCopy(usdomain.domain.options.logFileName, backupLogFileName)
        this.clearLog()
    }
    
    changeLogFileClick(Sender: TObject): void {
        let backupLogFileName: string
        
        this.OpenDialog.Title = "Choose or type in a new log file name"
        this.OpenDialog.FileName = usdomain.domain.options.logFileName
        this.OpenDialog.Filter = "Log files (*.log)|*.log|All files (*.*)|*.*"
        this.OpenDialog.DefaultExt = "log"
        if (this.OpenDialog.Execute()) {
            backupLogFileName = ChangeFileExt(usdomain.domain.options.logFileName, ".~lo")
            makeBackupCopy(usdomain.domain.options.logFileName, backupLogFileName)
            usdomain.domain.options.logFileName = this.OpenDialog.FileName
            if (!FileExists(usdomain.domain.options.logFileName)) {
                this.clearLog()
            }
            this.loadChangeLog()
            this.scrollLogEndIntoView()
        }
    }
    
    WMGetMinMaxInfo(MSG: Tmessage): void {
        TForm.prototype.WMGetMinMaxInfo.call(this)
        //FIX unresolved WITH expression: UNRESOLVED.PMinMaxInfo(MSG.lparam).PDF_FIX_POINTER_ACCESS
        UNRESOLVED.ptMinTrackSize.x = 316
        UNRESOLVED.ptMinTrackSize.y = 100
    }
    
    helpButtonClick(Sender: TObject): void {
        delphi_compatability.Application.HelpJump("Using_the_Change_Log_file_to_recover_text")
    }
    
    FormKeyUp(Sender: TObject, Key: byte, Shift: TShiftState): void {
        if (Key === delphi_compatability.VK_F2) {
            this.showTechSupportInfoInChangeLog()
        }
        return Key
    }
    
    showTechSupportInfoInChangeLog(): void {
        let numRules: long
        let numVariables: long
        let memoryStatus: TMemoryStatus
        let diskSpace: long
        let screenColorBits: int
        let screenColors: long
        let screenDC: HDC
        
        this.LogContentsRichEdit.lines.add("")
        this.LogContentsRichEdit.lines.add("StoryHarp Technical Support Information")
        this.LogContentsRichEdit.lines.add("")
        numRules = usdomain.domain.world.rules.Count
        this.LogContentsRichEdit.lines.add("Rules: " + IntToStr(numRules))
        numVariables = usdomain.domain.world.variables.Count
        this.LogContentsRichEdit.lines.add("Variables: " + IntToStr(numVariables))
        // more here...
        this.LogContentsRichEdit.lines.add("")
        screenDC = UNRESOLVED.GetDC(0)
        try {
            screenColorBits = (UNRESOLVED.GetDeviceCaps(screenDC, delphi_compatability.BITSPIXEL) * UNRESOLVED.GetDeviceCaps(screenDC, delphi_compatability.PLANES))
        } finally {
            UNRESOLVED.ReleaseDC(0, screenDC)
        }
        if (screenColorBits !== 32) {
            screenColors = 1 << screenColorBits
        } else {
            screenColors = intround(UNRESOLVED.power(2.0, screenColorBits))
        }
        this.LogContentsRichEdit.lines.add("Colors: " + IntToStr(screenColors) + " (" + IntToStr(screenColorBits) + " bits)")
        this.LogContentsRichEdit.lines.add("Size: " + IntToStr(delphi_compatability.Screen.Width) + " x " + IntToStr(delphi_compatability.Screen.Height))
        this.LogContentsRichEdit.lines.add("Resolution: " + IntToStr(delphi_compatability.Screen.PixelsPerInch) + " pixels/inch")
        this.LogContentsRichEdit.lines.add("")
        this.GetOSInfo()
        memoryStatus.dwLength = FIX_sizeof(memoryStatus)
        UNRESOLVED.GlobalMemoryStatus(memoryStatus)
        this.LogContentsRichEdit.lines.add("Percent memory in use: " + IntToStr(memoryStatus.dwMemoryLoad))
        this.LogContentsRichEdit.lines.add("Total physical memory: " + IntToStr(memoryStatus.dwTotalPhys / 1024) + " K")
        this.LogContentsRichEdit.lines.add("Available physical memory: " + IntToStr(memoryStatus.dwAvailPhys / 1024) + " K")
        this.LogContentsRichEdit.lines.add("Total paging file: " + IntToStr(memoryStatus.dwTotalPageFile / 1024) + " K")
        this.LogContentsRichEdit.lines.add("Available paging file: " + IntToStr(memoryStatus.dwAvailPageFile / 1024) + " K")
        this.LogContentsRichEdit.lines.add("Total user memory: " + IntToStr(memoryStatus.dwTotalVirtual / 1024) + " K")
        this.LogContentsRichEdit.lines.add("Available user memory: " + IntToStr(memoryStatus.dwAvailVirtual / 1024) + " K")
        diskSpace = UNRESOLVED.DiskFree(0)
        this.LogContentsRichEdit.lines.add("Disk space on current drive: " + IntToStr(diskSpace / (1024 * 1024)) + " MB")
    }
    
    GetOSInfo(): void {
        let Platform: string
        let BuildNumber: int
        
        switch (UNRESOLVED.Win32Platform) {
            case UNRESOLVED.VER_PLATFORM_WIN32_WINDOWS:
                Platform = "Windows 95"
                BuildNumber = UNRESOLVED.Win32BuildNumber && 0x0000FFFF
                break
            case UNRESOLVED.VER_PLATFORM_WIN32_NT:
                Platform = "Windows NT"
                BuildNumber = UNRESOLVED.Win32BuildNumber
                break
            default:
                Platform = "Windows"
                BuildNumber = 0
                break
        if ((UNRESOLVED.Win32Platform === UNRESOLVED.VER_PLATFORM_WIN32_WINDOWS) || (UNRESOLVED.Win32Platform === UNRESOLVED.VER_PLATFORM_WIN32_NT)) {
            if (UNRESOLVED.Win32CSDVersion === "") {
                this.LogContentsRichEdit.lines.add(UNRESOLVED.Format("%s %d.%d (Build %d)", {Platform, UNRESOLVED.Win32MajorVersion, UNRESOLVED.Win32MinorVersion, BuildNumber, }))
            } else {
                this.LogContentsRichEdit.lines.add(UNRESOLVED.Format("%s %d.%d (Build %d: %s)", {Platform, UNRESOLVED.Win32MajorVersion, UNRESOLVED.Win32MinorVersion, BuildNumber, UNRESOLVED.Win32CSDVersion, }))
            }
        } else {
            this.LogContentsRichEdit.lines.add(UNRESOLVED.Format("%s %d.%d", {Platform, UNRESOLVED.Win32MajorVersion, UNRESOLVED.Win32MinorVersion, }))
        }
    }
    
}

