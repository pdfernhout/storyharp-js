// unit ufilesupport

from conversion_common import *
import usstream
import ucursor
import delphi_compatability

// var
let iniFileChanged: boolean
let plantFileChanged: boolean
let gVersionName: string


// record
export interface SaveFileNamesStructure {
    tempFile: string
    newFile: string
    backupFile: string
    writingWasSuccessful: boolean
}

// const
const kFileTypeAny = 0
const kFileTypePlant = 1
const kFileTypeTabbedText = 2
const kFileTypeStrategy = 3
const kFileTypeIni = 4
const kFileTypeExceptionList = 5
const kFileTypeBitmap = 6
const kFileTypeTdo = 7
const kFileTypeDXF = 8
const kFileTypeWorld = 9
const kFileTypeSession = 10
const kFileTypeSound = 11
const kFileTypeMusic = 12
const kFileTypeAgentCharacter = 13
const kWritingWasSuccessful = true
const kWritingFailed = false
const kNoSuggestedFile = ""
const kAskForFileName = true
const kDontAskForFileName = false
const kOtherExtOK = true
const kOtherExtNotOK = false


// unfinished - maybe unneeded
function startWaitMessage(waitMessage: string): void {
}

function stopWaitMessage(): void {
}

// conversion
function boolToStr(value: boolean): string {
    let result = ""
    if (value) {
        result = "true"
    } else {
        result = "false"
    }
    return result
}

function strToBool(booleanString: string): boolean {
    let result = false
    result = false
    if (booleanString === "") {
        return result
    }
    if ((uppercase(booleanString) === "TRUE")) {
        result = true
    } else if ((uppercase(booleanString) === "FALSE")) {
        result = false
    }
    return result
}

function rectToString(aRect: TRect): string {
    let result = ""
    result = IntToStr(aRect.Left) + " " + IntToStr(aRect.Top) + " " + IntToStr(aRect.Right) + " " + IntToStr(aRect.Bottom)
    return result
}

function stringToRect(aString: string): TRect {
    let result = new TRect()
    let stream: KfStringStream
    
    result = Rect(0, 0, 0, 0)
    stream = usstream.KfStringStream.create
    try {
        stream.onStringSeparator(aString, " ")
        result.Left = StrToIntDef(stream.nextToken(), 0)
        result.Top = StrToIntDef(stream.nextToken(), 0)
        result.Right = StrToIntDef(stream.nextToken(), 0)
        result.Bottom = StrToIntDef(stream.nextToken(), 0)
    } finally {
        stream.free
    }
    return result
}

function pointToString(aPoint: TPoint): string {
    let result = ""
    result = IntToStr(aPoint.X) + "  " + IntToStr(aPoint.Y)
    return result
}

function stringToPoint(aString: string): TPoint {
    let result = new TPoint()
    let stream: KfStringStream
    
    result = point(0, 0)
    stream = usstream.KfStringStream.create
    try {
        stream.onStringSeparator(aString, " ")
        result.X = StrToIntDef(stream.nextToken(), 0)
        result.Y = StrToIntDef(stream.nextToken(), 0)
    } finally {
        stream.free
    }
    return result
}

// file i/o 
// ---------------------------------------------------------------------------- file i/o 
function makeFileNameFrom(aString: string): string {
    let result = ""
    let done: boolean
    let spacePos: short
    
    result = aString
    done = false
    while (!done) {
        spacePos = UNRESOLVED.pos(" ", result)
        done = (spacePos <= 0)
        if (!done) {
            UNRESOLVED.delete(result, spacePos, 1)
        }
    }
    return result
}

function nameStringForFileType(fileType: short): string {
    let result = ""
    switch (fileType) {
        case kFileTypeAny:
            result = ""
            break
        case kFileTypePlant:
            result = "Plant"
            break
        case kFileTypeTabbedText:
            result = "Tabbed text"
            break
        case kFileTypeStrategy:
            result = "Strategy"
            break
        case kFileTypeIni:
            result = "Ini"
            break
        case kFileTypeExceptionList:
            result = "Exception list"
            break
        case kFileTypeBitmap:
            result = "Bitmap"
            break
        case kFileTypeTdo:
            result = "3D object"
            break
        case kFileTypeDXF:
            result = "DXF"
            break
        case kFileTypeWorld:
            result = "World"
            break
        case kFileTypeSession:
            result = "Session"
            break
        case kFileTypeSound:
            result = "Sound"
            break
        case kFileTypeMusic:
            result = "Music"
            break
        case kFileTypeAgentCharacter:
            result = "Character"
            break
    return result
}

function extensionForFileType(fileType: short): string {
    let result = ""
    result = ""
    switch (fileType) {
        case kFileTypeAny:
            result = "*"
            break
        case kFileTypePlant:
            result = "pla"
            break
        case kFileTypeTabbedText:
            result = "tab"
            break
        case kFileTypeStrategy:
            result = "str"
            break
        case kFileTypeIni:
            result = "ini"
            break
        case kFileTypeExceptionList:
            result = "nex"
            break
        case kFileTypeBitmap:
            result = "bmp"
            break
        case kFileTypeTdo:
            result = "tdo"
            break
        case kFileTypeDXF:
            result = "dxf"
            break
        case kFileTypeWorld:
            result = "wld"
            break
        case kFileTypeSession:
            result = "ses"
            break
        case kFileTypeSound:
            result = "wav"
            break
        case kFileTypeMusic:
            result = "mid"
            break
        case kFileTypeAgentCharacter:
            result = "acs"
            break
    return result
}

function filterStringForFileType(fileType: short): string {
    let result = ""
    let extension: string
    
    extension = extensionForFileType(fileType)
    if (fileType === kFileTypeAny) {
        result = "All files (*.*)|*.*"
    } else {
        result = nameStringForFileType(fileType) + " files (*." + extension + ")|*." + extension + "|All files (*.*)|*.*"
    }
    return result
}

function getFileOpenInfo(fileType: short, suggestedFile: string, aTitle: string, allowOtherExtensions: boolean): string {
    let result = ""
    let fullSuggestedFileName: string
    let openDialog: TOpenDialog
    let nameString: string
    
    result = ""
    openDialog = delphi_compatability.TOpenDialog().Create(delphi_compatability.Application)
    try {
        if (suggestedFile === "") {
            openDialog.FileName = "*." + extensionForFileType(fileType)
        } else {
            fullSuggestedFileName = ExpandFileName(suggestedFile)
            // if directory does not exist, will leave as it was 
            openDialog.InitialDir = ExtractFilePath(fullSuggestedFileName)
            if (FileExists(fullSuggestedFileName)) {
                openDialog.FileName = ExtractFileName(fullSuggestedFileName)
            }
        }
        nameString = nameStringForFileType(fileType)
        if (len(aTitle) > 0) {
            openDialog.Title = aTitle
        } else if (nameString[1] in {"A", "E", "I", "O", "U", }) {
            openDialog.Title = "Choose an " + nameString + " file"
        } else {
            openDialog.Title = "Choose a " + nameString + " file"
        }
        openDialog.Filter = filterStringForFileType(fileType)
        openDialog.DefaultExt = extensionForFileType(fileType)
        openDialog.Options = openDialog.Options + {delphi_compatability.TOpenOption.ofPathMustExist, delphi_compatability.TOpenOption.ofFileMustExist, delphi_compatability.TOpenOption.ofHideReadOnly, }
        if (openDialog.Execute()) {
            if ((delphi_compatability.TOpenOption.ofExtensionDifferent in openDialog.Options) && (!allowOtherExtensions)) {
                ShowMessage("The file (" + openDialog.FileName + ") does not have the correct extension (" + openDialog.DefaultExt + ").")
                return result
            } else {
                result = openDialog.FileName
            }
        }
    } finally {
        openDialog.free
    }
    return result
}

function fileNameIsOkayForSaving(suggestedFile: string): boolean {
    let result = false
    let fullSuggestedFileName: string
    
    result = false
    if (len(suggestedFile) === 0) {
        return result
    }
    fullSuggestedFileName = ExpandFileName(suggestedFile)
    if (!UNRESOLVED.directoryExists(ExtractFilePath(fullSuggestedFileName))) {
        // check if directory exists 
        ShowMessage("The directory " + ExtractFilePath(fullSuggestedFileName) + " does not exist.")
        return result
    }
    if (FileExists(fullSuggestedFileName) && UNRESOLVED.FileGetAttr(fullSuggestedFileName) && UNRESOLVED.faReadOnly) {
        // if file exists and is writable, it's ok because Save (not Save As) should not ask to rewrite 
        // if file exists but is read-only, quit  
        ShowMessage("The file " + fullSuggestedFileName + " exists and is read-only.")
        return result
    }
    result = true
    return result
}

function getFileSaveInfo(fileType: short, askForFileName: boolean, suggestedFile: string, fileInfo: SaveFileNamesStructure): boolean {
    let result = false
    let saveDialog: TSaveDialog
    let tryBackupName: string
    let tryTempName: string
    let fullSuggestedFileName: string
    let prompt: string
    let extension: string
    let index: short
    let tempFileHandle: long
    
    result = false
    saveDialog = delphi_compatability.TSaveDialog().Create(delphi_compatability.Application)
    //  saveDialog := nil;
    try {
        // default info 
        fileInfo.tempFile = ""
        fileInfo.newFile = ""
        fileInfo.backupFile = ""
        fileInfo.writingWasSuccessful = false
        if (!askForFileName) {
            // if this is a Save, try to set the file name from the suggestedFile given; if file name
            //    is invalid, set flag to move into Save As instead 
            askForFileName = !fileNameIsOkayForSaving(suggestedFile)
            if (!askForFileName) {
                fileInfo.newFile = ExpandFileName(suggestedFile)
            }
        }
        if (askForFileName) {
            if (len(suggestedFile) > 0) {
                // if this is a Save As, or if this is a Save and the file in suggestedFile is invalid,
                //    ask user for a file name 
                fullSuggestedFileName = ExpandFileName(suggestedFile)
                // if directory does not exist, will leave as it was 
                saveDialog.InitialDir = ExtractFilePath(fullSuggestedFileName)
                if (UNRESOLVED.directoryExists(ExtractFilePath(fullSuggestedFileName))) {
                    // don't check if file exists (because saving); check if dir exists 
                    saveDialog.FileName = ExtractFileName(fullSuggestedFileName)
                }
            }
            saveDialog.Filter = filterStringForFileType(fileType)
            saveDialog.DefaultExt = extensionForFileType(fileType)
            saveDialog.Options = saveDialog.Options + {delphi_compatability.TOpenOption.ofPathMustExist, delphi_compatability.TOpenOption.ofOverwritePrompt, delphi_compatability.TOpenOption.ofHideReadOnly, delphi_compatability.TOpenOption.ofNoReadOnlyReturn, }
            if (!saveDialog.Execute()) {
                saveDialog.free
                saveDialog = null
                return result
            }
            fileInfo.newFile = saveDialog.FileName
        }
        try {
            // set backup file name, check if read-only 
            // changed backup file extension to put tilde first because it is better to have all backup files sort together 
            // includes dot 
            extension = ExtractFileExt(fileInfo.newFile)
            extension = ".~" + UNRESOLVED.copy(extension, 2, 2)
        } catch (Exception e) {
            extension = ".bak"
        }
        tryBackupName = ChangeFileExt(fileInfo.newFile, extension)
        if (FileExists(tryBackupName)) {
            if ((UNRESOLVED.fileGetAttr(tryBackupName) && UNRESOLVED.faReadOnly)) {
                prompt = "The backup file " + tryBackupName + " is read-only. Continue?"
                if (MessageDialog(prompt, mtConfirmation, {mbYes, mbNo, }, 0) !== mrYes) {
                    return result
                }
            }
        } else {
            fileInfo.backupFile = tryBackupName
        }
        for (index = 100; index <= 999; index++) {
            // set temp file name 
            tryTempName = ChangeFileExt(fileInfo.newFile, "." + IntToStr(index))
            if (!FileExists(tryTempName)) {
                fileInfo.tempFile = tryTempName
                break
            }
        }
        if (fileInfo.tempFile === "") {
            // if can't find unused temp file, quit 
            ShowMessage("Could not create temporary file " + tryTempName + ".")
            return result
        }
        // test whether temp file can be created 
        tempFileHandle = UNRESOLVED.fileCreate(fileInfo.tempFile)
        if (tempFileHandle > 0) {
            UNRESOLVED.fileClose(tempFileHandle)
            if (!DeleteFile(fileInfo.tempFile)) {
                ShowMessage("Problem with temporary file " + fileInfo.tempFile + ".")
                return result
            }
        } else {
            ShowMessage("Could not write to temporary file " + fileInfo.tempFile + ".")
            return result
        }
        result = true
    } finally {
        saveDialog.free
    }
    return result
}

function startFileSave(fileInfo: SaveFileNamesStructure): void {
    ucursor.cursor_startWait()
    startWaitMessage("Saving " + ExtractFileName(fileInfo.newFile) + "...")
}

function cleanUpAfterFileSave(fileInfo: SaveFileNamesStructure): boolean {
    let result = false
    let useBackup: boolean
    let renamingFailed: boolean
    let deletingFailed: boolean
    let prompt: string
    
    result = false
    ucursor.cursor_stopWait()
    stopWaitMessage()
    useBackup = true
    if (!fileInfo.writingWasSuccessful) {
        //if couldn't write, then remove temp file and exit without warning
        DeleteFile(fileInfo.tempFile)
        return result
    }
    if (fileInfo.backupFile !== "") {
        if (FileExists(fileInfo.backupFile)) {
            //remove backup file if exists from prior backup
            //try to delete backup file
            deletingFailed = !DeleteFile(fileInfo.backupFile)
            if (deletingFailed) {
                //couldn't delete backup file
                prompt = "Could not write backup file " + fileInfo.backupFile + ". Continue?"
                if (MessageDialog(prompt, mtConfirmation, {mbYes, mbNo, }, 0) !== mrYes) {
                    //user doesn't want to proceed - so cleanup temp file
                    DeleteFile(fileInfo.tempFile)
                    return result
                } else {
                    useBackup = false
                }
            }
        }
    } else {
        useBackup = false
    }
    if (FileExists(fileInfo.newFile)) {
        if (useBackup) {
            //if original file exists make backup if requested...
            //rename old copy of new file to make backup
            renamingFailed = !RenameFile(fileInfo.newFile, fileInfo.backupFile)
            if (renamingFailed) {
                prompt = "Could not rename old file to backup file " + fileInfo.backupFile + ". Continue?"
                if (MessageDialog(prompt, mtConfirmation, {mbYes, mbNo, }, 0) !== mrYes) {
                    //user doesn't want to proceed - so cleanup temp file
                    DeleteFile(fileInfo.tempFile)
                    return result
                } else {
                    useBackup = false
                }
            }
        }
        if (!useBackup) {
            //could not create backup file - so just delete old file instead of renaming
            deletingFailed = !DeleteFile(fileInfo.newFile)
            if (deletingFailed) {
                ShowMessage("Could not write file " + fileInfo.newFile)
                return result
            }
        }
    }
    //rename temp file to newFile name
    renamingFailed = !RenameFile(fileInfo.tempFile, fileInfo.newFile)
    if (renamingFailed) {
        //clean up by removing temp file
        ShowMessage("Could not write file " + fileInfo.newFile + " from " + fileInfo.tempFile)
        DeleteFile(fileInfo.tempFile)
        return result
    }
    result = true
    return result
}


