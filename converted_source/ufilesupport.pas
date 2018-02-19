unit ufilesupport;

interface

uses WinTypes, WinProcs, StdCtrls, Graphics;

var
  iniFileChanged: boolean;
  plantFileChanged: boolean;
  gVersionName: string;

type
  SaveFileNamesStructure = record
    tempFile: string;
    newFile: string;
    backupFile: string;
    writingWasSuccessful: boolean;
    end;

const
  kFileTypeAny = 0;
  kFileTypePlant = 1;
  kFileTypeTabbedText = 2;
  kFileTypeStrategy = 3;
  kFileTypeIni = 4;
  kFileTypeExceptionList = 5;
  kFileTypeBitmap = 6;
  kFileTypeTdo = 7;
  kFileTypeDXF = 8;
  kFileTypeWorld = 9;
  kFileTypeSession = 10;
  kFileTypeSound = 11;
  kFileTypeMusic = 12;
  kFileTypeAgentCharacter = 13;
  kWritingWasSuccessful = true;
  kWritingFailed = false;
  kNoSuggestedFile = '';
  kAskForFileName = true; kDontAskForFileName = false;
  kOtherExtOK = true; kOtherExtNotOK = false;

{ file i/o }
function makeFileNameFrom(aString: string): string;
function nameStringForFileType(fileType: smallint): string;
function extensionForFileType(fileType: smallint): string;
function filterStringForFileType(fileType: smallint): string;
function getFileOpenInfo(fileType: smallint; const suggestedFile: string; aTitle: string; allowOtherExtensions: boolean): string;
function fileNameIsOkayForSaving(suggestedFile: string): boolean;
function getFileSaveInfo(fileType: smallint; askForFileName: boolean;
  const suggestedFile: string; var fileInfo: SaveFileNamesStructure): boolean;
procedure startFileSave(var fileInfo: SaveFileNamesStructure);
function cleanUpAfterFileSave(var fileInfo: SaveFileNamesStructure): boolean;
// conversion
function boolToStr(value: boolean): string;
function strToBool(booleanString: string): boolean;
function rectToString(aRect: TRect): string;
function stringToRect(aString: string): TRect;
function pointToString(aPoint: TPoint): string;
function stringToPoint(aString: string): TPoint;

procedure startWaitMessage(waitMessage: string);
procedure stopWaitMessage;

implementation

uses SysUtils, Classes, Dialogs, Forms, FileCtrl, Controls, ExtCtrls, ucursor, usstream;

// unfinished - maybe unneeded

procedure startWaitMessage(waitMessage: string);
  begin
  end;

procedure stopWaitMessage;
  begin
  end;

function boolToStr(value: boolean): string;
  begin
  if value then
    result := 'true'
  else
    result := 'false';
  end;

function strToBool(booleanString: string): boolean;
  begin
  result := false;
  if booleanString = '' then exit;
  if (upperCase(booleanString) = 'TRUE') then
    result := true
  else if (upperCase(booleanString) = 'FALSE') then
    result := false;
  end;

function rectToString(aRect: TRect): string;
  begin
  result := intToStr(aRect.left) + ' ' + intToStr(aRect.top) + ' ' + intToStr(aRect.right) + ' ' + intToStr(aRect.bottom);
  end;

function stringToRect(aString: string): TRect;
	var
    stream: KfStringStream;
  begin
  result := rect(0,0,0,0);
  stream := KfStringStream.create;
  try
    stream.onStringSeparator(aString, ' ');
    result.left := strToIntDef(stream.nextToken, 0);
    result.top := strToIntDef(stream.nextToken, 0);
    result.right := strToIntDef(stream.nextToken, 0);
    result.bottom := strToIntDef(stream.nextToken, 0);
  finally
    stream.free;
  end;
  end;

function pointToString(aPoint: TPoint): string;
  begin
  result := intToStr(aPoint.x) + '  ' + intToStr(aPoint.y);
  end;

function stringToPoint(aString: string): TPoint;
	var
    stream: KfStringStream;
  begin
  result := point(0,0);
  stream := KfStringStream.create;
  try
    stream.onStringSeparator(aString, ' ');
    result.x := strToIntDef(stream.nextToken, 0);
    result.y := strToIntDef(stream.nextToken, 0);
  finally
    stream.free;
  end;
  end;

{ ---------------------------------------------------------------------------- file i/o }
function makeFileNameFrom(aString: string): string;
  var
    done: boolean;
    spacePos: smallint;
  begin
  result := aString;
  done := false;
  while not done do
    begin
    spacePos := pos(' ', result);
    done := (spacePos <= 0);
    if not done then delete(result, spacePos, 1);
    end;
  end;

function nameStringForFileType(fileType: smallint): string;
  begin
  case fileType of
    kFileTypeAny: result := '';
    kFileTypePlant: result := 'Plant';
    kFileTypeTabbedText: result := 'Tabbed text';
    kFileTypeStrategy: result := 'Strategy';
    kFileTypeIni: result := 'Ini';
    kFileTypeExceptionList: result := 'Exception list';
    kFileTypeBitmap: result := 'Bitmap';
    kFileTypeTdo: result := '3D object';
    kFileTypeDXF: result := 'DXF';
    kFileTypeWorld: result := 'World';
    kFileTypeSession: result := 'Session';
    kFileTypeSound: result := 'Sound';
    kFileTypeMusic: result := 'Music';
    kFileTypeAgentCharacter: result := 'Character';
    end;
  end;

function extensionForFileType(fileType: smallint): string;
  begin
  result := '';
  case fileType of
    kFileTypeAny: result := '*';
    kFileTypePlant: result := 'pla';
    kFileTypeTabbedText: result := 'tab';
    kFileTypeStrategy: result := 'str';
    kFileTypeIni: result := 'ini';
    kFileTypeExceptionList: result := 'nex';
    kFileTypeBitmap: result := 'bmp';
    kFileTypeTdo: result := 'tdo';
    kFileTypeDXF: result := 'dxf';
    kFileTypeWorld: result := 'wld';
    kFileTypeSession: result := 'ses';
    kFileTypeSound: result := 'wav';
    kFileTypeMusic: result := 'mid';
    kFileTypeAgentCharacter: result := 'acs';
    end;
  end;

function filterStringForFileType(fileType: smallint): string;
  var extension: string;
  begin
  extension := extensionForFileType(fileType);
  if fileType = kFileTypeAny then
    result := 'All files (*.*)|*.*'
  else
    result := nameStringForFileType(fileType) + ' files (*.' + extension + ')|*.' + extension +
      '|All files (*.*)|*.*';
  end;

function getFileOpenInfo(fileType: smallint; const suggestedFile: string; aTitle: string; allowOtherExtensions: boolean): string;
  var
    fullSuggestedFileName: string;
    openDialog: TOpenDialog;
    nameString: string;
  begin
  result := '';
  openDialog := TOpenDialog.create(Application);
  try
  with openDialog do
    begin
    if suggestedFile = '' then
      begin
      fileName := '*.' + extensionForFileType(fileType);
      end
    else
      begin
      fullSuggestedFileName := expandFileName(suggestedFile);
      { if directory does not exist, will leave as it was }
      initialDir := extractFilePath(fullSuggestedFileName);
      if fileExists(fullSuggestedFileName) then fileName := extractFileName(fullSuggestedFileName);
      end;
    nameString := nameStringForFileType(fileType);
    if length(aTitle) > 0 then
      title := aTitle
    else if nameString[1] in ['A', 'E', 'I', 'O', 'U'] then
      title := 'Choose an ' + nameString + ' file'
    else
      title := 'Choose a ' + nameString + ' file';
    filter := filterStringForFileType(fileType);
    defaultExt := extensionForFileType(fileType);
    options := options + [ofPathMustExist, ofFileMustExist, ofHideReadOnly];
   end;
  if openDialog.execute then
    begin
    if (ofExtensionDifferent in openDialog.options) and (not allowOtherExtensions) then
      begin
      showMessage('The file (' + openDialog.fileName
        + ') does not have the correct extension (' + openDialog.defaultExt + ').');
      exit;
      end
    else
      result := openDialog.fileName;
    end;
  finally
  openDialog.free;
  end;
  end;

function fileNameIsOkayForSaving(suggestedFile: string): boolean;
  var
    fullSuggestedFileName: string;
  begin
  result := false;
  if length(suggestedFile) = 0 then exit;
  fullSuggestedFileName := expandFileName(suggestedFile);
  { check if directory exists }
  if not directoryExists(extractFilePath(fullSuggestedFileName)) then
    begin
    showMessage('The directory ' + extractFilePath(fullSuggestedFileName) + ' does not exist.');
    exit;
    end;
  { if file exists and is writable, it's ok because Save (not Save As) should not ask to rewrite }
  { if file exists but is read-only, quit  }
  if fileExists(fullSuggestedFileName)
      and Boolean(FileGetAttr(fullSuggestedFileName) and faReadOnly) then
    begin
    showMessage('The file ' + fullSuggestedFileName + ' exists and is read-only.');
    exit;
    end;
  result := true;
  end;

function getFileSaveInfo(fileType: smallint; askForFileName: boolean;
    const suggestedFile: string; var fileInfo: SaveFileNamesStructure): boolean;
  var
    saveDialog: TSaveDialog;
    tryBackupName, tryTempName, fullSuggestedFileName, prompt, extension: string;
    index: smallint;
    tempFileHandle: longint;
  begin
  result := false;
  saveDialog := TSaveDialog.create(application);
  try
  { default info }
  with fileInfo do
    begin
    tempFile := '';
    newFile := '';
    backupFile := '';
    writingWasSuccessful := false;
    end;
  { if this is a Save, try to set the file name from the suggestedFile given; if file name
    is invalid, set flag to move into Save As instead }
  if not askForFileName then
    begin
    askForFileName := not fileNameIsOkayForSaving(suggestedFile);
    if not askForFileName then fileInfo.newFile := expandFileName(suggestedFile);
    end;
  { if this is a Save As, or if this is a Save and the file in suggestedFile is invalid,
    ask user for a file name }
  if askForFileName then
    begin
    with saveDialog do
      begin
      if length(suggestedFile) > 0 then
        begin
        fullSuggestedFileName := expandFileName(suggestedFile);
        { if directory does not exist, will leave as it was }
        initialDir := extractFilePath(fullSuggestedFileName);
        { don't check if file exists (because saving); check if dir exists }
        if directoryExists(extractFilePath(fullSuggestedFileName)) then
          fileName := extractFileName(fullSuggestedFileName);
        end;
      filter := filterStringForFileType(fileType);
      defaultExt := extensionForFileType(fileType);
      options := options + [ofPathMustExist, ofOverwritePrompt, ofHideReadOnly, ofNoReadOnlyReturn];
      end;
    if not saveDialog.execute then
      begin
      saveDialog.free;
      saveDialog := nil;
      exit;
      end;
    fileInfo.newFile := saveDialog.fileName;
    end;
  { set backup file name, check if read-only }
  { changed backup file extension to put tilde first because it is better to have all backup files sort together }
  try
    extension := extractFileExt(fileInfo.newFile);   { includes dot }
    extension := '.~' + copy(extension, 2, 2);
  except
    extension := '.bak';
  end;
  tryBackupName := changeFileExt(fileInfo.newFile, extension); 
  if fileExists(tryBackupName) then
    begin
    if (boolean(fileGetAttr(tryBackupName) and faReadOnly)) then
      begin
      prompt := 'The backup file ' + tryBackupName + ' is read-only. Continue?';
      if messageDlg(prompt, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
        exit;
      end;
    end
  else
    fileInfo.backupFile := tryBackupName;
  { set temp file name }
  for index := 100 to 999 do
    begin
    tryTempName := changeFileExt(fileInfo.newFile, '.' + intToStr(index));
    if not fileExists(tryTempName) then
      begin
      fileInfo.tempFile := tryTempName;
      break;
      end;
    end;
  { if can't find unused temp file, quit }
  if fileInfo.tempFile = '' then
    begin
    showMessage('Could not create temporary file ' + tryTempName + '.');
    exit;
    end;
  { test whether temp file can be created }
  tempFileHandle := fileCreate(fileInfo.tempFile);
  if tempFileHandle > 0 then
    begin
    fileClose(tempFileHandle);
    if not deleteFile(fileInfo.tempFile) then
      begin
      showMessage('Problem with temporary file ' + fileInfo.tempFile + '.');
      exit;
      end;
    end
  else
    begin
    showMessage('Could not write to temporary file ' + fileInfo.tempFile + '.');
    exit;
    end;
  result := true;
  finally
  saveDialog.free;
//  saveDialog := nil;
  end;
  end;

procedure startFileSave(var fileInfo: SaveFileNamesStructure);
  begin
  cursor_startWait;
  startWaitMessage('Saving ' + extractFileName(fileInfo.newFile) + '...');
  end;

function cleanUpAfterFileSave(var fileInfo: SaveFileNamesStructure): boolean;
  var
    useBackup, renamingFailed, deletingFailed: boolean;
    prompt: string;
  begin
  result := false;
  cursor_stopWait;
  stopWaitMessage;
  useBackup := true;
  {if couldn't write, then remove temp file and exit without warning}
  if not fileInfo.writingWasSuccessful then
    begin
    deleteFile(fileInfo.tempFile);
    exit;
    end;
  {remove backup file if exists from prior backup}
  if fileInfo.backupFile <> '' then
    begin
    if fileExists(fileInfo.backupFile) then
      begin
      {try to delete backup file}
      deletingFailed := not deleteFile(fileInfo.backupFile);
      if deletingFailed then
        begin
        {couldn't delete backup file}
        prompt := 'Could not write backup file ' + fileInfo.backupFile + '. Continue?';
        if messageDlg(prompt, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
          begin
          {user doesn't want to proceed - so cleanup temp file}
          deleteFile(fileInfo.tempFile);
          exit;
          end
        else
          useBackup := false;
        end;
      end
    end
  else
    useBackup := false;
  {if original file exists make backup if requested...}
  if fileExists(fileInfo.newFile) then
    begin
    if useBackup then
      begin
      {rename old copy of new file to make backup}
      renamingFailed := not renameFile(fileInfo.newFile, fileInfo.backupFile);
      if renamingFailed then
        begin
        prompt := 'Could not rename old file to backup file ' + fileInfo.backupFile + '. Continue?';
        if messageDlg(prompt, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
          begin
          {user doesn't want to proceed - so cleanup temp file}
          deleteFile(fileInfo.tempFile);
          exit;
          end
        else
          useBackup := false;
        end;
      end;
    {could not create backup file - so just delete old file instead of renaming}
    if not useBackup then
      begin
      deletingFailed := not deleteFile(fileInfo.newFile);
      if deletingFailed then
        begin
        ShowMessage('Could not write file ' + fileInfo.newFile);
        exit;
        end;
      end;
    end;
  {rename temp file to newFile name}
  renamingFailed := not renameFile(fileInfo.tempFile, fileInfo.newFile);
  if renamingFailed then
    begin
    {clean up by removing temp file}
    ShowMessage('Could not write file ' + fileInfo.newFile + ' from ' + fileInfo.tempFile);
    DeleteFile(fileInfo.tempFile);
    exit;
    end;
  result := true;
  end;

begin
gVersionName := 'Version 1.31';
iniFileChanged := false;
plantFileChanged := false;
end.
 
