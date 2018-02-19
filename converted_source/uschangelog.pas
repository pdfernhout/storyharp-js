unit uschangelog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type
  TChangeLogForm = class(TForm)
    LogContentsRichEdit: TRichEdit;
    OpenDialog: TOpenDialog;
    bottomPanel: TPanel;
    CopySelectedTextButton: TButton;
    UpdateButton: TButton;
    changeLogFile: TButton;
    clearLogFile: TButton;
    helpButton: TButton;
    procedure CopySelectedTextButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bottomPanelResize(Sender: TObject);
    procedure clearLogFileClick(Sender: TObject);
    procedure changeLogFileClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    procedure WMGetMinMaxInfo(var MSG: Tmessage);  message WM_GetMinMaxInfo;
  public
    { Public declarations }
    issuedWarning: boolean;
		procedure fileErrorWarning;
		procedure ensureLogFileName;
    procedure addToLog(change: string);
		procedure clearLog;
		procedure loadChangeLog;
		procedure scrollLogEndIntoView;
		procedure showTechSupportInfoInChangeLog;
		procedure GetOSInfo;
  end;

var
  ChangeLogForm: TChangeLogForm;

implementation

{$R *.DFM}

uses Math, USDomain;

procedure TChangeLogForm.FormCreate(Sender: TObject);
	begin
  domain.setFormSize(self, domain.options.logFileWindowRect);
	end;

procedure TChangeLogForm.fileErrorWarning;
  begin
  if issuedWarning then exit;
  ShowMessage('Could not write to the log file ' + domain.options.logFileName + char(13) + char(10) +
  	'The file may be write protected or in use by another program, or your disk may be full.' + char(13) + char(10) +
    'Your changes will not be logged.');
  issuedWarning := true;
  end;

procedure TChangeLogForm.ensureLogFileName;
  begin
  if domain.options.logFileName = '' then
    domain.options.logFileName := ExtractFilePath(Application.exeName) + kDefaultLogFileName;
  end;

procedure TChangeLogForm.addToLog(change: string);
	var
  	LogFile: TextFile;
	begin
  if Trim(change) = '' then exit;
  self.ensureLogFileName;
  AssignFile(LogFile, domain.options.logFileName);
  try
  	try
			if FileExists(domain.options.logFileName) then
				Append(LogFile)
  		else
    		Rewrite(LogFile);
  	except
  		self.fileErrorWarning;
  		exit;
  	end;
  	writeln(LogFile, change);
  	Flush(LogFile);
  finally
  	try
			CloseFile(LogFile);
  	except
  		if not issuedWarning then
  			ShowMessage('Problem closing log file ' + domain.options.logFileName);
  	end;
  end;
  end;

procedure TChangeLogForm.clearLog;
	var
  	LogFile: TextFile;
	begin
  self.ensureLogFileName;
  AssignFile(LogFile, domain.options.logFileName);
  try
  	try
      Rewrite(LogFile);
  	except
  		self.fileErrorWarning;
  		exit;
  	end;
  finally
  	try
			CloseFile(LogFile);
  	except
  		if not issuedWarning then
  			ShowMessage('Problem closing log file ' + domain.options.logFileName);
  	end;
  end;
  self.loadChangeLog;
  end;

procedure TChangeLogForm.CopySelectedTextButtonClick(Sender: TObject);
	begin
	LogContentsRichEdit.CopyToClipboard;
  {self.modalResult := 1;   }
	end;

procedure TChangeLogForm.OKButtonClick(Sender: TObject);
	begin
  {self.modalResult := 1;}
  self.close;
	end;

procedure TChangeLogForm.loadChangeLog;
	begin
  self.ensureLogFileName;
  if not FileExists(domain.options.logFileName) then exit;
  LogContentsRichEdit.Text := '';
  LogContentsRichEdit.lines.loadFromFile(domain.options.logFileName);
  self.caption := 'Log file ' + ExtractFileName(domain.options.logFileName);
	end;

procedure TChangeLogForm.scrollLogEndIntoView;
  begin
  LogContentsRichEdit.SelStart := Length(LogContentsRichEdit.Text);
  LogContentsRichEdit.SelLength := 0;
  LogContentsRichEdit.Perform(EM_SCROLLCARET, 0, 0);
  end;

procedure TChangeLogForm.UpdateButtonClick(Sender: TObject);
  begin
  self.loadChangeLog;
  self.scrollLogEndIntoView;
  end;

procedure TChangeLogForm.bottomPanelResize(Sender: TObject);
	begin
  copySelectedTextButton.left := 4;
  helpButton.left := bottomPanel.clientWidth - helpButton.width - 4;
  changeLogFile.left := helpButton.left - changeLogFile.width - 4;
  clearLogFile.left := changeLogFile.left - clearLogFile.width - 4;
  updateButton.left := clearLogFile.left - updateButton.width - 4;
	end;

procedure makeBackupCopy(fileFrom, fileTo: string);
  var
    fileFromCString: array[0..255] of char;
    fileToCString: array[0..255] of char;
  begin
  strPCopy(fileFromCString, fileFrom);
  strPCopy(fileToCString, fileTo);
  copyFile(fileFromCString, fileToCString, false {overwrite existing file});
  end;

procedure TChangeLogForm.clearLogFileClick(Sender: TObject);
  var
    backupLogFileName: string;
	begin
  if MessageDlg('Are you sure you want to clear the log file?' + chr(13)
    + 'This is not undoable, but a backup file will be made.',
      mtConfirmation, [mbYes, mbNo], 0) = idNo then exit;
  backupLogFileName := changeFileExt(domain.options.logFileName, '.~lo');
  makeBackupCopy(domain.options.logFileName, backupLogFileName);
  self.clearLog;
	end;

procedure TChangeLogForm.changeLogFileClick(Sender: TObject);
  var
    backupLogFileName: string;
	begin
  with openDialog do
    begin
    title := 'Choose or type in a new log file name';
    fileName := domain.options.logFileName;
    filter := 'Log files (*.log)|*.log|All files (*.*)|*.*';
    defaultExt := 'log';
   end;
  if openDialog.execute then
    begin
  	backupLogFileName := changeFileExt(domain.options.logFileName, '.~lo');
  	makeBackupCopy(domain.options.logFileName, backupLogFileName);
    domain.options.logFileName := openDialog.fileName;
    if not fileExists(domain.options.logFileName) then
      self.clearLog;
    self.loadChangeLog;
    self.scrollLogEndIntoView;
    end;
	end;

procedure TChangeLogForm.WMGetMinMaxInfo(var MSG: Tmessage);
  begin
  inherited;
  with PMinMaxInfo(MSG.lparam)^ do
    begin
    ptMinTrackSize.x := 316;
    ptMinTrackSize.y := 100;
    end;
  end;

procedure TChangeLogForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('Using_the_Change_Log_file_to_recover_text'); 
  end;

procedure TChangeLogForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
	begin
  if key = VK_F2 then
    self.showTechSupportInfoInChangeLog;
	end;

procedure TChangeLogForm.showTechSupportInfoInChangeLog;
  var
    numRules, numVariables: longint;
    memoryStatus: TMemoryStatus;
    diskSpace: longint;
    screenColorBits: integer;
    screenColors: longint;                          
    screenDC: HDC;
  begin
  LogContentsRichEdit.lines.add('');
  LogContentsRichEdit.lines.add('StoryHarp Technical Support Information');
  LogContentsRichEdit.lines.add('');

  numRules := domain.world.rules.count;
  LogContentsRichEdit.lines.add('Rules: ' + intToStr(numRules));

  numVariables := domain.world.variables.count;
  LogContentsRichEdit.lines.add('Variables: ' + intToStr(numVariables));

  // more here...

  LogContentsRichEdit.lines.add('');
  screenDC := GetDC(0);
  try
    screenColorBits := (GetDeviceCaps(screenDC, BITSPIXEL) * GetDeviceCaps(screenDC, PLANES));
  finally
    ReleaseDC(0, screenDC);
  end;
  if screenColorBits <> 32 then
    screenColors := 1 shl screenColorBits
  else
    screenColors := round(power(2.0, screenColorBits));
  LogContentsRichEdit.lines.add('Colors: ' + intToStr(screenColors) + ' (' + intToStr(screenColorBits) + ' bits)');
  LogContentsRichEdit.lines.add('Size: ' + intToStr(Screen.width) + ' x ' + intToStr(Screen.height));
  LogContentsRichEdit.lines.add('Resolution: ' + intToStr(Screen.pixelsPerInch) + ' pixels/inch');

  LogContentsRichEdit.lines.add('');
  self.GetOSInfo;
  memoryStatus.dwLength := sizeOf(memoryStatus);
  GlobalMemoryStatus(memoryStatus);
  LogContentsRichEdit.lines.add('Percent memory in use: ' + intToStr(memoryStatus.dwMemoryLoad));
  LogContentsRichEdit.lines.add('Total physical memory: ' + intToStr(memoryStatus.dwTotalPhys div 1024) + ' K');
  LogContentsRichEdit.lines.add('Available physical memory: ' + intToStr(memoryStatus.dwAvailPhys div 1024) + ' K');
  LogContentsRichEdit.lines.add('Total paging file: ' + intToStr(memoryStatus.dwTotalPageFile div 1024) + ' K');
  LogContentsRichEdit.lines.add('Available paging file: ' + intToStr(memoryStatus.dwAvailPageFile div 1024) + ' K');
  LogContentsRichEdit.lines.add('Total user memory: ' + intToStr(memoryStatus.dwTotalVirtual div 1024) + ' K');
  LogContentsRichEdit.lines.add('Available user memory: ' + intToStr(memoryStatus.dwAvailVirtual div 1024) + ' K');
  
  diskSpace := DiskFree(0);
  LogContentsRichEdit.lines.add('Disk space on current drive: ' + intToStr(diskSpace div (1024 * 1024)) + ' MB');
  end;

procedure TChangeLogForm.GetOSInfo;
var
  Platform: string;
  BuildNumber: Integer;
begin
  case Win32Platform of
    VER_PLATFORM_WIN32_WINDOWS:
      begin
        Platform := 'Windows 95';
        BuildNumber := Win32BuildNumber and $0000FFFF;
      end;
    VER_PLATFORM_WIN32_NT:
      begin
        Platform := 'Windows NT';
        BuildNumber := Win32BuildNumber;
      end;
      else
      begin
        Platform := 'Windows';
        BuildNumber := 0;
      end;
  end;
  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) or
    (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    if Win32CSDVersion = '' then
      LogContentsRichEdit.lines.add(Format('%s %d.%d (Build %d)', [Platform, Win32MajorVersion,
        Win32MinorVersion, BuildNumber]))
    else
      LogContentsRichEdit.lines.add(Format('%s %d.%d (Build %d: %s)', [Platform, Win32MajorVersion,
        Win32MinorVersion, BuildNumber, Win32CSDVersion]));
  end
  else
    LogContentsRichEdit.lines.add(Format('%s %d.%d', [Platform, Win32MajorVersion,
      Win32MinorVersion]));
end;

end.
