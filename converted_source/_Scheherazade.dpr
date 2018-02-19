program Scheherazade;

uses
  Forms,
  SysUtils,
  USConsoleForm in 'USConsoleForm.pas' {ConsoleForm},
  Ucommand in 'Ucommand.pas',
  ucursor in 'Ucursor.pas',
  UFileSupport in 'UFileSupport.pas',
  USMapView in 'USMapView.pas',
  USDomain in 'USDomain.pas',
  USRuleEditorForm in 'USRuleEditorForm.pas' {RuleEditorForm},
  USSpeech in 'USSpeech.pas',
  USWorld in 'USWorld.pas',
  USCommands in 'USCommands.pas',
  USCommandWizard in 'USCommandWizard.pas' {CommandWizardForm},
  USFocusCommands in 'USFocusCommands.pas',
  USModelChanges in 'USModelChanges.pas',
  USVariableCommands in 'USVariableCommands.pas',
  USContextWizard in 'USContextWizard.pas' {ContextWizardForm},
  USLinkWizard in 'USLinkWizard.pas' {LinkWizardForm},
  USChangeLog in 'USChangeLog.pas' {ChangeLogForm},
  USAbout in 'USAbout.pas' {AboutForm},
  USPreferences in 'USPreferences.pas' {PreferencesForm},
  Usstream in 'Usstream.pas',
  USMediaDirForm in 'USMediaDirForm.pas' {ExtraMediaDirectoryForm},
  uabout in 'uabout.pas' {UnregisteredAboutForm},
  uregister in 'uregister.pas' {RegistrationForm},
  URegisterSupport in 'URegisterSupport.pas',
  USAgentWarning in 'USAgentWarning.pas' {AgentWarningForm},
  USPictureForm in 'USPictureForm.pas' {PictureForm},
  USJavaWriter in 'USJavaWriter.pas';

{$R *.RES}

begin
  Application.Initialize;

  Application.Title := 'StoryHarp';
  Application.HelpFile := 'StoryHarp.hlp';
  Application.CreateForm(TConsoleForm, ConsoleForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TPictureForm, PictureForm);
  AboutForm.setUpAsSplashOrAbout(kAsSplash);
  AboutForm.show;
  AboutForm.refresh;

  Application.CreateForm(TRuleEditorForm, RuleEditorForm);
  Application.CreateForm(TCommandWizardForm, CommandWizardForm);
  Application.CreateForm(TContextWizardForm, ContextWizardForm);
  Application.CreateForm(TLinkWizardForm, LinkWizardForm);
  Application.CreateForm(TChangeLogForm, ChangeLogForm);
  Application.CreateForm(TPreferencesForm, PreferencesForm);
  Application.CreateForm(TExtraMediaDirectoryForm, ExtraMediaDirectoryForm);
  Application.CreateForm(TUnregisteredAboutForm, UnregisteredAboutForm);
  Application.CreateForm(TRegistrationForm, RegistrationForm);
  domain.loadFileAtStartupAndInitializeForms;

  AboutForm.hide;
  AboutForm.setUpAsSplashOrAbout(kAsAbout);

  Application.Run;
end.
