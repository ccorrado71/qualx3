#define MyDateTimeString GetDateTimeString('ddmmyy', '', '');
[Setup]
AppName=Expo2
AppVerName=Expo version 2.0.0
AppVersion=2.0
VersionInfoVersion=2.0.0
AppPublisher=IC-Bari-Italy
AppPublisherURL=http://www.ic.cnr.it
AppSupportURL=http://www.ic.cnr.it
AppUpdatesURL=http://www.ic.cnr.it
DefaultDirName={commonpf}\Expo2
DefaultGroupName=Expo2
LicenseFile=licence.txt
OutputDir=.\setup_dir
OutputBaseFilename=expo-{#MyDateTimeString}-x64_install
SetupIconFile=expo2006_setup.ico
WizardImageFile=expoImage.bmp
WizardSmallImageFile=expoSmallimage.bmp
Compression=lzma
SolidCompression=yes
UninstallDisplayIcon=expo2006_uninstall.ico
UninstallDisplayName=Expo2 Version 2.0.0
DirExistsWarning=yes
ChangesAssociations=yes
ChangesEnvironment=yes
PrivilegesRequired=admin
;LanguageDetectionMethod=uilanguage
; "ArchitecturesAllowed=x64" specifies that Setup cannot run on
; anything but x64.
ArchitecturesAllowed=x64
; "ArchitecturesInstallIn64BitMode=x64" requests that the install be
; done in "64-bit mode" on x64, meaning it should use the native
; 64-bit Program Files directory and the 64-bit view of the registry.
ArchitecturesInstallIn64BitMode=x64

;[Languages]
;Name: "en"; MessagesFile: "compiler:Default.isl"
;Name: "it"; MessagesFile: "compiler:Languages\Italian.isl"

[LangOptions]
DialogFontName=Microsoft Sans Serif
DialogFontSize=8
WelcomeFontName=Microsoft Sans Serif
WelcomeFontSize=14

[Messages]
WelcomeLabel1=%n%nWelcome to the [name] Setup
WelcomeLabel2=This will install [name/ver] on your computer.%n%nIt is recommended that you close all other applications before continuing.

[CustomMessages]
MyDescription=Create a &desktop icon for

[Tasks]
Name: "desktopicon"; Description: "{cm:MyDescription} expo"; GroupDescription: "{cm:AdditionalIcons}"

[Files]
Source: ".\Files\*"; DestDir: "{app}\bin"; Flags: recursesubdirs ignoreversion
Source: "C:\Program Files (x86)\Intel\oneAPI\compiler\2023.0.0\windows\redist\intel64_win\compiler\*.dll";  DestDir: "{app}\bin"; Flags: ignoreversion
Source: "..\..\build_ifort_release\expo.exe"; DestDir: "{app}\bin"; Flags: recursesubdirs ignoreversion
Source: "..\..\share\*"; DestDir: "{app}\share"; Flags: recursesubdirs ignoreversion
Source: "..\..\indexing\*"; DestDir: "{app}\bin"; Flags: recursesubdirs ignoreversion
;Source: "..\dicvol06.exe"; DestDir: "{app}\bin"; Flags: ignoreversion
;Source: "..\McMaille.exe"; DestDir: "{app}\bin"; Flags: ignoreversion
;Source: "..\*.hkl"; DestDir: "{app}\bin"; Flags: ignoreversion
;Source: "..\expo2011Readme.txt"; DestDir: "{app}\share"; Flags: ignoreversion


;FIX: add other files
Source: "..\..\src\*.f"; DestDir: "{app}\src"; Flags: ignoreversion
Source: "..\..\src\*.f90"; DestDir: "{app}\src"; Flags: ignoreversion
Source: "..\..\src\*.cpp"; DestDir: "{app}\src"; Flags: ignoreversion
Source: "..\..\src\*.h"; DestDir: "{app}\src"; Flags: ignoreversion

[Run]
;Filename: "{app}\vc_redist.x64.exe"; Parameters: "/q /norestart"; \
;    Check: VC2017RedistNeedsInstall; StatusMsg: "Installing VC++ redistributables..."

;[Code]
;function VC2017RedistNeedsInstall: Boolean;
;var 
;  Version: String;
;begin
;  if (RegQueryStringValue(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\VisualStudio\14.0\VC\Runtimes\x64', 'Version', Version)) then
;  begin
;    // Is the installed version at least 14.14 ? 
;    Log('VC Redist Version check : found ' + Version);
;    Result := (CompareStr(Version, 'v14.14.26429.03')<0);
;  end
;  else 
;  begin
;    // Not even an old version installed
;    Result := True;
;  end;
;  if (Result) then
;  begin
;    ExtractTemporaryFile('VC_redist.x64.exe');
;  end;
;end;

[CustomMessages]
expoHelp=Expo2 Help
expoReadMe=Expo2 ReadMe
expoManual=Expo2 Manual

[Code]
procedure URLLabelOnClick(Sender: TObject);
var
  Dummy: Integer;
begin
  ShellExec('open', 'http://www.ic.cnr.it', '', '', SW_SHOWNORMAL, ewNoWait, Dummy);
end;

procedure InitializeWizard();
var
  URLLabel: TNewStaticText;
  BackgroundBitmapImage: TBitmapImage;
  BackgroundBitmapText: TNewStaticText;
begin
  URLLabel := TNewStaticText.Create(WizardForm);
  URLLabel.Top := WizardForm.Height-70;
  URLLabel.Left := 30;
  URLLabel.Caption := 'www.ic.cnr.it';
  URLLabel.Font.Style := URLLabel.Font.Style + [fsUnderLine];
  URLLabel.Font.Color := clBlue;
  URLLabel.Cursor := crHand;
  URLLabel.OnClick := @URLLabelOnClick;
  URLLabel.Parent := WizardForm;

  BackgroundBitmapImage := TBitmapImage.Create(MainForm);
  BackgroundBitmapImage.AutoSize := True;
  BackgroundBitmapImage.Bitmap := WizardForm.WizardBitmapImage.Bitmap;
  BackgroundBitmapImage.Left := 50;
  BackgroundBitmapImage.Top := 100;
  BackgroundBitmapImage.Parent := MainForm;

  BackgroundBitmapText := TNewStaticText.Create(MainForm);
  BackgroundBitmapText.Caption := 'Expo2 2.0.0';
  BackgroundBitmapText.Left := BackGroundBitmapImage.Left;
  BackgroundBitmapText.Top := BackGroundBitmapImage.Top + BackGroundBitmapImage.Height + 8;
  BackgroundBitmapText.Parent := MainForm;
end;

[Icons]
Name: "{group}\Expo2"; Filename: "{app}\bin\expo.exe"; WorkingDir: "{app}"
Name: "{group}\{cm:expoHelp}"; Filename: "{app}\share\expo\expo_help\expo2011.html"
Name: "{group}\{cm:expoManual}"; Filename: "{app}\share\expo\expo_help\expo_help.pdf"
Name: "{userdesktop}\Expo2"; Filename: "{app}\bin\expo.exe" ; Tasks: desktopicon ; WorkingDir: "{app}"
Name: "{group}\{cm:UninstallProgram,Expo2}";  Filename: "{uninstallexe}"