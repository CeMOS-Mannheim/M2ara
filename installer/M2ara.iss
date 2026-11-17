; ---------------------------------------------------------------------------
;  Inno Setup script for the M2ara stand-alone Windows installer.
;
;  It is compiled by .github/workflows/build-installer.yml with:
;
;    ISCC.exe /DMyAppVersion=<version> ^
;              /DPayloadDir=<payload> ^
;              /DOutputDir=<output directory> ^
;              [/DMyIcon=<path to M2ara.ico>] ^
;              installer\M2ara.iss
;
;  If you still have the AppId of the previously published installer (1.4.2),
;  put it into AppId below. Keeping it means an upgrade replaces the old
;  installation instead of adding a second entry under "Apps & features".
; ---------------------------------------------------------------------------

#define MyAppName "M2ara"
#define MyAppExeName "M2ara.bat"
#define MyAppPublisher "CeMOS, Hochschule Mannheim"
#define MyAppURL "https://github.com/CeMOS-Mannheim/M2ara"

#ifndef MyAppVersion
  #define MyAppVersion "0.0.0"
#endif

#ifndef PayloadDir
  #define PayloadDir "..\payload"
#endif

#ifndef OutputDir
  #define OutputDir "."
#endif

#ifndef MyIcon
  #define MyIcon ""
#endif

; Numeric version for the Windows file version resource (a.b.c.d). Passed
; separately by the workflow, because MyAppVersion may contain a suffix such as
; "-rc1", which the file version resource does not accept.
#ifndef MyAppVersionInfo
  #define MyAppVersionInfo MyAppVersion
#endif

[Setup]
; NOTE: {#...} is an Inno Setup Preprocessor directive, {{ and }} escape braces.
AppId={{8B3D2A31-6F4C-4E59-9E2B-1D7C5A0F9B42}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}/issues
AppUpdatesURL={#MyAppURL}/releases
VersionInfoVersion={#MyAppVersionInfo}
VersionInfoCompany={#MyAppPublisher}
VersionInfoDescription={#MyAppName} {#MyAppVersion} Setup
VersionInfoProductName={#MyAppName}
VersionInfoProductVersion={#MyAppVersionInfo}
DefaultDirName={localappdata}\{#MyAppName}
DefaultGroupName={#MyAppName}
DisableDirPage=yes
DisableProgramGroupPage=yes
DisableWelcomePage=no
AllowNoIcons=yes
PrivilegesRequired=lowest
MinVersion=10.0
OutputDir={#OutputDir}
OutputBaseFilename={#MyAppName}_{#MyAppVersion}
UninstallDisplayName={#MyAppName} {#MyAppVersion}
#if MyIcon != ""
UninstallDisplayIcon={app}\M2ara.ico
#else
UninstallDisplayIcon={app}\{#MyAppExeName}
#endif
Compression=lzma2/max
SolidCompression=yes
WizardStyle=modern
ShowLanguageDialog=auto
#if MyIcon != ""
SetupIconFile={#MyIcon}
#endif

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "{#PayloadDir}\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
#if MyIcon != ""
Source: "{#MyIcon}"; DestDir: "{app}"; DestName: "M2ara.ico"; Flags: ignoreversion
#endif

[Icons]
#if MyIcon != ""
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\M2ara.ico"; Comment: "M2ara {#MyAppVersion}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\M2ara.ico"; Comment: "M2ara {#MyAppVersion}"; Tasks: desktopicon
#else
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Comment: "M2ara {#MyAppVersion}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Comment: "M2ara {#MyAppVersion}"; Tasks: desktopicon
#endif

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,M2ara}"; Flags: shellexec postinstall nowait skipifsilent

[UninstallDelete]
; written by the app itself ("Save settings" in the Settings tab)
Type: files; Name: "{app}\settings.csv"
; profile of the bundled browser (created on first start)
Type: filesandordirs; Name: "{app}\browser-profile"
