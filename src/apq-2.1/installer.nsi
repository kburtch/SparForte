;NSIS Script For APQ-2.1 Binary Release
; $Id: installer.nsi,v 1.2 2005/02/11 02:59:44 ken Exp $
; Warren W. Gay VE3WWG

;Title Of Your Application
Name "APQ-${VERS} Binary Release"

;Do A CRC Check
CRCCheck On

;Output File Name
OutFile "installer.exe"

ComponentText "APQ Components?"
UninstallButtonText "Uninstall APQ"

InstallDir "$PROGRAMFILES\APQ-${VERS}"

InstallDirRegKey HKEY_LOCAL_MACHINE "SOFTWARE\Ada Core Technologies\GNAT" root

DirText "APQ is best installed where GNAT is installed:"

LicenseData win32_copying.rtf
LicenseText "APQ-${VERS} License Terms"

;
; Test to see if we have GNAT installed :
;
Function .onGUIInit

  StrCpy $R0 ""
  ReadRegStr $R0 HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\APQ" UninstallString

  StrCmp "$R0" "" No_APQ_Installed

  MessageBox MB_OKCANCEL "APQ appears to be installed already. Proceed? ($R0)" IDOK No_APQ_Installed
  MessageBox MB_OK "FYI: You can uninstall APQ from the control panel."
  goto I_Quit

No_APQ_Installed:

  StrCmp "$PROGRAMFILES\APQ-${VERS}" "$INSTDIR" No_GNAT Got_GNAT

No_GNAT:
  MessageBox MB_OKCANCEL "You do not appear to have GNAT installed. Do you want to proceed?" IDOK Got_GNAT

I_Quit:
  MessageBox MB_OK "Installation cancelled."
  Abort "Installation Cancelled."

Got_GNAT:

FunctionEnd

Function .onInstSuccess

  MessageBox MB_OK "Try the test program $INSTDIR\Bindings\APQ\win32_test.adb"

FunctionEnd


  !define SRC    "C:\OPT\CYGWIN\home\wgay\cvs\ada\apq"
  !define WINSRC "${SRC}\win32-${VERS}"

Section "APQ Core Library Support"
  ;Install Files

  SetOutPath $INSTDIR\Bindings\APQ
  SetCompress Auto
  SetOverwrite IfNewer

  File /a ${WINSRC}\adainclude\apq.ads
  File /a ${WINSRC}\adainclude\apq.adb

  File /a ${WINSRC}\adalib\libapq.a
  File /a ${WINSRC}\adalib\apq.ali

  ; Add APQ to libraries searched by GNAT
  WriteRegStr HKLM "Software\Ada Core Technologies\GNAT\Standard Libraries" \
  "APQ" "$INSTDIR\Bindings\APQ"

SectionEnd

Section "APQ PostgreSQL Support"

  SetOutPath $INSTDIR\Bindings\APQ
  SetCompress Auto
  SetOverwrite IfNewer

  File /a ${WINSRC}\adainclude\apq-postgresql.ads
  File /a ${WINSRC}\adainclude\apq-postgresql-client.adb
  File /a ${WINSRC}\adainclude\apq-postgresql-client.ads
  File /a ${WINSRC}\adainclude\apq-postgresql-decimal.ads
  File /a ${WINSRC}\adainclude\apq-postgresql-decimal.adb

  File /a ${WINSRC}\adalib\apq-postgresql.ali
  File /a ${WINSRC}\adalib\apq-postgresql-client.ali
  File /a ${WINSRC}\adalib\apq-postgresql-decimal.ali

SectionEnd

Section "APQ MySQL Support"

  SetOutPath $INSTDIR\Bindings\APQ
  SetCompress Auto
  SetOverwrite IfNewer

  File /a ${WINSRC}\adainclude\apq-mysql.ads
  File /a ${WINSRC}\adainclude\apq-mysql-client.adb
  File /a ${WINSRC}\adainclude\apq-mysql-client.ads

  File /a ${WINSRC}\adalib\apq-mysql.ali
  File /a ${WINSRC}\adalib\apq-mysql-client.ali

  SetOutPath $INSTDIR\bin
  File /a ${WINSRC}\bin\apq_myadapter.dll

SectionEnd

Section "PostgreSQL's libpq.dll file"

  SetOutPath $INSTDIR\bin
  File /a ${WINSRC}\bin\libpq.dll

SectionEnd

Section "MySQL's libmySQL.dll file"

  SetOutPath $INSTDIR\bin
  File /a ${WINSRC}\bin\libmysql.dll

SectionEnd

Section "APQ Manual"

  SetOutPath $INSTDIR\doc
  File "${WINSRC}\APQ-Manual.pdf"
  File "${WINSRC}\APQ_ACL_LICENSE.txt"
  File "${WINSRC}\APQ_GPL_LICENSE.txt"
  File "${WINSRC}\APQ_LICENSE.rtf"
  File "${WINSRC}\APQ_PG_COPYRIGHT.txt"

;  CreateShortCut is not working yet..
;  CreateShortCut "$DESKTOP/APQ Manual.PDF" $INSTDIR/doc/apq-manual.pdf

SectionEnd

Section "APQ Win32_Test Test Program"

  SetOutPath $INSTDIR\Bindings\APQ
  File ${WINSRC}\win32_test.adb

  SetFileAttributes $INSTDIR\Bindings\APQ\win32_test.adb READONLY

SectionEnd

Section "APQ Uninstall Support"

  WriteRegStr HKEY_LOCAL_MACHINE Software\APQ Version "2.1"

  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\APQ" \
	  DisplayName "APQ-${VERS} (remove only)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\APQ" \
	  UninstallString "$INSTDIR\Uninstall_APQ.exe"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\APQ" \
	  URLInfoAbout "http://home.cogeco.ca/~ve3wwg"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\APQ" \
	  Publisher "Warren W. Gay VE3WWG"
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\APQ" \
	  NoModify 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\APQ" \
	  NoRepair 1

  WriteUninstaller "Uninstall_APQ.exe"

SectionEnd

UninstallText "This will uninstall the APQ-${VERS} Ada database binding for GNAT"

Section Uninstall

  ;Delete Files
  Delete $INSTDIR\Bindings\APQ\apq-postgresql-decimal.ads
  Delete $INSTDIR\Bindings\APQ\apq.ads
  Delete $INSTDIR\Bindings\APQ\apq-mysql.ads
  Delete $INSTDIR\Bindings\APQ\apq-mysql-client.adb
  Delete $INSTDIR\Bindings\APQ\apq-mysql-client.ads
  Delete $INSTDIR\Bindings\APQ\apq-postgresql.ads
  Delete $INSTDIR\Bindings\APQ\apq-postgresql-client.adb
  Delete $INSTDIR\Bindings\APQ\apq-postgresql-client.ads
  Delete $INSTDIR\Bindings\APQ\apq-postgresql-decimal.adb
  Delete $INSTDIR\Bindings\APQ\apq.adb

  Delete $INSTDIR\Bindings\APQ\win32_test.adb

  Delete $INSTDIR\Bindings\APQ\libapq.a
  Delete $INSTDIR\Bindings\APQ\apq-mysql.ali
  Delete $INSTDIR\Bindings\APQ\apq-mysql-client.ali
  Delete $INSTDIR\Bindings\APQ\apq-postgresql.ali
  Delete $INSTDIR\Bindings\APQ\apq-postgresql-client.ali
  Delete $INSTDIR\Bindings\APQ\apq-postgresql-decimal.ali
  Delete $INSTDIR\Bindings\APQ\apq.ali

  Delete $INSTDIR\bin\libpq.dll
  Delete $INSTDIR\bin\libmysql.dll
  Delete $INSTDIR\bin\apq_myadapter.dll

  Delete "$DESKTOP\APQ Manual.pdf"
  Delete $INSTDIR\doc\apq-manual.pdf
  Delete $INSTDIR\APQ_ACL_LICENSE.txt
  Delete $INSTDIR\APQ_GPL_LICENSE.txt
  Delete $INSTDIR\APQ_COPYING.txt
  Delete $INSTDIR\APQ_PG_COPYRIGHT.txt

  ;Delete Uninstaller And Unistall Registry Entries

  Delete $INSTDIR\Uninstall_APQ.exe

  ; Remove registry entries

  DeleteRegKey HKEY_LOCAL_MACHINE "Software\APQ" 

  DeleteRegKey HKEY_LOCAL_MACHINE "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\APQ"
  DeleteRegKey HKEY_LOCAL_MACHINE "SOFTWARE\Ada Core Technologies\GNAT\Standard Libraries\APQ"

  ; Remove the bindings subdirectory

  RMDir $INSTDIR\Bindings\APQ

SectionEnd

; $Source: /home/cvsroot/bush/src/apq-2.1/installer.nsi,v $
