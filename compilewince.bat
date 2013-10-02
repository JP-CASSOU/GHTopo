PATH=C:\Lazarus\fpc\2.2.4\bin\i386-win32;%PATH%
echo Compilation de %1
ppcrossarm.exe %1 -Mdelphi
pause