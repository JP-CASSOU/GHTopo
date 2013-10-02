program GHTopoFPC;

{$mode DELPHI}{$H+}

uses
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
      cthreads,
    {$ENDIF}
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  BarreMenuPpale, frmImportClipboardToSerie;
  //ConversionsCoordonneesESibert,
  //ConvertisseurJPC, JPC_Ellipsoide, dlgHelpSystem, dlgProjectManagerWideScreen;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBarreMenuPpal, BarreMenuPpal);
  Application.CreateForm(TdlgImportClipboardToSerie, dlgImportClipboardToSerie);
  //Application.CreateForm(TfrmHelpSystem, frmHelpSystem);
  Application.Run;
end.

