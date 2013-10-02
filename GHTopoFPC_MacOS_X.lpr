program GHTopoFPC_MacOS_X;

{$mode DELPHI}{$H+}

uses
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
      cthreads,
    {$ENDIF}
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, BarreMenuPpale, ConversionsCoordonneesESibert,
  ConvertisseurJPC, JPC_Ellipsoide;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBarreMenuPpal, BarreMenuPpal);
  Application.Run;
end.

