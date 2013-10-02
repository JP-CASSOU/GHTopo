program GHTopoFPC;

{$mode DELPHI}{$H+}

uses
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
      cthreads,
    {$ENDIF}
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, BarreMenuPpale;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBarreMenuPpal, BarreMenuPpal);
  Application.Run;
end.
