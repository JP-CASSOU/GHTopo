// Projet d'exercice de développement PocketPC en Free Pascal:
// un mini-logiciel de topographie compatible Visual Topo
// But de l'exercice:
// - Apprentissage du développement WinCE
// - Proposer une application terrain simple et portable
// (contrairement à Pocket Topo, qui nécessite .NET)
program project1;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  //Windows,
  Forms, Unit1, LResources, StructuresDonnees, Common, UnitEntites,
CallDialogs, dlgSelectElementsDrawn, dlgColorDialog, CdrExpes, CdrCodes,
CdrSerie, CdrDessin, dlgEntry, dlgChooseReseau, dlgToporobotColor,
dlgExportVTopo, CdrNavigDB;

{$IFDEF WINDOWS}{$R project1.rc}{$ENDIF}

begin
  {$I project1.lrs}

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  //Application.CreateForm(TDialogVTopo, DialogVTopo);
  //Application.CreateForm(TDialogSelTopoColor, DialogSelTopoColor);
  //Application.CreateForm(TDialogChooseReseau, DialogChooseReseau);
  //Application.CreateForm(TDialogEntry, DialogEntry);
  //Application.CreateForm(TfrmColorDialog, frmColorDialog);
  //Application.CreateForm(TfrmSelectElementsDrawn, frmSelectElementsDrawn);
  Application.Run;
end.

