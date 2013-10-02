unit CallDialogs;

{$mode delphi}{$H+}

interface

uses
  Graphics,
  StructuresDonnees,
  ToporobotClasses2012,
  //ToporobotClasses,
  Forms, Controls, StdCtrls, Dialogs,
  Classes, SysUtils;

  function QuestionOuiNon(const Msg: string): boolean;
  function ChooseColor(const OldColor: TColor): TColor;
  function ChooseToporobotColor(const IdxColor: integer): integer;

  function ChooseReseau(const FDocTopo: TToporobotStructure2012; const IdxReseau: integer): integer;

  procedure DialogEntry(var QEditBox: TEdit; const QCaption: string; const QMode: integer;
                        const QMin, QMax: string);
  // export vers visual topo
  procedure ExporterVisualTopo(const DocuTopo: TToporobotStructure2012; const FichierVTopo: string);



implementation
uses
  dlgColorDialog,
  dlgExportVTopo,         // pour export vers Visual Topo
  dlgToporobotColor,      // pour sélection de couleurs Toporobot
  dlgChooseReseau,        // dialogue de choix du réseau
  dlgEntry,               // dialogue à gros boutons pour entrée de données
  dlgSelectElementsDrawn; // pour le dialogue de sélection
// Question Oui-Non
function QuestionOuiNon(const Msg: string): boolean;
begin
  Result := (MessageDlg(Msg, mtConfirmation,[mbYES, mbNO], 0) = mrYES);
end;
// Choisir un réseau
function ChooseReseau(const FDocTopo: TToporobotStructure2012; const IdxReseau: integer): integer;
begin
  Result := IdxReseau;
  with TDialogChooseReseau.Create(Application) do
  begin
    try
      SetCurrentIdx(IdxReseau);
      SetDocTopo(FDocTopo);
      ShowModal;
      if ModalResult = mrOK then Result := GetCurrentIdx;
    finally
      Release;
    end;
  end;
end;

function ChooseColor(const OldColor: TColor): TColor;
begin
  Result := OldColor;
  // Windows CE
  with TfrmColorDialog.Create(Application) do begin
    try
      SetColor(Result);
      ShowModal;
      if ModalResult = mrOK then begin
        Result := GetColor;

      end;

    finally
      Release;
    end;
  end;
end;
function ChooseToporobotColor(const IdxColor: integer): integer;
begin
  Result := IdxColor;
  with TDialogSelTopoColor.Create(Application) do
  begin
    try
      Initialize(IdxColor);
      ShowModal;

      if (ModalResult = mrOK) then Result := GetIdxColor();
      Finalize;
    finally
      Release;
    end;
  end;
end;

// dialogue pour faciliter l'entrée de valeur
// QMode: 0 = Texte; 1 = Entier; 2 = Réel
procedure DialogEntry(var QEditBox: TEdit; const QCaption: string; const QMode: integer;
                      const QMin, QMax: string);
var
  S: string;
begin
  // On demande du texte ? Utilisation de InputQuery
  if QMode = 0 then begin
    S := QEditBox.Text;

    if InputQuery(QCaption, 'Texte', S) then
      QEditBox.Text := Trim(S);
    Exit;
  end;
  // sinon, on affiche le clavier à grosses touches
  with TDialogEntry.Create(Application) do begin
    try
       QSetModeInput(QMode);
       QSetBounds(QMin, QMax);
       QSetValue(QEditBox.Text);
       QSetCaption(QCaption);
       InitClavier(QMode);
       ShowModal;
       if ModalResult = mrOK then begin

         QEditBox.Text := QGetValue;

       end;

    finally
       Free;
    end;
  end;

end;

// export vers visual topo
procedure ExporterVisualTopo(const DocuTopo: TToporobotStructure2012; const FichierVTopo: string);
begin
  with TDialogVTopo.Create(Application) do
  begin
    try
      if Initialize(DocuTopo, FichierVTopo) then begin
        ShowModal;

      end;
    finally
      Release;
    end;
  end;

end;

end.

