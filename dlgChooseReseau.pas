unit dlgChooseReseau;

{$mode DELPHI}{$H+}

interface

uses
  Common, StructuresDonnees, ToporobotClasses2012, Classes, SysUtils, FileUtil,
  LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TDialogChooseReseau }

  TDialogChooseReseau = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    lbNbReseaux: TLabel;
    lsbReseaux: TListBox;
    procedure BitBtn1Click(Sender: TObject);
    procedure lsbReseauxClick(Sender: TObject);
  private
    { private declarations }
    FIdxReseau: Integer;

    FDocuTopo: TToporobotStructure2012;
    procedure  ListerReseaux;
  public
    { public declarations }
    function  GetCurrentIdx: integer;
    procedure SetCurrentIdx(const Idx: integer);
    function SetDocTopo(const FD: TToporobotStructure2012): Boolean;
  end;

var
  DialogChooseReseau: TDialogChooseReseau;

implementation
function TDialogChooseReseau.SetDocTopo(const FD: TToporobotStructure2012): Boolean;
begin
  Result := False;
  try
    FDocuTopo := FD;
    // lister les réseaux
    ListerReseaux;
    Result := True;
  except
  end;
end;
function  TDialogChooseReseau.GetCurrentIdx: integer;
begin
  Result := lsbReseaux.ItemIndex;
  if (Result<0) then Result := 0;
end;

procedure TDialogChooseReseau.SetCurrentIdx(const Idx: integer);
begin
  FIdxReseau := Idx;
end;

procedure TDialogChooseReseau.BitBtn1Click(Sender: TObject);
begin

end;

procedure TDialogChooseReseau.lsbReseauxClick(Sender: TObject);
begin
   FIdxReseau := lsbReseaux.ItemIndex;
end;

procedure TDialogChooseReseau.ListerReseaux;
var
  R: TReseau;
  i: integer;
begin
  lsbReseaux.Clear;
  for i := 0 to FDocuTopo.GetNbReseaux - 1 do begin
    R := FDocuTopo.GetReseau(i);
    lsbReseaux.Items.Add(Format('%d: %s',[R.IdxReseau, PurgerAccents(R.NomReseau)]));
  end;

  lsbReseaux.ItemIndex :=FIdxReseau;
  lbNbReseaux.Caption  := Format('%d reseaux', [FDocuTopo.GetNbReseaux]);
end;

initialization
  //{$I dlgChooseReseau.lrs}

end.

