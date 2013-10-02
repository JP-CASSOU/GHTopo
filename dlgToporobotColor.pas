unit dlgToporobotColor;
// Sélecteur de couleurs TOPOROBOT
{$mode delphi}{$H+}
// Date: 19/04/2012
// Statut: Fonctionnel
// Seule la palette TOPOROBOT est gérée par le dialogue
// (les autres étant inutilisées)

interface

uses
  Common,
  UnitClassPalette,
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type

  { TDialogSelTopoColor }

  TDialogSelTopoColor = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    GP:      TPanel;
    pbxPalette: TPaintBox;
    lblIndex: TStaticText;
    procedure pbxPaletteMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure UpdateColor(const XX, YY: integer);
    procedure UpdateColorPrm(const Idx: byte);
    function GetIndex(const XX, YY: integer): byte;

    procedure FormDestroy(Sender: TObject);
    procedure pbxPaletteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure pbxPalettePaint(Sender: TObject);
  private
    { private declarations }
    FPalette:   TPalette256;
    FCanDrawPalette: boolean;
    FCurrIndex: integer;
    procedure DrawPalette;
  public
    { public declarations }
    function  Initialize(const Idx: integer): boolean;
    procedure Finalize;
    function  GetIdxColor: integer;
    function  GetColor: TColor;

  end;

var
  DialogSelTopoColor: TDialogSelTopoColor;

implementation

{ TDialogSelTopoColor }
function TDialogSelTopoColor.GetIdxColor: integer;
begin
  Result := FCurrIndex;
end;

function TDialogSelTopoColor.GetColor: TColor;
begin
  Result := FPalette.GetColorByIndex(FCurrIndex);
end;

procedure TDialogSelTopoColor.DrawPalette;
var
  CCX, CCY: integer;
  i, h, l, t: integer;
  R, RC: TRect;
  TmpBuffer: TBitMap;
begin
  if not FCanDrawPalette then
    Exit;

  CCX      := pbxPalette.Width div 16;
  CCY      := pbxPalette.Height div 16;
  TmpBuffer := TBitmap.Create;
  try
    with TmpBuffer do begin
      Height   := pbxPalette.Height;
      Width    := pbxPalette.Width;
      R.Left   := pbxPalette.Left;
      R.Top    := pbxPalette.Top;
      R.Bottom := pbxPalette.Left + pbxPalette.Height;
      R.Right  := pbxPalette.Top + pbxPalette.Width;
      Canvas.Brush.Color := clWhite;
      Canvas.FillRect(R);
      with TmpBuffer.Canvas do begin
        Brush.Color := clWhite;
        FillRect(R);
        // dessiner les carrés
        h := 0;
        l := 0;
        for i := 0 to 255 do begin
          t := h * CCY;
          if h > 15 then begin
            l := l + CCX;
            t := 0;
            h := 0;
          end;
          RC.Left   := l;
          RC.Top    := t;
          RC.Right  := l + CCX;
          RC.Bottom := t + CCY;
          Brush.Color := FPalette.GetColorByIndex(i);
          FillRect(RC);
          Inc(h);
        end;
      end;



    end;
    pbxPalette.Canvas.CopyRect(R, TmpBuffer.Canvas, R);

  finally
    TmpBuffer.Free;
  end;
  (*
   R.Left   := 0;
  R.Top    := 0;
  R.Right  := GP.Width;
  R.Bottom := GP.Height;
  CCX      := pbxPalette.Width div 16;
  CCY      := pbxPalette.Height div 16;
  finally
  end;
  with pbxPalette.Canvas do
  begin

    Brush.Color := clWhite;
    FillRect(R);
    // dessiner les carrés
    h := 0;
    l := 0;
    for i := 0 to 255 do
    begin
      t := h * CCY;
      if h > 15 then
      begin
        l := l + CCX;
        t := 0;
        h := 0;
      end;
      R.Left   := l;
      R.Top    := t;
      R.Right  := l + CCX;
      R.Bottom := t + CCY;
      Brush.Color := FPalette.GetColorByIndex(i);
      FillRect(R);
      Inc(h);
    end;
  end;
  //*)
end;

procedure TDialogSelTopoColor.UpdateColorPrm(const Idx: byte);
begin
  FCurrIndex     := Idx;
  lblIndex.Color := FPalette.GetColorByIndex(Idx);
  lblIndex.Caption := Format('#%d', [FCurrIndex]);
end;

procedure TDialogSelTopoColor.UpdateColor(const XX, YY: integer);
begin
  FCurrIndex := GetIndex(XX, YY);
  UpdateColorPrm(FCurrIndex);
end;

procedure TDialogSelTopoColor.pbxPaletteMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
end;

function TDialogSelTopoColor.GetIndex(const XX, YY: integer): byte;
var
  L, C:     integer;
  CCX, CCY: integer;
begin
  Result := 0;
  CCX    := pbxPalette.Width div 16;
  CCY    := pbxPalette.Height div 16;
  L      := YY div CCY;
  C      := XX div CCX;
  //Label4.Caption:=Format('L%d C%d',[L,C]);
  Result := C * 16 + L;

end;

procedure TDialogSelTopoColor.FormDestroy(Sender: TObject);
begin
end;

procedure TDialogSelTopoColor.pbxPaletteMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  UpDateColor(X, Y);
end;

procedure TDialogSelTopoColor.pbxPalettePaint(Sender: TObject);
begin
  DrawPalette;
end;

function TDialogSelTopoColor.Initialize(const Idx: integer): boolean;
begin
  FCanDrawPalette := False;
  FCurrIndex := Idx;
  FPalette := TPalette256.Create;
  FPalette.GenerateTOPOROBOTPalette;
  FCanDrawPalette := True;

  pbxPalette.Invalidate;
  lblIndex.Caption := IntToStr(FCurrIndex);
end;
procedure TDialogSelTopoColor.Finalize;
begin
  FPalette.Free;
end;

initialization
{$I dlgToporobotColor.lrs}

end.

