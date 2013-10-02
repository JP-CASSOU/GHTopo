unit CadreHistoAltitudes;

interface

uses
  Common,
  UnitEntites,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type

  { TCdrHistoAltitudes }

  TCdrHistoAltitudes = class(TFrame)
    imgHistoAltitudes: TImage;
    lbAltitude: TLabel;
    Panel1: TPanel;
    procedure imgHistoAltitudesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetHistoParameters(const Filtres: string;
                                 const NbPetales: integer;
                                 const ColorBarre, ColorFill: TColor);
    procedure DessinerHistogramme;
    procedure imgHistoAltitudesClick(Sender: TObject);
    procedure imgHistoAltitudesMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
  private
    { Déclarations privées }
    FTableEntites: TTableDesEntites;
    FFiltres     : string;
    FNbPetales   : integer;
    FColorBarre  : TColor;
    FColorFill   : TColor;
    FX, FY       : integer;
    function GetAltitude(const X, Y: integer): double;

  public
    { Déclarations publiques }
    function Initialise(const DT: TTableDesEntites; const Filtres: string): boolean;

  end;

implementation

{$R *.lfm}

function TCdrHistoAltitudes.Initialise(const DT: TTableDesEntites;
                                       const Filtres: string): boolean;
begin
  Result := False;
  FTableEntites := DT;
  try
    SetHistoParameters(Filtres, 20, clBlue, clBlue);
    DessinerHistogramme;
    Result := True;
  except
  end;
end;

function TCdrHistoAltitudes.GetAltitude(const X, Y: integer): double;
begin
  Result := FTableEntites.GetAltitudeDiagram(imgHistoAltitudes.Canvas, imgHistoAltitudes.Height, Y);


end;
procedure TCdrHistoAltitudes.SetHistoParameters(const Filtres: string;
                                                const NbPetales: integer;
                                                const ColorBarre, ColorFill: TColor);
begin
  FFiltres      := Filtres;
  FNbPetales    := NbPetales;
  FColorBarre   := ColorBarre;
  FColorFill    := ColorFill;
end;

procedure TCdrHistoAltitudes.imgHistoAltitudesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  EWE: Double;
begin
  try
    EWE := GetAltitude(X,Y);
    lbAltitude.Caption := Format('Altitude: %.0f m', [EWE]);

  except
  end;
end;

procedure TCdrHistoAltitudes.DessinerHistogramme;
begin
  AfficherMessage(Format('%s.DessinerHistogramme("%s")',[ClassName, FFiltres]));
  // appliquer les filtres
  //Exit;
  FTableEntites.MetaFiltre(FFiltres);
  // recalculer histogramme;
  FTableEntites.ParseDepthHistogramme(FNbPetales, 1.00);
  FTableEntites.DrawDepthHistogramme(imgHistoAltitudes.Canvas,
                                     imgHistoAltitudes.Width, imgHistoAltitudes.Height,
                                     FColorBarre, FColorFill);
  //imgHistoAltitudes.Invalidate;
end;

procedure TCdrHistoAltitudes.imgHistoAltitudesClick(Sender: TObject);
begin
  ShowMessageFmt('%.2f', [GetAltitude(FX, FY)]);
end;

procedure TCdrHistoAltitudes.imgHistoAltitudesMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  FX := X; FY := Y;
end;
end.

