unit CadreHistoDirections;

{$mode delphi}

interface

uses
  Common,
  UnitEntites,
  Graphics,
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls;

type

  { TCdrHistogrammes }

  TCdrHistogrammes = class(TFrame)
    editFiltres: TEdit;
    imgHistogrammes: TImage;
    lbFiltres: TLabel;
    Panel1: TPanel;
  private
    { private declarations }
    FTableEntites: TTableDesEntites;

    FFiltres     : string;
    FNbPetales   : integer;
    FColorBarre  : TColor;
    FColorFill   : TColor;
    FX, FY       : integer;
    procedure DessinerHistogramme;
    procedure SetHistoParameters(const Filtres: string;
      const NbPetales: integer; const ColorBarre, ColorFill: TColor);

  public
    { public declarations }
    function Initialise(const DT: TTableDesEntites; const Filtres: string): boolean;
  end; 

implementation

{$R *.lfm}
procedure TCdrHistogrammes.SetHistoParameters(const Filtres: string;
                                                const NbPetales: integer;
                                                const ColorBarre, ColorFill: TColor);
begin
  FFiltres      := Filtres;
  FNbPetales    := NbPetales;
  FColorBarre   := ColorBarre;
  FColorFill    := ColorFill;
end;
function TCdrHistogrammes.Initialise(const DT: TTableDesEntites; const Filtres: string): boolean;
begin
  Result := False;
  FTableEntites := DT;
  try
    SetHistoParameters(Filtres, 30, clRed, clYellow);
    DessinerHistogramme;
    Result := True;
  except
  end;

end;
procedure TCdrHistogrammes.DessinerHistogramme;
begin
  AfficherMessage(Format('%s.DessinerHistogramme("%s")',[ClassName, FFiltres]));
  // appliquer les filtres
  lbFiltres.Caption := 'Filtres:';
  editFiltres.Text  := FFiltres;
  //Exit;
  FTableEntites.MetaFiltre(FFiltres);
  // recalculer histogramme;
  FTableEntites.ParseDiagram(FNbPetales, 1.00);
  FTableEntites.DrawDiagram(imgHistogrammes.Canvas,
                            imgHistogrammes.Width,
                            FColorBarre, FColorFill);
  //imgHistoAltitudes.Invalidate;
end;

end.

