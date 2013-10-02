unit dlgSelectionTypeVisee;

{$INCLUDE CompilationParameters.inc}
interface

uses
  {$IFDEF FRENCH_MESSAGES}
     UnitMessages_fr,
  {$ENDIF}
  {$IFDEF ENGLISH_MESSAGES}
     UnitMessages_en,
  {$ENDIF}
  {$IFDEF SPANISH_MESSAGES}
     UnitMessages_sp,
  {$ENDIF}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls;

type

  { TdlgSelectTypeVisee }

  TdlgSelectTypeVisee = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    RadioGroup1: TRadioGroup;
    procedure FormCreate(Sender: TObject);
  private

    { private declarations }
  public
    { public declarations }
    function  GetTypeVisee: integer;
    procedure SetTypeVisee(const N: integer);
  end; 

var
  dlgSelectTypeVisee: TdlgSelectTypeVisee;

implementation

{$R *.lfm}
function TrimNumeros(const S: string): string;
var
  WU: string;
  P : integer;
begin
  WU := Trim(S);
  Result := AnsiToUtf8(WU);

end;

{ TdlgSelectTypeVisee }
procedure TdlgSelectTypeVisee.SetTypeVisee(const N: integer);
begin
  RadioGroup1.ItemIndex := N;
end;
function TdlgSelectTypeVisee.GetTypeVisee: integer;
begin
  Result := RadioGroup1.ItemIndex;
end;

procedure TdlgSelectTypeVisee.FormCreate(Sender: TObject);
begin
  self.Caption := AnsiToUtf8(rsTITRE_SELECTEUR_VISEE);
  RadioGroup1.Caption := AnsiToUtf8(rsCDR_CODES_TYPEGALERIE);
  RadioGroup1.Items.Clear;
  RadioGroup1.Items.Add(TrimNumeros(rsCMBTYPE_D));
  RadioGroup1.Items.Add(TrimNumeros(rsCMBTYPE_E));
  RadioGroup1.Items.Add(TrimNumeros(rsCMBTYPE_B));
  RadioGroup1.Items.Add(TrimNumeros(rsCMBTYPE_V));
  RadioGroup1.Items.Add(TrimNumeros(rsCMBTYPE_W));
  RadioGroup1.Items.Add(TrimNumeros(rsCMBTYPE_C));
  RadioGroup1.Items.Add(TrimNumeros(rsCMBTYPE_F));
  RadioGroup1.Items.Add(TrimNumeros(rsCMBTYPE_S));
  RadioGroup1.Items.Add(TrimNumeros(rsCMBTYPE_A));
  RadioGroup1.Items.Add(TrimNumeros(rsCMBTYPE_M));
end;

end.

