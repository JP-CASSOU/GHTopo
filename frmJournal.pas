unit frmJournal;
// Date: 19/04/2012
// Statut: Fonctionnel
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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TdlgProcessing }

  TdlgProcessing = class(TForm)
    ListBox1: TListBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  dlgProcessing: TdlgProcessing;

implementation

{$R *.lfm}

{ TdlgProcessing }

procedure TdlgProcessing.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=False;
  ShowMessage(AnsiToUtf8(rsNOCANCLOSEWND));
end;

end.

