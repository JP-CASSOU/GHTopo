unit VueEn3DOpenGL;
// Date: 11/07/2012
// Statut: Fonctionnel - Portage en cours
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
  Common,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  CadreVisuOpenGL;

type

  { TdlgVue3DOpenGL }

  TdlgVue3DOpenGL = class(TForm)
    CdrVueOpenGL1: TCdrVueOpenGL;
  private

    { private declarations }
  public
    { public declarations }
    function InitaliserVue3DOpenGL(const Filename: string): boolean;
  end; 

var
  dlgVue3DOpenGL: TdlgVue3DOpenGL;


implementation

{$R *.lfm}




function TdlgVue3DOpenGL.InitaliserVue3DOpenGL(const Filename: string): boolean;
begin
  Result := False;
  self.Caption := AnsiToUtf8(EnlevePerluete(rsRENDU3D)) + ': ' + ExtractFileName(Filename);
  try
    Result := CdrVueOpenGL1.Initialise(Filename);
  except

  end;
end;

end.

