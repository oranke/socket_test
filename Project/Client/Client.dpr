program Client;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
