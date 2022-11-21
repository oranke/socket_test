program Server2;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  ShowCliUnit in '..\Client2\ShowCliUnit.pas' {ShowCliForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
