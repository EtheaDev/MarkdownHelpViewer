program MarkDownHelpViewerDemoFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainFormFmx in 'MainFormFmx.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
