program mypaint;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, About, UFigures, UTools, UScale;

{$R *.res}

begin
  Application.Title:='mypaint';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainform, Mainform);
  Application.CreateForm(TAboutform, Aboutform);
  Application.Run;
end.

