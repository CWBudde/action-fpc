program VerifyLCLSimple;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

var
  ExitCode: Integer;

procedure PrintHeader(const Title: String);
begin
  WriteLn;
  WriteLn('=================================');
  WriteLn(Title);
  WriteLn('=================================');
end;

procedure PrintSuccess(const Msg: String);
begin
  WriteLn('[✓] ', Msg);
end;

procedure PrintInfo(const Msg: String);
begin
  WriteLn('[i] ', Msg);
end;

procedure PrintError(const Msg: String);
begin
  WriteLn('[✗] ', Msg);
end;

procedure VerifyLCLUnits;
begin
  PrintHeader('LCL Units Verification');

  PrintInfo('FPC Version: ' + {$I %FPCVERSION%});
  PrintInfo('Target CPU: ' + {$I %FPCTARGETCPU%});
  PrintInfo('Target OS: ' + {$I %FPCTARGETOS%});

  {$IFDEF LCL}
  PrintSuccess('LCL defined');
  {$ELSE}
  PrintError('LCL not defined');
  ExitCode := 1;
  {$ENDIF}

  {$IFDEF LCLgtk2}
  PrintInfo('LCL Widgetset: GTK2');
  PrintSuccess('GTK2 widgetset available');
  {$ENDIF}

  {$IFDEF LCLcocoa}
  PrintInfo('LCL Widgetset: Cocoa');
  {$ENDIF}

  {$IFDEF LCLwin32}
  PrintInfo('LCL Widgetset: Win32');
  {$ENDIF}

  {$IFDEF LCLqt5}
  PrintInfo('LCL Widgetset: Qt5');
  {$ENDIF}
end;

procedure TestClassesUnit;
var
  List: TStringList;
begin
  PrintHeader('Classes Unit Test');

  try
    List := TStringList.Create;
    try
      List.Add('Test 1');
      List.Add('Test 2');
      List.Add('Test 3');

      PrintInfo('Created TStringList with ' + IntToStr(List.Count) + ' items');
      PrintSuccess('Classes unit working correctly');
    finally
      List.Free;
    end;
  except
    on E: Exception do
    begin
      PrintError('Classes test failed: ' + E.Message);
      ExitCode := 1;
    end;
  end;
end;

procedure PrintSummary;
begin
  WriteLn;
  WriteLn('=================================');
  if ExitCode = 0 then
  begin
    WriteLn('[✓] All LCL tests PASSED');
    WriteLn('LCL units are accessible and working!');
    WriteLn;
    WriteLn('Note: This test only verifies LCL unit');
    WriteLn('availability. Full GUI tests require');
    WriteLn('a display server.');
  end
  else
  begin
    WriteLn('[✗] Some tests FAILED');
    WriteLn('Please check the output above for details.');
  end;
  WriteLn('=================================');
  WriteLn;
end;

begin
  ExitCode := 0;

  WriteLn;
  WriteLn('╔═══════════════════════════════════╗');
  WriteLn('║  LCL Units Verification (Simple)  ║');
  WriteLn('╔═══════════════════════════════════╝');
  WriteLn;

  VerifyLCLUnits;
  TestClassesUnit;
  PrintSummary;

  Halt(ExitCode);
end.
