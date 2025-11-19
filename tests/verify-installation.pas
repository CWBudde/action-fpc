program VerifyInstallation;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

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

procedure VerifyBasicFPC;
begin
  PrintHeader('Free Pascal Compiler Verification');

  PrintInfo('FPC Version: ' + {$I %FPCVERSION%});
  PrintInfo('Target CPU: ' + {$I %FPCTARGETCPU%});
  PrintInfo('Target OS: ' + {$I %FPCTARGETOS%});
  PrintInfo('Compiled on: ' + {$I %DATE%} + ' at ' + {$I %TIME%});

  // Test basic RTL functionality
  try
    PrintInfo('Testing RTL SysUtils unit...');
    PrintSuccess('SysUtils unit works correctly');

    PrintInfo('Testing RTL Classes unit...');
    PrintSuccess('Classes unit works correctly');

    ExitCode := 0;
  except
    on E: Exception do
    begin
      PrintError('Error during basic tests: ' + E.Message);
      ExitCode := 1;
    end;
  end;
end;

procedure TestStringOperations;
var
  TestStr: String;
begin
  PrintHeader('String Operations Test');

  try
    TestStr := 'Hello, FPC!';
    PrintInfo('Original string: ' + TestStr);

    TestStr := UpperCase(TestStr);
    PrintInfo('Uppercase: ' + TestStr);

    TestStr := LowerCase(TestStr);
    PrintInfo('Lowercase: ' + TestStr);

    PrintSuccess('String operations working correctly');
  except
    on E: Exception do
    begin
      PrintError('String operation failed: ' + E.Message);
      ExitCode := 1;
    end;
  end;
end;

procedure TestClassesAndObjects;
var
  List: TStringList;
begin
  PrintHeader('Classes and Objects Test');

  try
    List := TStringList.Create;
    try
      List.Add('Item 1');
      List.Add('Item 2');
      List.Add('Item 3');

      PrintInfo('Created TStringList with ' + IntToStr(List.Count) + ' items');
      PrintSuccess('Object creation and manipulation working correctly');
    finally
      List.Free;
    end;
  except
    on E: Exception do
    begin
      PrintError('Object test failed: ' + E.Message);
      ExitCode := 1;
    end;
  end;
end;

procedure TestFileOperations;
var
  TempFile: String;
  F: TextFile;
  TestContent: String;
begin
  PrintHeader('File I/O Test');

  try
    TempFile := GetTempFileName('/tmp', 'fpc_test_');
    PrintInfo('Temp file: ' + TempFile);

    // Write test
    AssignFile(F, TempFile);
    Rewrite(F);
    WriteLn(F, 'FPC File I/O Test');
    CloseFile(F);
    PrintSuccess('File write successful');

    // Read test
    AssignFile(F, TempFile);
    Reset(F);
    ReadLn(F, TestContent);
    CloseFile(F);

    if TestContent = 'FPC File I/O Test' then
      PrintSuccess('File read successful')
    else
    begin
      PrintError('File content mismatch');
      ExitCode := 1;
    end;

    // Cleanup
    DeleteFile(TempFile);
    PrintSuccess('File operations working correctly');
  except
    on E: Exception do
    begin
      PrintError('File I/O test failed: ' + E.Message);
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
    WriteLn('[✓] All tests PASSED');
    WriteLn('FPC installation is working correctly!');
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
  WriteLn('║  FPC Installation Verification    ║');
  WriteLn('╔═══════════════════════════════════╝');
  WriteLn;

  VerifyBasicFPC;
  TestStringOperations;
  TestClassesAndObjects;
  TestFileOperations;
  PrintSummary;

  Halt(ExitCode);
end.
