program VerifyLCL;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics;

type
  { TTestForm }
  TTestForm = class(TForm)
  public
    constructor Create(TheOwner: TComponent); override;
  end;

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

{ TTestForm }

constructor TTestForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Width := 400;
  Height := 300;
  Caption := 'LCL Test Form';
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

procedure TestLCLComponents;
var
  TestForm: TTestForm;
  Button: TButton;
  Label1: TLabel;
begin
  PrintHeader('LCL Components Test');

  try
    // Test form creation
    TestForm := TTestForm.Create(nil);
    try
      PrintSuccess('Form created successfully');
      PrintInfo('Form caption: ' + TestForm.Caption);

      // Test button creation
      Button := TButton.Create(TestForm);
      Button.Parent := TestForm;
      Button.Caption := 'Test Button';
      Button.Left := 10;
      Button.Top := 10;
      PrintSuccess('Button created successfully');

      // Test label creation
      Label1 := TLabel.Create(TestForm);
      Label1.Parent := TestForm;
      Label1.Caption := 'Test Label';
      Label1.Left := 10;
      Label1.Top := 50;
      PrintSuccess('Label created successfully');

      PrintSuccess('LCL components working correctly');
    finally
      TestForm.Free;
    end;
  except
    on E: Exception do
    begin
      PrintError('LCL component test failed: ' + E.Message);
      ExitCode := 1;
    end;
  end;
end;

procedure TestGraphics;
var
  Bitmap: TBitmap;
begin
  PrintHeader('Graphics Test');

  try
    Bitmap := TBitmap.Create;
    try
      Bitmap.Width := 100;
      Bitmap.Height := 100;

      // Draw something simple
      Bitmap.Canvas.Brush.Color := clRed;
      Bitmap.Canvas.FillRect(0, 0, 50, 50);

      Bitmap.Canvas.Brush.Color := clBlue;
      Bitmap.Canvas.FillRect(50, 50, 100, 100);

      PrintSuccess('Bitmap created and drawn successfully');
      PrintInfo('Bitmap size: ' + IntToStr(Bitmap.Width) + 'x' + IntToStr(Bitmap.Height));
    finally
      Bitmap.Free;
    end;
  except
    on E: Exception do
    begin
      PrintError('Graphics test failed: ' + E.Message);
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
    WriteLn('LCL installation is working correctly!');
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
  WriteLn('║  LCL Installation Verification    ║');
  WriteLn('╔═══════════════════════════════════╝');
  WriteLn;

  VerifyLCLUnits;
  TestLCLComponents;
  TestGraphics;
  PrintSummary;

  Halt(ExitCode);
end.
