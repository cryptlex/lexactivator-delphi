program Sample;

{$APPTYPE CONSOLE}

{$IF CompilerVersion >= 23.0}
  {$DEFINE DELPHI_UNITS_SCOPED}
{$IFEND}

uses
{$IFDEF DELPHI_UNITS_SCOPED}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF}
  LexActivator in 'LexActivator.pas',
  LexActivator.DelphiFeatures in 'LexActivator.DelphiFeatures.pas';

function ScopedClassName(Item: TClass): string;
var
  UnitName: string;
begin
  UnitName := TClass_UnitName(Item);
  if UnitName <> '' then
    Result := UnitName + '.' + Item.ClassName else Result := Item.ClassName;
end;

procedure TestActivation;
var
  Status: TLAKeyStatus;
begin
  try
    Status := ActivateProduct;
    WriteLn('Status: ', LAKeyStatusToString(Status));
    case Status of
      lkOK: WriteLn('Activated Successfully');
      lkExpired: WriteLn('License Expired');
      lkRevoked: WriteLn('License Revoked');
    end;
  except
    on ELAFailException do WriteLn('Failed! Invalid License');
    on E: Exception do
    begin
      WriteLn('Exception: ', ScopedClassName(E.ClassType));
      WriteLn(E.Message);
    end;
	end;
end;

procedure Init;
var
  Step: string;
begin
  try
    Step := 'SetProductFile'; SetProductFile('Product.dat');
    Step := 'SetVersionGUID';
    SetVersionGUID('1EEBD7A6-7691-6E91-4524-7B7E68EF5F8B', lfUser);
    Step := 'SetProductKey'; SetProductKey('8E925-AA1EE-26D27-A8511-9CDD3');
    Step := 'SetExtraActivationData'; SetExtraActivationData('3A15E2E');
  except
    on E: Exception do
    begin
      WriteLn('Exception from ' + Step + ': ', ScopedClassName(E.ClassType));
      WriteLn(E.Message);
      raise;
    end;
	end;
end;

begin
  try
    WriteLn('Entering Init...');
    Init;
    WriteLn('Exiting Init...');
    WriteLn('Entering TestActivation...');
    TestActivation;
    WriteLn('Exiting TestActivation...');
  except
    WriteLn('Exiting on exception');
  end;
end.
