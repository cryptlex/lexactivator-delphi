program Sample;

{$IFDEF FPC}
  {$mode delphiunicode}
  {$H+}
  {$apptype console}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$IFNDEF FPC}
  {$IF CompilerVersion >= 23.0}
    {$DEFINE DELPHI_UNITS_SCOPED}
  {$IFEND}
{$ENDIF}

uses
{$IFDEF FPC}
  SysUtils, Windows, Math, DateUtils,
//{$ELSEIF DEFINED(DELPHI_UNITS_SCOPED)}
//  System.SysUtils, Winapi.Windows, System.Math, System.DateUtils,
//{$ELSE}
//  SysUtils, Windows, Math, DateUtils,
{$ENDIF}
  LexActivator,
  LexActivator.DelphiFeatures;


const
  TryActivateAnyway = True; // change to True or False for testing

function ScopedClassName(Item: TClass): string;
var
  UnitName: string;
begin
  UnitName := TClass_UnitName(Item);
  if UnitName <> '' then
    Result := UnitName + '.' + Item.ClassName else Result := Item.ClassName;
end;

const
  ProductData: UnicodeString = 'QTNGMDkzMTExNzM5MjFDMTZFQzg0NDdGRDE1ODAxNUQ=.wfhNAAY3MLlYVJJpqKI3D/WjrfXJXLHCkg1bViGf/fbDKTe9yMserQjlyNoZmoh5xizGWW41qvcWTjjiTA9P91i4xGd70JkOqQjjAu+NpinzXMFm+XHVV66/P09sTzc3is4Z/UX2eqZFBlxVbfE4q38Az6331+OZmmlS+HzA+MZcdjVYv5/hytBC2SDaVZjAXLR8YhNWDj9/nFr+4Kb1ZgGSD/YtH6b1JireSEp9j3l5I/fWw7q/5mHtmDcabMMfofetfdSIr8eOxi4nIjKvB229QffDOql5ljnvSW2XMcZ4xEUWS71h/sQuswsAfgwoGB5b7LbMac0/ocmKuOBMRLKDTvPDkCItfAeJ+v8wgi5RlUSejKoJSpWMN+5Mf+5iB2BN3S6eRji+gkA4m9m1PxNv3pUvDxWUhUBYqUZbR6yp3yhUmovVMhlOpfhTNvDzZl+zbRWvn4bBxgFBwGqmh830vFIX51yw7OxDGC+llzDjT+SEjsGAohW66Z+HwDY3EYxf9qkylMw/Kimt71QtaR3AqfdQzg3jjp+KqFsVKxQgvrLRjtpzSDhBS5umr43RuX67q9ElBUFXLeKDLl2bRWrChgtdAi4B15rNDOnQsY5Lm9U6adNeRspv6TzaobGIkTb1dhVc0hC8shvOvPyiTwAuOPEhwg5e3w7Hg2wEu4THwJ4Dyco8tP/wr/uwSNL+HMfasiv2EcHkiDFml64s1n2R4cCgnAU1QQWcL6lT0A7V2+YzjKxr9ytds8AZyzGSqzx0gDkmHrKnqLf1SUednDsvEs2lzFuwMQV0WELflM6/fTiZ/L+Mz/hWld9rc78Q';
  ProductId: UnicodeString = '01997b28-eeb0-7fcb-aab2-a6bf4a2f6fc3';
  LicenseKey: UnicodeString = '2430B5-3FFBAA-49D881-311BC2-D1A748-E2DDEB';

procedure Init;
var
  Step: string;
begin
  try
    Step := 'SetProductData'; SetProductData(ProductData);
    Step := 'SetProductId'; SetProductId(ProductId, lfUser);
    Step := 'SetReleaseVersion'; SetReleaseVersion('0.0.0');
    SetDebugMode(1);
  except
    on E: Exception do
    begin
      WriteLn('Exception from ', Step, ': ', ScopedClassName(E.ClassType));
      WriteLn(E.Message);
      raise;
    end;
  end;
end;

// Ideally on a button click inside a dialog
procedure Activate;
var
  Step: string;
  Status: TLAKeyStatus;
begin
  try
    Step := 'SetLicenseKey'; SetLicenseKey(LicenseKey);
    Step := 'SetActivationMetadata'; SetActivationMetadata('key1', 'value1');
    Step := 'ActivateLicense'; Status := ActivateLicense;
    case Status of
      lkOK, lkExpired, lkSuspended:
        WriteLn('License activated successfully, status: ', LAKeyStatusToString(Status));
      // other statuses can go here, use LAKeyStatusToString(Status) to display identifier
    else
      raise ELAKeyStatusError.CreateByKeyStatus(Status)
    end;
  except
    on E: Exception do
    begin
      WriteLn('Exception from ', Step, ': ', ScopedClassName(E.ClassType));
      WriteLn(E.Message);
      raise;
    end;
  end;
end;

// Ideally on a button click inside a dialog
procedure ActivateTrial;
var
  Step: string;
  Status: TLAKeyStatus;
begin
  try
    Step := 'SetTrialActivationMetadata'; SetTrialActivationMetadata('key1', 'value1');
    Step := 'ActivateTrial'; Status := LexActivator.ActivateTrial;
    case Status of
      lkOK: WriteLn('Product trial activated successfully!');
      lkTrialExpired: WriteLn('Product trial has expired!');
      // other statuses can go here, use LAKeyStatusToString(Status) to display identifier
    else
      raise ELAKeyStatusError.CreateByKeyStatus(Status);
    end;
  except
    on E: Exception do
    begin
      WriteLn('Exception from ', Step, ': ', ScopedClassName(E.ClassType));
      WriteLn(E.Message);
      raise;
    end;
  end;
end;

function UTCNow: TDateTime;
var
  SystemTime: TSystemTime;
begin
  GetSystemTime(SystemTime);
  with SystemTime do
    Result := EncodeDate(wYear, wMonth, wDay) +
      EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
end;

procedure OnLexActivator(const Error: Exception; Status: TLAKeyStatus);
begin
  // No synchronization, write everything to console
  if Assigned(Error) then
  begin
    WriteLn('Asynchronous event: ', ScopedClassName(Error.ClassType));
    WriteLn(Error.Message);
  end;

  if Status <> lkException then
  begin
    WriteLn('Key status: ', LAKeyStatusToString(Status));
  end;
end;

var
  Status: TLAKeyStatus;
  Step: string;
  WriteException: Boolean = True;
  ExpiryDate: TDateTime;
  DaysLeft: Integer;
  TrialStatus: TLAKeyStatus;
  TrialExpiryDate: TDateTime;
begin
  try
    // embedded in now
    (*
    WriteLn('Entering ReadSampleData...');
    ReadSampleData;
    WriteLn('Exiting ReadSampleData...');
    *)

    WriteLn('Entering Init...');
    WriteException := False; Init; WriteException := True;
    WriteLn('Exiting Init...');
    // console application has no message loop, thus Synchronized is False
    Step := 'SetLicenseCallback'; SetLicenseCallback(OnLexActivator, False);

    if TryActivateAnyway then
    begin
      WriteLn('Entering Activate...');
      WriteException := False; Activate; WriteException := True;
      WriteLn('Exiting Activate...');
    end;

    Step := 'IsLicenseGenuine'; Status := IsLicenseGenuine;
    case Status of
      lkOK:
      begin
        Step := 'GetLicenseExpiryDate'; ExpiryDate := GetLicenseExpiryDate;
        DaysLeft := Max(Ceil(ExpiryDate - UTCNow), 0);
        WriteLn('Days left: ', DaysLeft);
        WriteLn('License is genuinely activated!');
      end;
      lkExpired:
        WriteLn('License is genuinely activated but has expired!');
      lkSuspended:
        WriteLn('License is genuinely activated but has been suspended!');
      lkGracePeriodOver:
        WriteLn('License is genuinely activated but grace period is over!');
    else
      Step := 'IsTrialGenuine'; TrialStatus := IsTrialGenuine;
      case TrialStatus of
        lkOk:
        begin
          Step := 'GetTrialExpiryDate'; TrialExpiryDate := GetTrialExpiryDate;
          DaysLeft := Max(Ceil(TrialExpiryDate - UTCNow), 0);
          WriteLn('Trial days left: ', DaysLeft);
        end;
        lkTrialExpired:
        begin
          WriteLn('Trial has expired!');

          // Time to buy the license and activate the app
          WriteLn('Entering Activate...');
          WriteException := False; Activate; WriteException := True;
          WriteLn('Exiting Activate...');
        end;
      else
        WriteLn('Either trial has not started or has been tampered! Status ', LAKeyStatusToString(TrialStatus));

        // Activating the trial
        WriteLn('Entering ActivateTrial...');
        WriteException := False; ActivateTrial; WriteException := True;
        WriteLn('Exiting ActivateTrial...');
      end;
    end;
  except
    on E: Exception do
    begin
      if WriteException then
      begin
        WriteLn('Exception from ', Step, ': ', ScopedClassName(E.ClassType));
        WriteLn(E.Message);
      end;
      WriteLn('Exiting on exception');
    end;
  end;

  Write('Press Enter...');
  ReadLn; // let asynchronous requests happen here if any
end.

