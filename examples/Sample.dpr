program Sample;

{$APPTYPE CONSOLE}

{$IF CompilerVersion >= 23.0}
  {$DEFINE DELPHI_UNITS_SCOPED}
{$IFEND}

uses
{$IFDEF DELPHI_UNITS_SCOPED}
  System.SysUtils, Winapi.Windows, System.Math, System.DateUtils,
{$ELSE}
  SysUtils, Windows, Math, DateUtils,
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
  PASTE_CONTENT_OF_PRODUCT_DAT_FILE: UnicodeString =
    'PASTE_CONTENT_OF_PRODUCT_DAT_FILE';
  PASTE_PRODUCT_ID: UnicodeString = 'PASTE_PRODUCT_ID';
  PASTE_LICENCE_KEY: UnicodeString = 'PASTE_LICENCE_KEY';

(*
procedure ReadSampleData;
var
  Source: Text;
begin
  Assign(Source, 'product.dat.txt');
  try
    Reset(Source);
    ReadLn(Source, PASTE_CONTENT_OF_PRODUCT_DAT_FILE);
  finally
    Close(Source);
  end;
  Assign(Source, 'data.txt');
  try
    Reset(Source);
    ReadLn(Source, PASTE_LICENCE_KEY);
    ReadLn(Source, PASTE_PRODUCT_ID);
  finally
    Close(Source);
  end;

  PASTE_LICENCE_KEY := Copy(PASTE_LICENCE_KEY, Pos(': ', PASTE_LICENCE_KEY) + 2, Length(PASTE_LICENCE_KEY) - Pos(': ', PASTE_LICENCE_KEY) - 1);
  PASTE_PRODUCT_ID := Copy(PASTE_PRODUCT_ID, Pos(': ', PASTE_PRODUCT_ID) + 2, Length(PASTE_PRODUCT_ID) - Pos(': ', PASTE_PRODUCT_ID) - 1);
end;
*)

procedure Init;
var
  Step: string;
begin
  try
    Step := 'SetProductData'; SetProductData(PASTE_CONTENT_OF_PRODUCT_DAT_FILE);
    Step := 'SetProductId'; SetProductId(PASTE_PRODUCT_ID, lfUser);
    Step := 'SetAppVersion'; SetAppVersion('PASTE_YOUR_APP_VERION');
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
    Step := 'SetLicenseKey'; SetLicenseKey(PASTE_LICENCE_KEY);
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

