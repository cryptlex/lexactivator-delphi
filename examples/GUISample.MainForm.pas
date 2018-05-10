unit GUISample.MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, LexActivator;

type
  TGUISampleForm = class(TForm)
    lblProductKey: TLabel;
    edtLicenseKey: TEdit;
    statStatusBar: TStatusBar;
    btnActivateTrial: TButton;
    btnActivate: TButton;
    lblExtraData: TLabel;
    edtExtraData: TEdit;
    procedure FormShow(Sender: TObject);
    procedure btnActivateClick(Sender: TObject);
    procedure btnActivateTrialClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure OnLexActivator(const Error: Exception; Status: TLAKeyStatus);
  public
    { Public declarations }
  end;

var
  GUISampleForm: TGUISampleForm;

implementation

uses
  DateUtils, Math,
  LexActivator.DelphiFeatures; // only because of TClass_UnitName

{$R *.dfm}

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
    'QzZDQTZDMkU5NTY4MDFDQ0VCM0VCQjJFNzYyODM0OEY=.Y15KUZ0RU08m' +
    'F6mx72G1iPrW/sE/38J9LRUbRlAZioz11TYOEA1M+bAQrs0O81inQ3bdc' +
    'lUSFaXjXNukfuO+z0uBsgAI8QJ4GQntyPfnvjty4/MjtQT7JE/YjLAjLL' +
    '4fqSyFt7RignRY9gbHOSQF7sIwYy+qL5CVyhSojY2bAwO7fWE9ED0ihjr' +
    'nCCC+2F8G+sYrzR315MpQ0xgm0FEyyZDbNoehkqn2J0xi2xkkM/rDn+NO' +
    'sUzKzyOO5rOlunWiuYnx2Z/WcWif/MA/lRs78u1RQ4siWQKEi96zSGYHR' +
    'wi13zhs5IA75k9JFDVUJXE1iWfjp45VYLi3lm7DZTfCS3hYE7qHlZe4iQ' +
    'LfRhVfkBE4GET16UL2lbeDTo0tQalOx+HDFH0FrAy5qZAFrFxV5FCKPwm' +
    'jqcIMjS8mOf3m49ex0+X4MRAUkRaN2539fFBvMkvKuaI8zEWLoFs1xqZU' +
    'b7uD9P5iJnxybjoJib3ANEPsPuaCFqM2CDP+arlCAoSEC/Ngfox+RS0DG' +
    'Wu8CvQ6UF+EPri4x0vB5gKpA1X8V9cFmzYjBqhB9I9kcL9zQQ+a/OVR2W' +
    'VWv+lcu1mj7L2B7X4r6LXXYwr1P6bo9zXwTlytmakUzYHDv2zdcybknKj' +
    'ln/eaOeM2qdFz92PCpM9z4lNkbZcQy8B7q2PzqBhcJHNjmElSuJSa0dBM' +
    'n6ahCooot1h0EGzmV+uWiSWhlN3IGfyoXeyqG90UEtR6K4dx4QQ=';
  PASTE_PRODUCT_ID: UnicodeString = '7dd70d27-4d76-439d-8ddf-ce29469e0150';

function UTCNow: TDateTime;
var
  SystemTime: TSystemTime;
begin
  GetSystemTime(SystemTime);
  with SystemTime do
    Result := EncodeDate(wYear, wMonth, wDay) +
      EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
end;
  
procedure TGUISampleForm.FormShow(Sender: TObject);
var
  Step: string;
  Status: TLAKeyStatus;
begin
  try
    Step := 'SetProductData'; SetProductData(PASTE_CONTENT_OF_PRODUCT_DAT_FILE);
    Step := 'SetProductId'; SetProductId(PASTE_PRODUCT_ID, lfUser);
    Step := 'SetAppVersion'; SetAppVersion('PASTE_YOUR_APP_VERION');
    // GUI application has message loop, it makes sense to use Synchronized = True
    // for automatic synchronization with main thread just like most Delphi
    // components do
    Step := 'SetLicenseCallback'; SetLicenseCallback(OnLexActivator, True);

    Step := 'IsLicenseGenuine'; Status := IsLicenseGenuine;
    if Status in [lkOK, lkGracePeriodOver] then
    begin
      statStatusBar.Panels[0].Text := 'Product genuinely activated!';
      btnActivate.Caption := 'Deactivate';
      btnActivateTrial.Enabled := False;
      Exit;
    end;
    Step := 'IsTrialGenuine'; Status := IsTrialGenuine;
    case Status of
      lkOK: begin
        Step := 'GetTrialExpiryDate';
        statStatusBar.Panels[0].Text :=
          'Trial period! Days left: ' + IntToStr(Max(Ceil(GetTrialExpiryDate - UTCNow), 0));
        btnActivateTrial.Enabled := False;
      end;
      lkExpired: begin
        statStatusBar.Panels[0].Text := 'Trial has expired!';
      end;
    else
      statStatusBar.Panels[0].Text :=
        'Trial has not started or has been tampered: ' +
        LAKeyStatusToString(Status);
    end;
  except
    on E: Exception do
    begin
      statStatusBar.Panels[0].Text := 'Exception from ' + Step + ': ' +
        ScopedClassName(E.ClassType) + ': ' + E.Message;
    end;
  end;
end;

procedure TGUISampleForm.btnActivateClick(Sender: TObject);
var
  Step: string;
  Status: TLAKeyStatus;
begin
  try
    if (Sender as TButton).Caption = 'Deactivate' then
    begin
      Step := 'DeactivateLicense'; Status := DeactivateLicense;
      if lkOK = Status then
      begin
        statStatusBar.Panels[0].Text := 'Product deactivated successfully';
        (Sender as TButton).Caption := 'Activate';
        btnActivateTrial.Enabled := True;
        Exit;
      end;
      statStatusBar.Panels[0].Text := 'Error deactivating product: ' +
        LAKeyStatusToString(Status);
      Exit;
    end;

    Step := 'SetLicenseKey'; SetLicenseKey(edtLicenseKey.Text);
    Step := 'SetActivationMetadata'; SetActivationMetadata('key1', edtExtraData.Text);
    Step := 'ActivateLicense'; Status := ActivateLicense;
    if Status <> lkOk then
    begin
      statStatusBar.Panels[0].Text := 'Error activating the product: ' +
        LAKeyStatusToString(Status);
      Exit;
    end else begin
      statStatusBar.Panels[0].Text := 'Activation Successful';
      (Sender as TButton).Caption := 'Deactivate';
      btnActivateTrial.Enabled := False;
    end;
  except
    on E: Exception do
    begin
      statStatusBar.Panels[0].Text := 'Exception from ' + Step + ': ' +
        ScopedClassName(E.ClassType) + ': ' + E.Message;
    end;
  end;
end;

procedure TGUISampleForm.btnActivateTrialClick(Sender: TObject);
var
  Step: string;
  Status: TLAKeyStatus;
begin
  try
    Step := 'SetTrialActivationMetadata'; SetTrialActivationMetadata('key1', edtExtraData.Text);
    Step := 'ActivateTrial'; Status := ActivateTrial;
    if Status <> lkOK then
    begin
      statStatusBar.Panels[0].Text :=
        'Error activating the trial: ' + LAKeyStatusToString(Status);
      Exit;
    end else begin
      statStatusBar.Panels[0].Text := 'Trial started Successful';
    end;
  except
    on E: Exception do
    begin
      statStatusBar.Panels[0].Text := 'Exception from ' + Step + ': ' +
        ScopedClassName(E.ClassType) + ': ' + E.Message;
    end;
  end;
end;

procedure TGUISampleForm.OnLexActivator(const Error: Exception; Status: TLAKeyStatus);
begin
  // Synchronized with main thread, can access UI
  if Status <> lkException then
  begin
    statStatusBar.Panels[0].Text := 'Asynchronous key status: ' + LAKeyStatusToString(Status);
  end else if Assigned(Error) then begin
    statStatusBar.Panels[0].Text := 'Asynchronous event: ' +
      ScopedClassName(Error.ClassType) + ': ' + Error.Message;
  end;
end;

procedure TGUISampleForm.FormDestroy(Sender: TObject);
var
  Step: string;
begin
  try
    Step := 'ResetLicenseCallback'; ResetLicenseCallback;
  except
    on E: Exception do
    begin
      statStatusBar.Panels[0].Text := 'Exception from ' + Step + ': ' +
        ScopedClassName(E.ClassType) + ': ' + E.Message;
    end;
  end;
end;

end.
