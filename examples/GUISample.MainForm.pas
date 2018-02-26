unit GUISample.MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TGUISampleForm = class(TForm)
    lblProductKey: TLabel;
    edtProductKey: TEdit;
    statStatusBar: TStatusBar;
    btnActivateTrial: TButton;
    btnActivate: TButton;
    lblExtraData: TLabel;
    edtExtraData: TEdit;
    procedure FormShow(Sender: TObject);
    procedure btnActivateClick(Sender: TObject);
    procedure btnActivateTrialClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GUISampleForm: TGUISampleForm;

implementation

uses
  LexActivator.DelphiFeatures, // only because of TClass_UnitName
  LexActivator;

{$R *.dfm}

function ScopedClassName(Item: TClass): string;
var
  UnitName: string;
begin
  UnitName := TClass_UnitName(Item);
  if UnitName <> '' then
    Result := UnitName + '.' + Item.ClassName else Result := Item.ClassName;
end;


procedure TGUISampleForm.FormShow(Sender: TObject);
var
  Step: string;
  Status: TLAKeyStatus;
begin
  try
    Step := 'SetProductFile'; SetProductFile('Product.dat');
    Step := 'SetVersionGUID';
    SetVersionGUID('1EEBD7A6-7691-6E91-4524-7B7E68EF5F8B', lfUser);
    Step := 'IsProductGenuine'; Status := IsProductGenuine;
    if (Status = lkOK) or (Status = lkGPOver) then
    begin
      statStatusBar.Panels[0].Text := 'Product genuinely activated!';
      btnActivate.Caption := 'Deactivate';
      btnActivateTrial.Enabled := False;
      Exit;
    end;
    Step := 'IsTrialGenuine'; Status := IsTrialGenuine;
    case Status of
      lkOK: begin
        Step := 'GetTrialDaysLeft';
        statStatusBar.Panels[0].Text :=
          'Trial period! Days left: ' + IntToStr(GetTrialDaysLeft(ltVTrial));
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
      Step := 'DeactivateProduct'; Status := DeactivateProduct;
      if lkOK = status then
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

    Step := 'SetProductKey'; SetProductKey(edtProductKey.Text);
    Step := 'SetExtraActivationData'; SetExtraActivationData(edtExtraData.Text);
    Step := 'ActivateProduct'; Status := ActivateProduct;
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
    Step := 'SetTrialKey';
    SetTrialKey('233FA0ED-44FF0B55-54A6AC79-60CB2783-F5ADC299');
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

end.
