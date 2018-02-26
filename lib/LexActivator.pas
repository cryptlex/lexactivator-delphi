{$WARN UNSAFE_TYPE OFF} // PAnsiChar, PWideChar, untyped

unit LexActivator;

interface

{$IF CompilerVersion >= 16.0}
  {$DEFINE DELPHI_HAS_UINT64}
{$IFEND}

{$IF CompilerVersion >= 17.0}
  {$DEFINE DELPHI_HAS_INLINE}
{$IFEND}

{$IF CompilerVersion >= 18.0}
  {$DEFINE DELPHI_CLASS_CAN_BE_ABSTRACT}
  {$DEFINE DELPHI_HAS_RECORDS}
{$IFEND}

{$IF CompilerVersion >= 20.0}
  {$DEFINE DELPHI_IS_UNICODE}
{$IFEND}

{$IF CompilerVersion >= 21.0}
  {$DEFINE DELPHI_HAS_RTTI}
{$IFEND}

{$IF CompilerVersion >= 23.0}
  {$DEFINE DELPHI_HAS_INTPTR}
  {$DEFINE DELPHI_UNITS_SCOPED}
{$IFEND}

uses
  LexActivator.DelphiFeatures,
{$IFDEF DELPHI_UNITS_SCOPED}
  System.SysUtils
{$ELSE}
  SysUtils
{$ENDIF}
  ;

type
  TLAFlags = (lfUser, lfSystem);
  TLATrialType = (ltVTrial, ltUVTrial);
  TLAKeyStatus = (lkOK, lkExpired, lkRevoked, lkGPOver, lkTExpired,
    lkTExtExpired,
    lkFail // Special value, should only be returned from ELAError.CheckKeyStatus
    );

function LAFlagsToString(Item: TLAFlags): string;
function LATrialTypeToString(Item: TLATrialType): string;
function LAKeyStatusToString(Item: TLAKeyStatus): string;    

(*
    PROCEDURE: SetProductFile()

    PURPOSE: Sets the path of the Product.dat file. This should be
    used if your application and Product.dat file are in different
    folders or you have renamed the Product.dat file.

    If this procedure is used, it must be called on every start of
    your program before any other functions are called.

    PARAMETERS:
    * FilePath - path of the product file (Product.dat)

    EXCEPTIONS: ELAFPathException, ELAPFileException

    NOTE: If this function fails to set the path of product file, none of the
    other functions will work.
*)

procedure SetProductFile(const FilePath: UnicodeString);

(*
    PROCEDURE: SetVersionGUID()

    PURPOSE: Sets the version GUID of your application.

    This function must be called on every start of your program before
    any other functions are called, with the exception of SetProductFile()
    function.

    PARAMETERS:
    * VersionGUID - the unique version GUID of your application as mentioned
      on the product version page of your application in the dashboard.

    * Flags - depending upon whether your application requires admin/root
      permissions to run or not, this parameter can have one of the following
      values: lfSystem, lfUser

    EXCEPTIONS: ELAWMICException, ELAPFileException, ELAGUIDException,
    ELAPermissionException

    NOTE: If this function fails to set the version GUID, none of the other
    functions will work.
*)

procedure SetVersionGUID(const VersionGUID: UnicodeString; Flags: TLAFlags);

(*
    PROCEDURE: SetProductKey()

    PURPOSE: Sets the product key required to activate the application.

    PARAMETERS:
    * ProductKey - a valid product key generated for the application.

    EXCEPTIONS: ELAGUIDException, ELAPKeyException
*)

procedure SetProductKey(const ProductKey: UnicodeString);

(*
    PROCEDURE: SetExtraActivationData()

    PURPOSE: Sets the extra data which you may want to fetch from the user
    at the time of activation.

    The extra data appears along with activation details of the product key
    in dashboard.

    PARAMETERS:
    * ExtraData - string of maximum length 256 characters with utf-8 encoding.

    EXCEPTIONS: ELAGUIDException, ELAEDataLenException

    NOTE: If the length of the string is more than 256, it is truncated to the
    allowed size.
*)

procedure SetExtraActivationData(const ExtraData: UnicodeString);

(*
    FUNCTION: ActivateProduct()

    PURPOSE: Activates your application by contacting the Cryptlex servers. It
    validates the key and returns with encrypted and digitally signed response
    which it stores and uses to activate your application.

    This function should be executed at the time of registration, ideally on
    a button click.

    RETURN CODES: lkOK, lkExpired, lkRevoked

    EXCEPTIONS: ELAFailException, ELAGUIDException, ELAPKeyException,
    ELAInetException, ELAVMException, ELATimeException, ELAActLimitException
*)

function ActivateProduct: TLAKeyStatus;

(*
    FUNCTION: DeactivateProduct()

    PURPOSE: Deactivates the application and frees up the correponding activation
    slot by contacting the Cryptlex servers.

    This function should be executed at the time of deregistration, ideally on
    a button click.

    RETURN CODES: lkOK, lkExpired, lkRevoked

    EXCEPTIONS: ELAFailException, ELAGUIDException, ELAPKeyException,
    ELAInetException, ELADeactLimitException
*)

function DeactivateProduct: TLAKeyStatus;

(*
    FUNCTION: ActivateProductOffline()

    PURPOSE: Activates your application using the offline activation response
    file.

    PARAMETERS:
    * FilePath - path of the offline activation response file

    RETURN CODES: lkOK, lkExpired

    EXCEPTIONS: ELAFailException, ELAGUIDException, ELAPKeyException,
    ELAOFileException, ELAVMException, ELATimeException
*)

function ActivateProductOffline(const FilePath: UnicodeString): TLAKeyStatus;

(*
    PROCEDURE: GenerateOfflineActivationRequest()

    PURPOSE: Generates the offline activation request needed for generating
    offline activation response in the dashboard.

    PARAMETERS:
    * FilePath - path of the file, needed to be created, for the offline request.

    EXCEPTIONS: ELAFailException, ELAGUIDException, ELAPKeyException
*)

procedure GenerateOfflineActivationRequest(const FilePath: UnicodeString);

(*
    PROCEDURE: GenerateOfflineDeactivationRequest()

    PURPOSE: Generates the offline deactivation request needed for deactivation of
    the product key in the dashboard and deactivates the application.

    A valid offline deactivation file confirms that the application has been successfully
    deactivated on the user's machine.

    PARAMETERS:
    * FilePath - path of the file, needed to be created, for the offline request.

    EXCEPTIONS: ELAExpiredError, ELARevokedError, ELAFailException, ELAGUIDException,
    ELAPKeyException
    
*)

procedure GenerateOfflineDeactivationRequest(const FilePath: UnicodeString);

(*
    FUNCTION: IsProductGenuine()

    PURPOSE: It verifies whether your app is genuinely activated or not. The verification is
    done locally by verifying the cryptographic digital signature fetched at the time of
    activation.

    After verifying locally, it schedules a server check in a separate thread on due dates.
    The default interval for server check is 100 days and this can be changed if required.

    In case server validation fails due to network error, it retries every 15 minutes. If it
    continues to fail for fixed number of days (grace period), the function returns LA_GP_OVER
    instead of LA_OK. The default length of grace period is 30 days and this can be changed if
    required.

    This function must be called on every start of your program to verify the activation
    of your app.

    RETURN CODES: lkOK, lkExpired, lkRevoked, lkGPOver

    EXCEPTIONS: ELAFailException, ELAGUIDException, ELAPKeyException

    NOTE: If application was activated offline using ActivateProductOffline() function, you
    may want to set grace period to 0 to ignore grace period.
*)

function IsProductGenuine: TLAKeyStatus;

(*
    FUNCTION: IsProductActivated()

    PURPOSE: It verifies whether your app is genuinely activated or not. The verification is
    done locally by verifying the cryptographic digital signature fetched at the time of
    activation.

    This is just an auxiliary function which you may use in some specific cases.

    RETURN CODES: lkOK, lkExpired, lkRevoked, lkGPOver

    EXCEPTIONS: ELAFailException, ELAGUIDException, ELAPKeyException
*)

function IsProductActivated: TLAKeyStatus;

(*
	FUNCTION: GetExtraActivationData()

	PURPOSE: Gets the value of the extra activation data.

	RESULT: The value of the extra activation data

	EXCEPTIONS: ELAGUIDException, ELABufferSizeException
*)

function GetExtraActivationData: UnicodeString;

(*
    FUNCTION: GetCustomLicenseField()

    PURPOSE: Get the value of the custom field associated with the product key.

    PARAMETERS:
    * FieldId - id of the custom field whose value you want to get

    RESULT: The value of the custom field associated with the product key

    EXCEPTIONS: ELACustomFieldIdException, ELAGUIDException, ELABufferSizeException
*)

function GetCustomLicenseField(const FieldId: UnicodeString): UnicodeString;

(*
    FUNCTION: GetProductKey()

    PURPOSE: Gets the stored product key which was used for activation.

    RESULT: The stored product key which was used for activation

    EXCEPTIONS: ELAPKeyException, ELAGUIDException, ELABufferSizeException
*)

function GetProductKey: UnicodeString;

(*
    FUNCTION: GetDaysLeftToExpiration()

    PURPOSE: Gets the number of remaining days after which the license expires.

    RESULT: The number of remaining days after which the license expires

    EXCEPTIONS: ELAFailException, ELAGUIDException
*)

function GetDaysLeftToExpiration: LongWord;

(*
    PROCEDURE: SetTrialKey()

    PURPOSE: Sets the trial key required to activate the verified trial.

    PARAMETERS:
    * TrialKey - trial key corresponding to the product version

    EXCEPTIONS: ELAGUIDException, ELATKeyException
*)

procedure SetTrialKey(const TrialKey: UnicodeString);

(*
    FUNCTION: ActivateTrial()

    PURPOSE: Starts the verified trial in your application by contacting the
    Cryptlex servers.

    This function should be executed when your application starts first time on
    the user's computer, ideally on a button click.

    RETURN CODES: lkOK, lkTExpired

    EXCEPTIONS: ELAFailException, ELAGUIDException, ELATKeyException,
    ELAInetException, ELAVMException, ELATimeException
*)

function ActivateTrial: TLAKeyStatus;

(*
    FUNCTION: IsTrialGenuine()

    PURPOSE: It verifies whether trial has started and is genuine or not. The
    verification is done locally by verifying the cryptographic digital signature
    fetched at the time of trial activation.

    This function must be called on every start of your program during the trial period.

    RETURN CODES: lkOK, lkTExpired

    EXCEPTIONS: ELAFailException, ELAGUIDException, ELATKeyException

    NOTE: The function is only meant for verified trials.
*)

function IsTrialGenuine: TLAKeyStatus;

(*
    FUNCTION: ExtendTrial()

    PURPOSE: Extends the trial using the trial extension key generated in the dashboard
    for the product version.

    PARAMETERS:
    * TrialExtensionKey - trial extension key generated for the product version

    RETURN CODES: lkOK, lkTExtExpired

    EXCEPTIONS: ELAFailException, ELAGUIDException, ELATExtKeyException,
    ELATKeyException, ELAInetException, ELAVMException, ELATimeException

    NOTE: The function is only meant for verified trials.
*)

function ExtendTrial(const TrialExtensionKey: UnicodeString): TLAKeyStatus;

(*
    FUNCTION: InitializeTrial()

    PURPOSE: Starts the unverified trial if trial has not started yet and if
    trial has already started, it verifies the validity of trial.

    This function must be called on every start of your program during the trial period.

    PARAMETERS:
    * TrialLength - trial length as set in the dashboard for the product version

    RETURN CODES: lkOK, lkTExpired

    EXCEPTIONS: ELAFailException, ELAGUIDException, ELATrialLenException

    NOTE: The function is only meant for unverified trials.
*)

function InitializeTrial(TrialLength: LongWord): TLAKeyStatus;

(*
    FUNCTION: GetTrialDaysLeft()

    PURPOSE: Gets the number of remaining trial days.

    If the trial has expired or has been tampered, daysLeft is set to 0 days.

    PARAMETERS:
    * TrialType - depending upon whether your application uses verified trial or not,
      this parameter can have one of the following values: ltVTrial, ltUVTrial

    RESULT: The number of remaining trial days

    EXCEPTIONS: ELAFailException, ELAGUIDException

    NOTE: The trial must be started by calling ActivateTrial() or  InitializeTrial() at least
    once in the past before calling this function.
*)

function GetTrialDaysLeft(TrialType: TLATrialType): LongWord;

(*
    PROCEDURE: SetDayIntervalForServerCheck()

    PURPOSE: Sets the interval for server checks done by IsProductGenuine() function.

    To disable sever check pass 0 as the day interval.

    PARAMETERS:
    * DayInterval - length of the interval in days

    EXCEPTIONS: ELAGUIDException
*)

procedure SetDayIntervalForServerCheck(DayInterval: LongWord);

(*
    PROCEDURE: SetGracePeriodForNetworkError()

    PURPOSE: Sets the grace period for failed re-validation requests sent
    by IsProductGenuine() function, caused due to network errors.

    It determines how long in days, should IsProductGenuine() function retry
    contacting CryptLex Servers, before returning LA_GP_OVER instead of LA_OK.

    To ignore grace period pass 0 as the grace period. This may be useful in
    case of offline activations.

    PARAMETERS:
    * GracePeriod - length of the grace period in days

    EXCEPTIONS: ELAGUIDException
*)

procedure SetGracePeriodForNetworkError(GracePeriod: LongWord);

(*
    FUNCTION: SetNetworkProxy()

    PURPOSE: Sets the network proxy to be used when contacting CryptLex servers.

    The proxy format should be: [protocol://][username:password@]machine[:port]

    Following are some examples of the valid proxy strings:
        - http://127.0.0.1:8000/
        - http://user:pass@127.0.0.1:8000/
        - socks5://127.0.0.1:8000/

    PARAMETERS:
    * Proxy - proxy string having correct proxy format

    EXCEPTIONS: ELAGUIDException, ELANetworkProxyException

    NOTE: Proxy settings of the computer are automatically detected. So, in most of the
    cases you don't need to care whether your user is behind a proxy server or not.
*)

procedure SetNetworkProxy(const Proxy: UnicodeString);

(*
    PROCEDURE: SetUserLock()

    PURPOSE: Enables the user locked licensing.

    It adds an additional user lock to the product key. Activations by different users in
    the same OS are treated as separate activations.

    PARAMETERS:
    * UserLock - boolean value to enable or disable the lock

    EXCEPTIONS: ELAGUIDException

    NOTE: User lock is disabled by default. You should enable it in case your application
    is used through remote desktop services where multiple users access individual sessions
    on a single machine instance at the same time.
*)

procedure SetUserLock(UserLock: Boolean);

(*
    PROCEDURE: SetCryptlexHost()

    PURPOSE: In case you are running Cryptlex on a private web server, you can set the
    host for your private server.

    PARAMETERS:
    * Host - the address of the private web server running Cryptlex

    EXCEPTIONS: ELAGUIDException, ELACryptlexHostException

    NOTE: This function should never be used unless you have opted for a private Cryptlex
    Server.
*)

procedure SetCryptlexHost(const Host: UnicodeString);

(*** Exceptions ***)

type
  {$M+}
  ELAError = class(Exception) // parent of all LexActivator exceptions
  protected
    FErrorCode: HRESULT;
  public
    // create exception of an appropriate class
    class function CreateByCode(ErrorCode: HRESULT): ELAError;

    // check for LA_OK, otherwise raise exception
    class procedure Check(ErrorCode: HRESULT); {$IFDEF DELPHI_HAS_INLINE} inline; {$ENDIF}

    // convert LA_OK into True, LA_FAIL into False, otherwise raise exception
    class function CheckOKFail(ErrorCode: HRESULT): Boolean; {$IFDEF DELPHI_HAS_INLINE} inline; {$ENDIF}

    // convert ErrorCode to TLAKeyStatus, otherwise raise exception
    // LA_FAIL will be converted to lkFail
    class function CheckKeyStatus(ErrorCode: HRESULT): TLAKeyStatus;

    property ErrorCode: HRESULT read FErrorCode;
  end;
  {$M-}

  // Exceptional situation for LA_E_* codes
  ELAException = class(ELAError);

  // Function returned a code not understandable by Delphi binding yet
  ELAUnknownErrorCodeException = class(ELAException)
  protected
    constructor Create(AErrorCode: HRESULT);
  end;

  // LA_FAIL
  ELAFailException = class(ELAException)
  public
    constructor Create; overload;
    constructor Create(const Msg: string); overload;
    procedure AfterConstruction; override;
  end;

  ELAKeyStatusError = class(ELAError)
  protected
    FKeyStatus: TLAKeyStatus;
  public
    // create exception of an appropriate class
    class function CreateByKeyStatus(KeyStatus: TLAKeyStatus): ELAError;
    property KeyStatus: TLAKeyStatus read FKeyStatus;
  end;

(*
    CODE: LA_EXPIRED

    MESSAGE: The product key has expired or system time has been tampered
    with. Ensure your date and time settings are correct.
*)

  ELAExpiredError = class(ELAKeyStatusError)
  public
    constructor Create;
  end;

(*
    CODE: LA_REVOKED

    MESSAGE: The product key has been revoked.
*)

  ELARevokedError = class(ELAKeyStatusError)
  public
    constructor Create;
  end;

(*
    CODE: LA_GP_OVER

    MESSAGE: The grace period is over.
*)

  ELAGPOverError = class(ELAKeyStatusError)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_INET

    MESSAGE: Failed to connect to the server due to network error.
*)

  ELAInetException = class(ELAException)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_PKEY

    MESSAGE: Invalid product key.
*)

  ELAPKeyException = class(ELAException)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_PFILE

    MESSAGE: Invalid or corrupted product file.
*)

  ELAPFileException = class(ELAException)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_FPATH

    MESSAGE: Invalid product file path.
*)

  ELAFPathException = class(ELAException)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_GUID

    MESSAGE: The version GUID doesn't match that of the product file.
*)

  ELAGUIDException = class(ELAException)
  public
    constructor Create;
  end;	// invalid version GUID

(*
    CODE: LA_E_OFILE

    MESSAGE: Invalid offline activation response file.
*)

  ELAOFileException = class(ELAException)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_PERMISSION

    MESSAGE: Insufficent system permissions. Occurs when LA_SYSTEM flag is used
    but application is not run with admin privileges.
*)

  ELAPermissionException = class(ELAException)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_EDATA_LEN

    MESSAGE: Extra activation data length is more than 256 characters.
*)

  ELAEDataLenException = class(ELAException)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_TKEY

    MESSAGE: The trial key doesn't match that of the product file.
*)

  ELATKeyException = class(ELAException)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_TIME

    MESSAGE: The system time has been tampered with. Ensure your date
    and time settings are correct.
*)

  ELATimeException = class(ELAException)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_VM

    MESSAGE: Application is being run inside a virtual machine / hypervisor,
    and activation has been disallowed in the VM.
    but
*)

  ELAVMException = class(ELAException)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_WMIC

    MESSAGE: Fingerprint couldn't be generated because Windows Management
    Instrumentation (WMI) service has been disabled. This error is specific
    to Windows only.
*)

  ELAWMICException = class(ELAException)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_TEXT_KEY

    MESSAGE: Invalid trial extension key.
*)

  ELATExtKeyException = class(ELAException)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_TRIAL_LEN

    MESSAGE: The trial length doesn't match that of the product file.
*)

  ELATrialLenException = class(ELAException)
  public
    constructor Create;
  end;

(*
    CODE: LA_T_EXPIRED

    MESSAGE: The trial has expired or system time has been tampered
    with. Ensure your date and time settings are correct.
*)

  ELATExpiredError = class(ELAKeyStatusError)
  public
    constructor Create;
  end;

(*
    CODE: LA_TEXT_EXPIRED

    MESSAGE: The trial extension key being used has already expired or system
    time has been tampered with. Ensure your date and time settings are correct.
*)

  ELATExtExpiredError = class(ELAKeyStatusError)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_BUFFER_SIZE

    MESSAGE: The buffer size was smaller than required.
*)

  ELABufferSizeException = class(ELAException)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_CUSTOM_FIELD_ID

    MESSAGE: Invalid custom field id.
*)

  ELACustomFieldIdException = class(ELAException)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_NET_PROXY

    MESSAGE: Invalid network proxy.
*)

  ELANetworkProxyException = class(ELAException)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_HOST_URL

    MESSAGE: Invalid Cryptlex host url.
*)

  ELACryptlexHostException = class(ELAException)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_DEACT_LIMIT

    MESSAGE: Deactivation limit for key has reached.
*)

  ELADeactLimitException = class(ELAException)
  public
    constructor Create;
  end;

(*
    CODE: LA_E_ACT_LIMIT

    MESSAGE: Activation limit for key has reached.
*)

  ELAActLimitException = class(ELAException)
  public
    constructor Create;
  end;


implementation

uses
{$IFDEF DELPHI_UNITS_SCOPED}
  System.TypInfo
{$ELSE}
  TypInfo
{$ENDIF}
  ;

const
  LexActivator_DLL = 'LexActivator.dll';

const
  LAFlagsToLongWord: array[TLAFlags] of LongWord = (1, 2);
  LATrialTypeToLongWord: array[TLATrialType] of LongWord = (1, 2);

function LAFlagsToString(Item: TLAFlags): string;
begin
  if (Item >= Low(TLAFlags)) and (Item <= High(TLAFlags)) then
  begin
    // sane value
    Result := GetEnumName(TypeInfo(TLAFlags), Integer(Item));
  end else begin
    // invalid value, should not appear
    Result := '$' + IntToHex(Integer(Item), 2 * SizeOf(Item));
  end;
end;

function LATrialTypeToString(Item: TLATrialType): string;
begin
  if (Item >= Low(TLATrialType)) and (Item <= High(TLATrialType)) then
  begin
    // sane value
    Result := GetEnumName(TypeInfo(TLATrialType), Integer(Item));
  end else begin
    // invalid value, should not appear
    Result := '$' + IntToHex(Integer(Item), 2 * SizeOf(Item));
  end;
end;

function LAKeyStatusToString(Item: TLAKeyStatus): string;
begin
  if (Item >= Low(TLAKeyStatus)) and (Item <= High(TLAKeyStatus)) then
  begin
    // sane value
    Result := GetEnumName(TypeInfo(TLAKeyStatus), Integer(Item));
  end else begin
    // invalid value, should not appear
    Result := '$' + IntToHex(Integer(Item), 2 * SizeOf(Item));
  end;
end;

(*** Return Codes ***)

const
  LA_OK            = HRESULT($00000000);

  LA_FAIL          = HRESULT($00000001);

(*
    CODE: LA_EXPIRED

    MESSAGE: The product key has expired or system time has been tampered
    with. Ensure your date and time settings are correct.
*)

  LA_EXPIRED	     = HRESULT($00000002);

(*
    CODE: LA_REVOKED

    MESSAGE: The product key has been revoked.
*)

  LA_REVOKED       = HRESULT($00000003);

(*
    CODE: LA_GP_OVER

    MESSAGE: The grace period is over.
*)

  LA_GP_OVER       = HRESULT($00000004);

(*
    CODE: LA_E_INET

    MESSAGE: Failed to connect to the server due to network error.
*)

  LA_E_INET        = HRESULT($00000005);

(*
    CODE: LA_E_PKEY

    MESSAGE: Invalid product key.
*)

  LA_E_PKEY        = HRESULT($00000006);

(*
    CODE: LA_E_PFILE

    MESSAGE: Invalid or corrupted product file.
*)

  LA_E_PFILE       = HRESULT($00000007);

(*
    CODE: LA_E_FPATH

    MESSAGE: Invalid product file path.
*)

  LA_E_FPATH       = HRESULT($00000008);

(*
    CODE: LA_E_GUID

    MESSAGE: The version GUID doesn't match that of the product file.
*)

  LA_E_GUID        = HRESULT($00000009);	// invalid version GUID

(*
    CODE: LA_E_OFILE

    MESSAGE: Invalid offline activation response file.
*)

  LA_E_OFILE       = HRESULT($0000000A);

(*
    CODE: LA_E_PERMISSION

    MESSAGE: Insufficent system permissions. Occurs when LA_SYSTEM flag is used
    but application is not run with admin privileges.
*)

  LA_E_PERMISSION  = HRESULT($0000000B);

(*
    CODE: LA_E_EDATA_LEN

    MESSAGE: Extra activation data length is more than 256 characters.
*)

  LA_E_EDATA_LEN   = HRESULT($0000000C);

(*
    CODE: LA_E_TKEY

    MESSAGE: The trial key doesn't match that of the product file.
*)

  LA_E_TKEY        = HRESULT($0000000D);

(*
    CODE: LA_E_TIME

    MESSAGE: The system time has been tampered with. Ensure your date
    and time settings are correct.
*)

  LA_E_TIME        = HRESULT($0000000E);

(*
    CODE: LA_E_VM

    MESSAGE: Application is being run inside a virtual machine / hypervisor,
    and activation has been disallowed in the VM.
    but
*)

  LA_E_VM          = HRESULT($0000000F);

(*
    CODE: LA_E_WMIC

    MESSAGE: Fingerprint couldn't be generated because Windows Management
    Instrumentation (WMI) service has been disabled. This error is specific
    to Windows only.
*)

  LA_E_WMIC        = HRESULT($00000010);

(*
    CODE: LA_E_TEXT_KEY

    MESSAGE: Invalid trial extension key.
*)

  LA_E_TEXT_KEY    = HRESULT($00000011);

(*
    CODE: LA_E_TRIAL_LEN

    MESSAGE: The trial length doesn't match that of the product file.
*)

  LA_E_TRIAL_LEN   = HRESULT($00000012);

(*
    CODE: LA_T_EXPIRED

    MESSAGE: The trial has expired or system time has been tampered
    with. Ensure your date and time settings are correct.
*)

  LA_T_EXPIRED     = HRESULT($00000013);

(*
    CODE: LA_TEXT_EXPIRED

    MESSAGE: The trial extension key being used has already expired or system
    time has been tampered with. Ensure your date and time settings are correct.
*)

  LA_TEXT_EXPIRED  = HRESULT($00000014);

(*
    CODE: LA_E_BUFFER_SIZE

    MESSAGE: The buffer size was smaller than required.
*)

  LA_E_BUFFER_SIZE = HRESULT($00000015);

(*
    CODE: LA_E_CUSTOM_FIELD_ID

    MESSAGE: Invalid custom field id.
*)

  LA_E_CUSTOM_FIELD_ID = HRESULT($00000016);

(*
    CODE: LA_E_NET_PROXY

    MESSAGE: Invalid custom field id.
*)

  LA_E_NET_PROXY = HRESULT($00000017);

(*
    CODE: LA_E_HOST_URL

    MESSAGE: Invalid custom field id.
*)

  LA_E_HOST_URL = HRESULT($00000018);

(*
    CODE: LA_E_DEACT_LIMIT

    MESSAGE: Deactivation limit for key has reached.
*)

  LA_E_DEACT_LIMIT = HRESULT($00000019);

(*
    CODE: LA_E_ACT_LIMIT

    MESSAGE: Activation limit for key has reached.
*)

  LA_E_ACT_LIMIT = HRESULT($0000001A);



function Thin_SetProductFile(const filePath: PWideChar): HRESULT; cdecl;
  external LexActivator_DLL name 'SetProductFile';

procedure SetProductFile(const FilePath: UnicodeString);
begin
  if not ELAError.CheckOKFail(Thin_SetProductFile(PWideChar(FilePath))) then
    raise
    ELAFailException.CreateFmt('Failed to set the path of the Product.dat ' +
      'file to %s', [FilePath]);
end;

function Thin_SetVersionGUID(versionGUID: PWideChar; flags: LongWord): HRESULT;
  cdecl; external LexActivator_DLL name 'SetVersionGUID';

procedure SetVersionGUID(const VersionGUID: UnicodeString; Flags: TLAFlags);
begin
  if not ELAError.CheckOKFail(Thin_SetVersionGUID(PWideChar(VersionGUID),
    LAFlagsToLongWord[Flags])) then
    raise ELAFailException.CreateFmt('Failed to set the version GUID of ' +
      'application to %s', [VersionGUID]);
end;

function Thin_SetProductKey(const productKey: PWideChar): HRESULT; cdecl;
  external LexActivator_DLL name 'SetProductKey';

procedure SetProductKey(const ProductKey: UnicodeString);
begin
  if not ELAError.CheckOKFail(Thin_SetProductKey(PWideChar(ProductKey))) then
    raise ELAFailException.CreateFmt('Failed to set the product key to %s',
      [ProductKey]);
end;

function Thin_SetExtraActivationData(extraData: PWideChar): HRESULT; cdecl;
  external LexActivator_DLL name 'SetExtraActivationData';

procedure SetExtraActivationData(const ExtraData: UnicodeString);
begin
  if not ELAError.CheckOKFail(Thin_SetExtraActivationData(PWideChar(ExtraData))) then
    raise ELAFailException.Create('Failed to set the extra data');
end;

function Thin_ActivateProduct: HRESULT; cdecl;
  external LexActivator_DLL name 'ActivateProduct';

function ActivateProduct: TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_ActivateProduct);
  if lkFail = Result then
    raise ELAFailException.Create('Failed to activate application');
end;

function Thin_DeactivateProduct: HRESULT; cdecl;
  external LexActivator_DLL name 'DeactivateProduct';

function DeactivateProduct: TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_DeactivateProduct);
  if lkFail = Result then
    raise ELAFailException.Create('Failed to deactivate application');
end;

function Thin_ActivateProductOffline(const filePath: PWideChar): HRESULT; cdecl;
  external LexActivator_DLL name 'ActivateProductOffline';

function ActivateProductOffline(const FilePath: UnicodeString): TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_ActivateProductOffline(PWideChar(FilePath)));
  if lkFail = Result then
    raise ELAFailException.CreateFmt('Failed to activate application using ' +
      'the offline activation response file %s', [FilePath]);
end;

function Thin_GenerateOfflineActivationRequest(const filePath: PWideChar): HRESULT; cdecl;
  external LexActivator_DLL name 'GenerateOfflineActivationRequest';

procedure GenerateOfflineActivationRequest(const FilePath: UnicodeString);
begin
  if not ELAError.CheckOKFail(Thin_GenerateOfflineActivationRequest(PWideChar(FilePath))) then
    raise ELAFailException.Create('Failed to generate the offline activation request');
end;

function Thin_GenerateOfflineDeactivationRequest(const filePath: PWideChar): HRESULT; cdecl;
  external LexActivator_DLL name 'GenerateOfflineDeactivationRequest';

procedure GenerateOfflineDeactivationRequest(const FilePath: UnicodeString);
begin
  if not ELAError.CheckOKFail(Thin_GenerateOfflineDeactivationRequest(PWideChar(FilePath))) then
    raise ELAFailException.Create('Failed to generate the offline deactivation request');
end;

function Thin_IsProductGenuine: HRESULT; cdecl;
  external LexActivator_DLL name 'IsProductGenuine';

function IsProductGenuine: TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_IsProductGenuine);
  if lkFail = Result then
    raise ELAFailException.Create('Failed to verify whether app is genuinely ' +
      'activated or not');
end;

function Thin_IsProductActivated: HRESULT; cdecl;
  external LexActivator_DLL name 'IsProductActivated';

function IsProductActivated: TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_IsProductActivated);
  if lkFail = Result then
    raise ELAFailException.Create('Failed to verify whether app is genuinely ' +
      'activated or not');
end;

function Thin_GetExtraActivationData(out extraData; length: LongWord): HRESULT; cdecl;
  external LexActivator_DLL name 'GetExtraActivationData';

function GetExtraActivationData: UnicodeString;
var
  ErrorCode: HRESULT;
  function Try256(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: array[0 .. 255] of WideChar;
  begin
    ErrorCode := Thin_GetExtraActivationData(Buffer, Length(Buffer));
    Result := ErrorCode <> LA_E_BUFFER_SIZE;
    if ErrorCode = LA_OK then OuterResult := Buffer;
  end;
  function Try1024(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: array[0 .. 1023] of WideChar;
  begin
    ErrorCode := Thin_GetExtraActivationData(Buffer, Length(Buffer));
    Result := ErrorCode <> LA_E_BUFFER_SIZE;
    if ErrorCode = LA_OK then OuterResult := Buffer;
  end;
  function Try4096(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: array[0 .. 4095] of WideChar;
  begin
    ErrorCode := Thin_GetExtraActivationData(Buffer, Length(Buffer));
    Result := ErrorCode <> LA_E_BUFFER_SIZE;
    if ErrorCode = LA_OK then OuterResult := Buffer;
  end;
begin
  if not Try256(Result) then if not Try1024(Result) then Try4096(Result);
  if not ELAError.CheckOKFail(ErrorCode) then
    raise ELAFailException.Create('Failed to get the value of the extra activation data');
end;

function Thin_GetCustomLicenseField(const fieldId: PWideChar; out fieldValue;
  length: LongWord): HRESULT; cdecl;
  external LexActivator_DLL name 'GetCustomLicenseField';

function GetCustomLicenseField(const FieldId: UnicodeString): UnicodeString;
var
  Arg1: PWideChar;
  ErrorCode: HRESULT;
  function Try256(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: array[0 .. 255] of WideChar;
  begin
    ErrorCode := Thin_GetCustomLicenseField(Arg1, Buffer, Length(Buffer));
    Result := ErrorCode <> LA_E_BUFFER_SIZE;
    if ErrorCode = LA_OK then OuterResult := Buffer;
  end;
  function Try1024(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: array[0 .. 1023] of WideChar;
  begin
    ErrorCode := Thin_GetCustomLicenseField(Arg1, Buffer, Length(Buffer));
    Result := ErrorCode <> LA_E_BUFFER_SIZE;
    if ErrorCode = LA_OK then OuterResult := Buffer;
  end;
  function Try4096(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: array[0 .. 4095] of WideChar;
  begin
    ErrorCode := Thin_GetCustomLicenseField(Arg1, Buffer, Length(Buffer));
    Result := ErrorCode <> LA_E_BUFFER_SIZE;
    if ErrorCode = LA_OK then OuterResult := Buffer;
  end;
begin
  Arg1 := PWideChar(FieldId);
  if not Try256(Result) then if not Try1024(Result) then Try4096(Result);
  if not ELAError.CheckOKFail(ErrorCode) then
    raise ELAFailException.CreateFmt('Failed to get the value of the custom '+
      'field %s associated with the product key', [FieldId]);
end;

function Thin_GetProductKey(out productKey; length: LongWord): HRESULT; cdecl;
  external LexActivator_DLL name 'GetProductKey';

function GetProductKey: UnicodeString;
var
  ErrorCode: HRESULT;
  function Try256(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: array[0 .. 255] of WideChar;
  begin
    ErrorCode := Thin_GetProductKey(Buffer, Length(Buffer));
    Result := ErrorCode <> LA_E_BUFFER_SIZE;
    if ErrorCode = LA_OK then OuterResult := Buffer;
  end;
  function Try1024(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: array[0 .. 1023] of WideChar;
  begin
    ErrorCode := Thin_GetProductKey(Buffer, Length(Buffer));
    Result := ErrorCode <> LA_E_BUFFER_SIZE;
    if ErrorCode = LA_OK then OuterResult := Buffer;
  end;
  function Try4096(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: array[0 .. 4095] of WideChar;
  begin
    ErrorCode := Thin_GetProductKey(Buffer, Length(Buffer));
    Result := ErrorCode <> LA_E_BUFFER_SIZE;
    if ErrorCode = LA_OK then OuterResult := Buffer;
  end;
begin
  if not Try256(Result) then if not Try1024(Result) then Try4096(Result);
  if not ELAError.CheckOKFail(ErrorCode) then
    raise ELAFailException.Create('Failed to get the stored product key');
end;

function Thin_GetDaysLeftToExpiration(out daysLeft: LongWord): HRESULT; cdecl;
  external LexActivator_DLL name 'GetDaysLeftToExpiration';

function GetDaysLeftToExpiration: LongWord;
begin
  if not ELAError.CheckOKFail(Thin_GetDaysLeftToExpiration(Result)) then
    raise ELAFailException.Create('Failed to get the number of remaining ' +
      'days after which the license expires');
end;

function Thin_SetTrialKey(const trialKey: PWideChar): HRESULT; cdecl;
  external LexActivator_DLL name 'SetTrialKey';

procedure SetTrialKey(const TrialKey: UnicodeString);
begin
  if not ELAError.CheckOKFail(Thin_SetTrialKey(PWideChar(TrialKey))) then
    raise ELAFailException.CreateFmt('Failed to set the trial key %s',
      [TrialKey]);
end;

function Thin_ActivateTrial: HRESULT; cdecl;
  external LexActivator_DLL name 'ActivateTrial';

function ActivateTrial: TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_ActivateTrial);
  if lkFail = Result then
    raise ELAFailException.Create('Failed to start the verified trial');
end;

function Thin_IsTrialGenuine: HRESULT; cdecl;
  external LexActivator_DLL name 'IsTrialGenuine';

function IsTrialGenuine: TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_IsTrialGenuine);
  if lkFail = Result then
    raise ELAFailException.Create('Failed to verify whether trial has ' +
      'started and is genuine or not');
end;

function Thin_ExtendTrial(const trialExtensionKey: PWideChar): HRESULT; cdecl;
  external LexActivator_DLL name 'ExtendTrial';

function ExtendTrial(const TrialExtensionKey: UnicodeString): TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_ExtendTrial(PWideChar(TrialExtensionKey)));
  if lkFail = Result then
    raise ELAFailException.CreateFmt('Failed to extend the trial using the ' +
      'trial extension key %s', [TrialExtensionKey]);
end;

function Thin_InitializeTrial(trialLength: LongWord): HRESULT; cdecl;
  external LexActivator_DLL name 'InitializeTrial';

function InitializeTrial(TrialLength: LongWord): TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_InitializeTrial(TrialLength));
  if lkFail = Result then
    raise ELAFailException.Create('Failed to either start the unverified ' +
      'trial or verify the validity of trial');
end;

function Thin_GetTrialDaysLeft(out daysLeft : LongWord; trialType: LongWord): HRESULT; cdecl;
  external LexActivator_DLL name 'GetTrialDaysLeft';

function GetTrialDaysLeft(TrialType: TLATrialType): LongWord;
begin
  if not ELAError.CheckOKFail(Thin_GetTrialDaysLeft(Result, LATrialTypeToLongWord[TrialType])) then
    raise ELAFailException.Create('Failed to get the number of remaining trial days');
end;

function Thin_SetDayIntervalForServerCheck(dayInterval: LongWord): HRESULT; cdecl;
  external LexActivator_DLL name 'SetDayIntervalForServerCheck';

procedure SetDayIntervalForServerCheck(DayInterval: LongWord);
begin
  if not ELAError.CheckOKFail(Thin_SetDayIntervalForServerCheck(DayInterval)) then
    raise ELAFailException.Create('Failed to set the interval for server checks');
end;

function Thin_SetGracePeriodForNetworkError(gracePeriod: LongWord): HRESULT; cdecl;
  external LexActivator_DLL name 'SetGracePeriodForNetworkError';

procedure SetGracePeriodForNetworkError(GracePeriod: LongWord);
begin
  if not ELAError.CheckOKFail(Thin_SetGracePeriodForNetworkError(GracePeriod)) then
    raise ELAFailException.Create('Failed to set the grace period for failed ' +
      're-validation requests');
end;

function Thin_SetNetworkProxy(const proxy: PWideChar): HRESULT; cdecl;
  external LexActivator_DLL name 'SetNetworkProxy';

procedure SetNetworkProxy(const Proxy: UnicodeString);
begin
  if not ELAError.CheckOKFail(Thin_SetNetworkProxy(PWideChar(Proxy))) then
    raise ELAFailException.Create('Failed to set the network proxy');
end;

function Thin_SetUserLock(userLock: Boolean): HRESULT; cdecl;
  external LexActivator_DLL name 'SetUserLock';

procedure SetUserLock(UserLock: Boolean);
begin
  if not ELAError.CheckOKFail(Thin_SetUserLock(UserLock)) then
    raise ELAFailException.Create('Failed to set the user lock');
end;

function Thin_SetCryptlexHost(const host: PWideChar): HRESULT; cdecl;
  external LexActivator_DLL name 'SetCryptlexHost';

procedure SetCryptlexHost(const Host: UnicodeString);
begin
  if not ELAError.CheckOKFail(Thin_SetCryptlexHost(PWideChar(Host))) then
    raise ELAFailException.Create('Failed to set the host for server');
end;

class function ELAError.CreateByCode(ErrorCode: HRESULT): ELAError;
begin
  case ErrorCode of
    LA_OK: Result := nil;
    LA_FAIL: Result := ELAFailException.Create;
    LA_EXPIRED: Result := ELAExpiredError.Create;
    LA_REVOKED: Result := ELARevokedError.Create;
    LA_GP_OVER: Result := ELAGPOverError.Create;
    LA_E_INET: Result := ELAInetException.Create;
    LA_E_PKEY: Result := ELAPKeyException.Create;
    LA_E_PFILE: Result := ELAPFileException.Create;
    LA_E_FPATH: Result := ELAFPathException.Create;
    LA_E_GUID: Result := ELAGUIDException.Create;
    LA_E_OFILE: Result := ELAOFileException.Create;
    LA_E_PERMISSION: Result := ELAPermissionException.Create;
    LA_E_EDATA_LEN: Result := ELAEDataLenException.Create;
    LA_E_TKEY: Result := ELATKeyException.Create;
    LA_E_TIME: Result := ELATimeException.Create;
    LA_E_VM: Result := ELAVMException.Create;
    LA_E_WMIC: Result := ELAWMICException.Create;
    LA_E_TEXT_KEY: Result := ELATExtKeyException.Create;
    LA_E_TRIAL_LEN: Result := ELATrialLenException.Create;
    LA_T_EXPIRED: Result := ELATExpiredError.Create;
    LA_TEXT_EXPIRED: Result := ELATExtExpiredError.Create;
    LA_E_BUFFER_SIZE: Result := ELABufferSizeException.Create;
	  LA_E_CUSTOM_FIELD_ID: Result := ELACustomFieldIdException.Create;
    LA_E_NET_PROXY: Result := ELANetworkProxyException.Create;
    LA_E_HOST_URL: Result := ELACryptlexHostException.Create;
    LA_E_DEACT_LIMIT: Result := ELADeactLimitException.Create;
    LA_E_ACT_LIMIT: Result := ELAActLimitException.Create;
    
  else
    Result := ELAUnknownErrorCodeException.Create(ErrorCode);
  end;
end;

// check for LA_OK, otherwise raise exception
class procedure ELAError.Check(ErrorCode: HRESULT);
begin
  // E2441 Inline function declared in interface section must not use local
  // symbol 'LA_OK'.
  if ErrorCode <> 0 {LA_OK} then
    raise CreateByCode(ErrorCode);
end;

class function ELAError.CheckOKFail(ErrorCode: HRESULT): Boolean;
begin
  // E2441 Inline function declared in interface section must not use local
  // symbol 'LA_OK'
  case ErrorCode of
    0 {LA_OK}: Result := True;
    1 {LA_FAIL}: Result := False;
  else
    raise CreateByCode(ErrorCode);
  end;
end;

class function ELAError.CheckKeyStatus(ErrorCode: HRESULT): TLAKeyStatus;
begin
  case ErrorCode of
    LA_OK: Result := lkOK;
    LA_EXPIRED: Result := lkExpired;
    LA_REVOKED: Result := lkRevoked;
    LA_GP_OVER: Result := lkGPOver;
    LA_T_EXPIRED: Result := lkTExpired;
    LA_TEXT_EXPIRED: Result := lkTExtExpired;
    LA_FAIL: Result := lkFail;
  else
    raise CreateByCode(ErrorCode);
  end;
end;

constructor ELAUnknownErrorCodeException.Create(AErrorCode: HRESULT);
begin
  inherited CreateFmt('LexActivator error %.8x', [AErrorCode]);
  FErrorCode := AErrorCode;
end;

constructor ELAFailException.Create;
begin
  inherited Create('Failed');
end;

constructor ELAFailException.Create(const Msg: string);
begin
  inherited;
end;

procedure ELAFailException.AfterConstruction;
begin
  FErrorCode := LA_FAIL;
end;

class function ELAKeyStatusError.CreateByKeyStatus(KeyStatus: TLAKeyStatus): ELAError;
begin
  case KeyStatus of
    lkOK: Result := nil;
    lkExpired: Result := ELAExpiredError.Create;
    lkRevoked: Result := ELARevokedError.Create;
    lkGPOver: Result := ELAGPOverError.Create;
    lkTExpired: Result := ELATExpiredError.Create;
    lkTExtExpired: Result := ELATExtExpiredError.Create;
    lkFail: Result := ELAFailException.Create;
  else
    if (KeyStatus >= Low(TLAKeyStatus)) and (KeyStatus <= High(TLAKeyStatus)) then
    begin
      // forgot to update list?
      Result := ELAKeyStatusError.Create('Key state is ' +
        GetEnumName(TypeInfo(TLAKeyStatus), Integer(KeyStatus)));
      ELAKeyStatusError(Result).FKeyStatus := KeyStatus;
    end else begin
      // invalid value, should not appear
      Result := ELAKeyStatusError.CreateFmt('Key state is $%.8x',
        [Integer(KeyStatus)]);
      ELAKeyStatusError(Result).FKeyStatus := KeyStatus;
    end;
  end;
end;

constructor ELAExpiredError.Create;
begin
  inherited Create('The product key has expired or system time has been ' +
    'tampered with. Ensure your date and time settings are correct');
  FErrorCode := LA_EXPIRED;
  FKeyStatus := lkExpired;
end;

constructor ELARevokedError.Create;
begin
  inherited Create('The product key has been revoked');
  FErrorCode := LA_REVOKED;
  FKeyStatus := lkRevoked;
end;

constructor ELAGPOverError.Create;
begin
  inherited Create('The grace period is over');
  FErrorCode := LA_GP_OVER;
  FKeyStatus := lkGPOver;
end;

constructor ELAInetException.Create;
begin
  inherited Create('Failed to connect to the server due to network error');
  FErrorCode := LA_E_INET;
end;

constructor ELAPKeyException.Create;
begin
  inherited Create('Invalid product key');
  FErrorCode := LA_E_PKEY;
end;

constructor ELAPFileException.Create;
begin
  inherited Create('Invalid or corrupted product file');
  FErrorCode := LA_E_PFILE;
end;

constructor ELAFPathException.Create;
begin
  inherited Create('Invalid product file path');
  FErrorCode := LA_E_FPATH;
end;

constructor ELAGUIDException.Create;
begin
  inherited Create('The version GUID doesn''t match that of the product file');
  FErrorCode := LA_E_GUID;
end;

constructor ELAOFileException.Create;
begin
  inherited Create('Invalid offline activation response file');
  FErrorCode := LA_E_OFILE;
end;

constructor ELAPermissionException.Create;
begin
  inherited Create('Insufficent system permissions. Occurs when LA_SYSTEM ' +
    'flag is used but application is not run with admin privileges');
  FErrorCode := LA_E_PERMISSION;
end;

constructor ELAEDataLenException.Create;
begin
  inherited Create('Extra activation data length is more than 256 characters');
  FErrorCode := LA_E_EDATA_LEN;
end;

constructor ELATKeyException.Create;
begin
  inherited Create('The trial key doesn''t match that of the product file');
  FErrorCode := LA_E_TKEY;
end;

constructor ELATimeException.Create;
begin
  inherited Create('The system time has been tampered with. Ensure your date ' +
    'and time settings are correct');
  FErrorCode := LA_E_TIME;
end;

constructor ELAVMException.Create;
begin
  inherited Create('Application is being run inside a virtual machine / ' +
    'hypervisor, and activation has been disallowed in the VM');
  FErrorCode := LA_E_VM;
end;

constructor ELAWMICException.Create;
begin
  inherited Create('Fingerprint couldn''t be generated because Windows ' +
    'Management Instrumentation (WMI) service has been disabled');
  FErrorCode := LA_E_WMIC;
end;

constructor ELATExtKeyException.Create;
begin
  inherited Create('Invalid trial extension key');
  FErrorCode := LA_E_TEXT_KEY;
end;

constructor ELATrialLenException.Create;
begin
  inherited Create('The trial length doesn''t match that of the product file');
  FErrorCode := LA_E_TRIAL_LEN;
end;

constructor ELATExpiredError.Create;
begin
  inherited Create('The trial has expired or system time has been tampered ' +
    'with. Ensure your date and time settings are correct');
  FErrorCode := LA_T_EXPIRED;
  FKeyStatus := lkTExpired;
end;

constructor ELATExtExpiredError.Create;
begin
  inherited Create('The trial extension key being used has already expired ' +
    'or system time has been tampered with. Ensure your date and time ' +
    'settings are correct');
  FErrorCode := LA_TEXT_EXPIRED;
  FKeyStatus := lkTExtExpired;
end;

constructor ELABufferSizeException.Create;
begin
  inherited Create('The buffer size was smaller than required');
  FErrorCode := LA_E_BUFFER_SIZE;
end;

constructor ELACustomFieldIdException.Create;
begin
  inherited Create('Invalid custom field id');
  FErrorCode := LA_E_CUSTOM_FIELD_ID;
end;

constructor ELANetworkProxyException.Create;
begin
  inherited Create('Invalid network proxy');
  FErrorCode := LA_E_NET_PROXY;
end;

constructor ELACryptlexHostException.Create;
begin
  inherited Create('Invalid Cryptlex host url');
  FErrorCode := LA_E_HOST_URL;
end;

constructor ELADeactLimitException.Create;
begin
  inherited Create('Deactivation limit for key has reached');
  FErrorCode := LA_E_DEACT_LIMIT;
end;

constructor ELAActLimitException.Create;
begin
  inherited Create('Activation limit for key has reached');
  FErrorCode := LA_E_ACT_LIMIT;
end;

end.
