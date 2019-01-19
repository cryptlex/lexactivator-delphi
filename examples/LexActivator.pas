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
  {$DEFINE DELPHI_HAS_CLOSURES}
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
  TLAKeyStatus = (lkOK, lkExpired, lkSuspended, lkGracePeriodOver,
    lkTrialExpired, lkLocalTrialExpired, lkFail,
    lkException // for callback
    );

function LAFlagsToString(Item: TLAFlags): string;
function LAKeyStatusToString(Item: TLAKeyStatus): string;    

(*
    PROCEDURE: SetProductFile()

    PURPOSE: Sets the absolute path of the Product.dat file.

    This function must be called on every start of your program
    before any other functions are called.

    PARAMETERS:
    * FilePath - absolute path of the product file (Product.dat)

    EXCEPTIONS: ELAFilePathException, ELAProductFileException

    NOTE: If this function fails to set the path of product file, none of the
    other functions will work.
*)

procedure SetProductFile(const FilePath: UnicodeString);

(*
    PROCEDURE: SetProductData()

    PURPOSE: Embeds the Product.dat file in the application.

    It can be used instead of SetProductFile() in case you want
    to embed the Product.dat file in your application.

    This function must be called on every start of your program
    before any other functions are called.

    PARAMETERS:
    * ProductData - content of the Product.dat file

    EXCEPTIONS: ELAProductDataException

    NOTE: If this function fails to set the product data, none of the
    other functions will work.
*)

procedure SetProductData(const ProductData: UnicodeString);

(*
    PROCEDURE: SetProductId()

    PURPOSE: Sets the product id of your application.

    This function must be called on every start of your program before
    any other functions are called, with the exception of SetProductFile()
    or SetProductData() function.

    PARAMETERS:
    * ProductId - the unique product id of your application as mentioned
      on the product page in the dashboard.

    * Flags - depending upon whether your application requires admin/root
      permissions to run or not, this parameter can have one of the following
      values: lfSystem, lfUser

    EXCEPTIONS: ELAWMICException, ELAProductFileException,
    ELAProductDataException, ELAProductIdException,
    ELASystemPermissionException

    NOTE: If this function fails to set the product id, none of the other
    functions will work.
*)

procedure SetProductId(const ProductId: UnicodeString; Flags: TLAFlags);

(*
    PROCEDURE: SetLicenseKey()

    PURPOSE: Sets the license key required to activate the license.

    PARAMETERS:
    * LicenseKey - a valid license key.

    EXCEPTIONS: ELAProductIdException, ELALicenseKeyException
*)

procedure SetLicenseKey(const LicenseKey: UnicodeString);

(*
    PROCEDURE: SetLicenseCallback()

    PURPOSE: Sets server sync callback function.

    Whenever the server sync occurs in a separate thread, and server returns the response,
    license callback function gets invoked with the following status codes:
    LA_OK, LA_EXPIRED, LA_SUSPENDED,
    LA_E_REVOKED, LA_E_ACTIVATION_NOT_FOUND, LA_E_MACHINE_FINGERPRINT
    LA_E_COUNTRY, LA_E_INET, LA_E_SERVER, LA_E_RATE_LIMIT, LA_E_IP

    PARAMETERS:
    * Callback - name of the callback procedure, method or closure
    * Synchronized - whether callback must be invoked in main (GUI) thread
    using TThread.Synchronize
    Usually True for GUI applications and handlers like TForm1.OnLexActivator
    Must be False if there is no GUI message loop, like in console applications,
    but then another thread synchronization measures must be used.

    EXCEPTIONS: ELAProductIdException, ELALicenseKeyException
*)

type
  TLAProcedureCallback = procedure(const Error: Exception; Status: TLAKeyStatus);
  TLAMethodCallback = procedure(const Error: Exception; Status: TLAKeyStatus) of object;
  {$IFDEF DELPHI_HAS_CLOSURES}
  TLAClosureCallback = reference to procedure(const Error: Exception; Status: TLAKeyStatus);
  {$ENDIF}

procedure SetLicenseCallback(Callback: TLAProcedureCallback; Synchronized: Boolean); overload;
procedure SetLicenseCallback(Callback: TLAMethodCallback; Synchronized: Boolean); overload;
{$IFDEF DELPHI_HAS_CLOSURES}
procedure SetLicenseCallback(Callback: TLAClosureCallback; Synchronized: Boolean); overload;
{$ENDIF}

(*
    PROCEDURE: SetLicenseCallback()

    PURPOSE: Resets server sync callback function.

    EXCEPTIONS: ELAProductIdException, ELALicenseKeyException
*)

procedure ResetLicenseCallback;

(*
    PROCEDURE: SetActivationMetadata()

    PURPOSE: Sets the activation metadata.

    The  metadata appears along with the activation details of the license
    in dashboard.

    PARAMETERS:
    * Key - string of maximum length 256 characters with utf-8 encoding.
    * Value - string of maximum length 256 characters with utf-8 encoding.

    EXCEPTIONS: ELAProductIdException, ELALicenseKeyException,
    ELAMetadataKeyLengthException, ELAMetadataValueLengthException,
    ELAActivationMetadataLimitException
*)

procedure SetActivationMetadata(const Key, Value: UnicodeString);

(*
    PROCEDURE: SetTrialActivationMetadata()

    PURPOSE: Sets the trial activation metadata.

    The  metadata appears along with the trial activation details of the product
    in dashboard.

    PARAMETERS:
    * Key - string of maximum length 256 characters with utf-8 encoding.
    * Value - string of maximum length 256 characters with utf-8 encoding.

    EXCEPTIONS: ELAProductIdException, ELAMetadataKeyLengthException,
    ELAMetadataValueLengthException, ELATrialActivationMetadataLimitException
*)

procedure SetTrialActivationMetadata(const Key, Value: UnicodeString);

(*
    PROCEDURE: SetAppVersion()

    PURPOSE: Sets the current app version of your application.

    The app version appears along with the activation details in dashboard. It
    is also used to generate app analytics.

    PARAMETERS:
    * AppVersion - string of maximum length 256 characters with utf-8 encoding.

    EXCEPTIONS: ELAProductIdException, ELAAppVersionLengthException
*)

procedure SetAppVersion(const AppVersion: UnicodeString);

(*
    PROCEDURE: SetNetworkProxy()

    PURPOSE: Sets the network proxy to be used when contacting Cryptlex servers.

    The proxy format should be: [protocol://][username:password@]machine[:port]

    Following are some examples of the valid proxy strings:
        - http://127.0.0.1:8000/
        - http://user:pass@127.0.0.1:8000/
        - socks5://127.0.0.1:8000/

    PARAMETERS:
    * Proxy - proxy string having correct proxy format

    EXCEPTIONS: ELAProductIdException, ELANetProxyException

    NOTE: Proxy settings of the computer are automatically detected. So, in most of the
    cases you don't need to care whether your user is behind a proxy server or not.
*)

procedure SetNetworkProxy(const Proxy: UnicodeString);

(*
    FUNCTION: GetProductMetadata()

    PURPOSE: Gets the product metadata as set in the dashboard.

    This is available for trial as well as license activations.

    PARAMETERS:
    * Key - key to retrieve the value

    RESULT: Product metadata as set in the dashboard

    EXCEPTIONS: ELAProductIdException, ELAMetadataKeyNotFoundException,
    ELABufferSizeException
*)

function GetProductMetadata(const Key: UnicodeString): UnicodeString;

(*
    FUNCTION: GetLicenseMetadata()

    PURPOSE: Gets the license metadata as set in the dashboard.

    PARAMETERS:
    * Key - key to retrieve the value

    RESULT: License metadata as set in the dashboard

    EXCEPTIONS: ELAProductIdException, ELAMetadataKeyNotFoundException,
    ELABufferSizeException
*)

function GetLicenseMetadata(const Key: UnicodeString): UnicodeString;

(*
    FUNCTION: GetLicenseKey()

    PURPOSE: Gets the license key used for activation.

    RESULT: License key used for activation

    EXCEPTIONS: ELAFailException, ELAProductIdException,
    ELABufferSizeException
*)

function GetLicenseKey: UnicodeString;

(*
    FUNCTION: GetLicenseExpiryDate()

    PURPOSE: Gets the license expiry date timestamp.

    RESULT: License expiry date timestamp

    EXCEPTIONS: ELAFailException, ELAProductIdException,
    ELATimeException, ELATimeModifiedException
*)

function GetLicenseExpiryDate: TDateTime;

(*
    FUNCTION: GetLicenseUserEmail()

    PURPOSE: Gets the email associated with license user.

    RESULT: Email associated with license user

    EXCEPTIONS: ELAFailException, ELAProductIdException, ELATimeException,
    ELATimeModifiedException, ELABufferSizeException
*)

function GetLicenseUserEmail: UnicodeString;

(*
    FUNCTION: GetLicenseUserName()

    PURPOSE: Gets the name associated with license user.

    RESULT: Name associated with license user

    EXCEPTIONS: ELAFailException, ELAProductIdException, ELATimeException,
    ELATimeModifiedException, ELABufferSizeException
*)

function GetLicenseUserName: UnicodeString;

(*
    FUNCTION: GetLicenseType()

    PURPOSE: Gets the license type (node-locked or hosted-floating).

    RESULT: License type

    EXCEPTIONS: ELAFailException, ELAProductIdException, ELATimeException,
    ELATimeModifiedException, ELABufferSizeException
*)

function GetLicenseType: UnicodeString;

(*
    FUNCTION: GetActivationMetadata()

    PURPOSE: Gets the activation metadata.

    PARAMETERS:
    * Key - key to retrieve the value

    RESULT: activation metadata

    EXCEPTIONS: ELAProductIdException, ELAMetadataKeyNotFoundException,
    ELABufferSizeException
*)

function GetActivationMetadata(const Key: UnicodeString): UnicodeString;

(*
    FUNCTION: GetTrialActivationMetadata()

    PURPOSE: Gets the trial activation metadata.

    PARAMETERS:
    * Key - key to retrieve the value

    RESULT: Trial activation metadata

    EXCEPTIONS: ELAProductIdException, ELAMetadataKeyNotFoundException,
    ELABufferSizeException
*)

function GetTrialActivationMetadata(const Key: UnicodeString): UnicodeString;

(*
    FUNCTION: GetTrialExpiryDate()

    PURPOSE: Gets the trial expiry date timestamp.

    RESULT: Trial expiry date timestamp

    EXCEPTIONS: ELAFailException, ELAProductIdException, ELATimeException,
    ELATimeModifiedException
*)

function GetTrialExpiryDate: TDateTime;

(*
    FUNCTION: GetTrialId()

    PURPOSE: Gets the trial activation id. Used in case of trial extension.

    RESULT: Trial activation id

    EXCEPTIONS: ELAFailException, ELAProductIdException, ELATimeException,
    ELATimeModifiedException, ELABufferSizeException
*)

function GetTrialId: UnicodeString;

(*
    FUNCTION: GetLocalTrialExpiryDate()

    PURPOSE: Gets the trial expiry date timestamp.

    RESULT: Trial expiry date timestamp

    EXCEPTIONS: ELAFailException, ELAProductIdException,
    ELATimeModifiedException
*)

function GetLocalTrialExpiryDate: TDateTime;

(*
    FUNCTION: ActivateLicense()

    PURPOSE: Activates the license by contacting the Cryptlex servers. It
    validates the key and returns with encrypted and digitally signed token
    which it stores and uses to activate your application.

    This function should be executed at the time of registration, ideally on
    a button click.

    RETURN CODES: lkOK, LkExpired, lkSuspended, lkRevoked

    EXCEPTIONS: ELAFailException, ELAProductIdException, ELAInetException,
    ELAVMException, ELATimeException, ELAActivationLimitException,
    ELAServerException, ELAClientException, ELALicenseTypeException,
    ELACountryException, ELAIPException, ELARateLimitException,
    ELALicenseKeyException
*)

function ActivateLicense: TLAKeyStatus;

(*
    FUNCTION: ActivateLicenseOffline()

    PURPOSE: Activates your licenses using the offline activation response file.

    PARAMETERS:
    * FilePath - path of the offline activation response file.

    RETURN CODES: lkOK, lkExpired

    EXCEPTIONS: ELAFailException, ELAProductIdException, ELALicenseKeyException,
    ELAOfflineResponseFileException, ELAVMException, ELATimeException,
    ELAFilePathException, ELAOfflineResponseFileExpiredException
*)

function ActivateLicenseOffline(const FilePath: UnicodeString): TLAKeyStatus;

(*
    PROCEDURE: GenerateOfflineActivationRequest()

    PURPOSE: Generates the offline activation request needed for generating
    offline activation response in the dashboard.

    PARAMETERS:
    * FilePath - path of the file for the offline request.

    EXCEPTION: ELAFailException, ELAProductIdException, ELALicenseKeyException,
    ELAFilePermissionException
*)

procedure GenerateOfflineActivationRequest(const FilePath: UnicodeString);

(*
    FUNCTION: DeactivateLicense()

    PURPOSE: Deactivates the license activation and frees up the corresponding activation
    slot by contacting the Cryptlex servers.

    This function should be executed at the time of de-registration, ideally on
    a button click.

    RETURN CODES: lkOk, lkFail

    EXCEPTIONS: ELADeactivationLimitException, ELAProductIdException,
    ELATimeException, ELALicenseKeyException, ELAInetException,
    ELAServerException, ELARateLimitException, ELATimeModifiedException
*)

function DeactivateLicense: TLAKeyStatus;

(*
    PROCEDURE: GenerateOfflineDeactivationRequest()

    PURPOSE: Generates the offline deactivation request needed for deactivation of
    the license in the dashboard and deactivates the license locally.

    A valid offline deactivation file confirms that the license has been successfully
    deactivated on the user's machine.

    PARAMETERS:
    * FilePath - path of the file for the offline request.

    EXCEPTION: ELAFailException, ELAProductIdException, ELALicenseKeyException,
    ELAFilePermissionException, ELATimeException, ELATimeModifiedException
*)

procedure GenerateOfflineDeactivationRequest(const FilePath: UnicodeString);

(*
    FUNCTION: IsLicenseGenuine()

    PURPOSE: It verifies whether your app is genuinely activated or not. The verification is
    done locally by verifying the cryptographic digital signature fetched at the time of
    activation.

    After verifying locally, it schedules a server check in a separate thread. After the
    first server sync it periodically does further syncs at a frequency set for the license.

    In case server sync fails due to network error, and it continues to fail for fixed
    number of days (grace period), the function returns lkGracePeriodOver instead of lkOK.

    This function must be called on every start of your program to verify the activation
    of your app.

    RETURN CODES: lkOK, lkExpired, lkSuspended, lkGracePeriodOver

    EXCEPTIONS: ELAFailException, ELAProductIdException, ELALicenseKeyException,
    ELATimeException, ELATimeModifiedException

    NOTE: If application was activated offline using ActivateLicenseOffline() function, you
    may want to set grace period to 0 to ignore grace period.
*)

function IsLicenseGenuine: TLAKeyStatus;

(*
    FUNCTION: IsLicenseValid()

    PURPOSE: It verifies whether your app is genuinely activated or not. The verification is
    done locally by verifying the cryptographic digital signature fetched at the time of
    activation.

    This is just an auxiliary function which you may use in some specific cases, when you
    want to skip the server sync.

    RETURN CODES: lkOK, lkExpired, lkSuspended, lkGracePeriodOver

    EXCEPTIONS: ELAFailException, ELAProductIdException, ELALicenseKeyException,
    ELATimeException

    NOTE: You may want to set grace period to 0 to ignore grace period.
*)

function IsLicenseValid: TLAKeyStatus;

(*
    FUNCTION: ActivateTrial()

    PURPOSE: Starts the verified trial in your application by contacting the
    Cryptlex servers.

    This function should be executed when your application starts first time on
    the user's computer, ideally on a button click.

    RETURN CODES: lkOK, lkTrialExpired

    EXCEPTIONS: ELAFailException, ELAProductIdException, ELAInetException,
    ELAVMException, ELATimeException, ELAServerException, ELAClientException,
    ELACountryException, ELAIPException, ELARateLimitException
*)

function ActivateTrial: TLAKeyStatus;

(*
    FUNCTION: ActivateTrialOffline()

    PURPOSE: Activates your trial using the offline activation response file.

    PARAMETERS:
    * FilePath - path of the offline activation response file.

    RETURN CODES: lkOK, lkTrialExpired, lkFail

    EXCEPTIONS: ELAProductIdException,
    ELAOfflineResponseFileException, ELAVMException, ELATimeException,
    ELAFilePathException, ELAOfflineResponseFileExpiredException
*)

function ActivateTrialOffline(const FilePath: UnicodeString): TLAKeyStatus;

(*
    PROCEDURE: GenerateOfflineTrialActivationRequest()

    PURPOSE: Generates the offline trial activation request needed for generating
    offline trial activation response in the dashboard.

    PARAMETERS:
    * FilePath - path of the file for the offline request.

    RETURN CODES: ELAFailException, ELAProductIdException,
    ELAFilePermissionException
*)

procedure GenerateOfflineTrialActivationRequest(const FilePath: UnicodeString);

(*
    FUNCTION: IsTrialGenuine()

    PURPOSE: It verifies whether trial has started and is genuine or not. The
    verification is done locally by verifying the cryptographic digital signature
    fetched at the time of trial activation.

    This function must be called on every start of your program during the trial period.

    RETURN CODES: lkOK, lkTrialExpired

    EXCEPTIONS: ELAFailException, ELATimeException, ELAProductIdException,
    ELATimeModifiedException

*)

function IsTrialGenuine: TLAKeyStatus;

(*
    FUNCTION: ActivateLocalTrial()

    PURPOSE: Starts the local(unverified) trial.

    This function should be executed when your application starts first time on
    the user's computer.

    PARAMETERS:
    * TrialLength - trial length in days

    RETURN CODES: lkOK, lkLocalTrialExpired

    EXCEPTIONS: ELAFailException, ELAProductIdException,
    ELATimeModifiedException

    NOTE: The function is only meant for local(unverified) trials.
*)

function ActivateLocalTrial(TrialLength: LongWord): TLAKeyStatus;

(*
    FUNCTION: IsLocalTrialGenuine()

    PURPOSE: It verifies whether trial has started and is genuine or not. The
    verification is done locally.

    This function must be called on every start of your program during the trial period.

    RETURN CODES: lkOK, lkLocalTrialExpired

    EXCEPTIONS: ELAFailException, ELAProductIdException,
    ELATimeModifiedException

    NOTE: The function is only meant for local(unverified) trials.
*)

function IsLocalTrialGenuine: TLAKeyStatus;

(*
    FUNCTION: ExtendLocalTrial()

    PURPOSE: Extends the local trial.

    PARAMETERS:
    * TrialExtensionLength - number of days to extend the trial

    RETURN CODES: lkOK

    EXCEPTIONS: ELAFailException, ELAProductIdException,
    ELATimeModifiedException

    NOTE: The function is only meant for local(unverified) trials.
*)

function ExtendLocalTrial(TrialExtensionLength: LongWord): TLAKeyStatus;

(*
    PROCEDURE: LAReset()

    PURPOSE: Resets the activation and trial data stored in the machine.

    This function is meant for developer testing only.

    EXCEPTIONS: ELAProductIdException

    NOTE: The function does not reset local(unverified) trial data.
*)

procedure LAReset;

(*** Exceptions ***)

type
  TLAStatusCode = type Integer;

  {$M+}
  ELAError = class(Exception) // parent of all LexActivator exceptions
  protected
    FErrorCode: TLAStatusCode;
  public
    // create exception of an appropriate class
    class function CreateByCode(ErrorCode: TLAStatusCode): ELAError;

    // check for LA_OK, otherwise raise exception
    class procedure Check(ErrorCode: TLAStatusCode); {$IFDEF DELPHI_HAS_INLINE} inline; {$ENDIF}

    // convert LA_OK into True, LA_FAIL into False, otherwise raise exception
    class function CheckOKFail(ErrorCode: TLAStatusCode): Boolean; {$IFDEF DELPHI_HAS_INLINE} inline; {$ENDIF}

    // convert ErrorCode to TLAKeyStatus, otherwise raise exception
    // LA_FAIL will be converted to lkFail
    class function CheckKeyStatus(ErrorCode: TLAStatusCode): TLAKeyStatus;

    property ErrorCode: TLAStatusCode read FErrorCode;
  end;
  {$M-}

  // Exceptional situation for LA_E_* codes
  ELAException = class(ELAError);

  // Function returned a code not understandable by Delphi binding yet
  ELAUnknownErrorCodeException = class(ELAException)
  protected
    constructor Create(AErrorCode: TLAStatusCode);
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
        CODE: LA_FAIL

        MESSAGE: Failure code.
    *)

  ELAFailException = class(ELAKeyStatusError)
  public
    constructor Create; overload;
    constructor Create(const Msg: string); overload;
    procedure AfterConstruction; override;
  end;

    (*
        CODE: LA_EXPIRED

        MESSAGE: The license has expired or system time has been tampered
        with. Ensure your date and time settings are correct.
    *)

  ELAExpiredError = class(ELAKeyStatusError)
  public
    constructor Create;
  end;

    (*
        CODE: LA_SUSPENDED

        MESSAGE: The license has been suspended.
    *)

  ELASuspendedError = class(ELAKeyStatusError)
  public
    constructor Create;
  end;

    (*
        CODE: LA_GRACE_PERIOD_OVER

        MESSAGE: The grace period for server sync is over.
    *)

  ELAGracePeriodOverError = class(ELAKeyStatusError)
  public
    constructor Create;
  end;

    (*
        CODE: LA_TRIAL_EXPIRED

        MESSAGE: The trial has expired or system time has been tampered
        with. Ensure your date and time settings are correct.
    *)

  ELATrialExpiredError = class(ELAKeyStatusError)
  public
    constructor Create;
  end;

    (*
        CODE: LA_LOCAL_TRIAL_EXPIRED

        MESSAGE: The local trial has expired or system time has been tampered
        with. Ensure your date and time settings are correct.
    *)

  ELALocalTrialExpiredError = class(ELAKeyStatusError)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_FILE_PATH

        MESSAGE: Invalid file path.
    *)

  ELAFilePathException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_PRODUCT_FILE

        MESSAGE: Invalid or corrupted product file.
    *)

  ELAProductFileException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_PRODUCT_DATA

        MESSAGE: Invalid product data.
    *)

  ELAProductDataException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_PRODUCT_ID

        MESSAGE: The product id is incorrect.
    *)

  ELAProductIdException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_SYSTEM_PERMISSION

        MESSAGE: Insufficent system permissions. Occurs when LA_SYSTEM flag is used
        but application is not run with admin privileges.
    *)

  ELASystemPermissionException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_FILE_PERMISSION

        MESSAGE: No permission to write to file.
    *)

  ELAFilePermissionException = class(ELAException)
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
        CODE: LA_E_TIME

        MESSAGE: The difference between the network time and the system time is
        more than allowed clock offset.
    *)

  ELATimeException = class(ELAException)
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
        CODE: LA_E_NET_PROXY

        MESSAGE: Invalid network proxy.
    *)

  ELANetProxyException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_HOST_URL

        MESSAGE: Invalid Cryptlex host url.
    *)

  ELAHostURLException = class(ELAException)
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
        CODE: LA_E_APP_VERSION_LENGTH

        MESSAGE: App version length is more than 256 characters.
    *)

  ELAAppVersionLengthException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_REVOKED

        MESSAGE: The license has been revoked.
    *)

  ELARevokedException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_LICENSE_KEY

        MESSAGE: Invalid license key.
    *)

  ELALicenseKeyException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_LICENSE_TYPE

        MESSAGE: Invalid license type. Make sure floating license
        is not being used.
    *)

  ELALicenseTypeException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_OFFLINE_RESPONSE_FILE

        MESSAGE: Invalid offline activation response file.
    *)

  ELAOfflineResponseFileException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_OFFLINE_RESPONSE_FILE_EXPIRED

        MESSAGE: The offline activation response has expired.
    *)

  ELAOfflineResponseFileExpiredException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_ACTIVATION_LIMIT

        MESSAGE: The license has reached it's allowed activations limit.
    *)

  ELAActivationLimitException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_ACTIVATION_NOT_FOUND

        MESSAGE: The license activation was deleted on the server.
    *)

  ELAActivationNotFoundException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_DEACTIVATION_LIMIT

        MESSAGE: The license has reached it's allowed deactivations limit.
    *)

  ELADeactivationLimitException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_TRIAL_NOT_ALLOWED

        MESSAGE: Trial not allowed for the product.
    *)

  ELATrialNotAllowedException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_TRIAL_ACTIVATION_LIMIT

        MESSAGE: Your account has reached it's trial activations limit.
    *)

  ELATrialActivationLimitException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_MACHINE_FINGERPRINT

        MESSAGE: Machine fingerprint has changed since activation.
    *)

  ELAMachineFingerprintException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_METADATA_KEY_LENGTH

        MESSAGE: Metadata key length is more than 256 characters.
    *)

  ELAMetadataKeyLengthException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_METADATA_VALUE_LENGTH

        MESSAGE: Metadata value length is more than 256 characters.
    *)

  ELAMetadataValueLengthException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_ACTIVATION_METADATA_LIMIT

        MESSAGE: The license has reached it's metadata fields limit.
    *)

  ELAActivationMetadataLimitException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_TRIAL_ACTIVATION_METADATA_LIMIT

        MESSAGE: The trial has reached it's metadata fields limit.
    *)

  ELATrialActivationMetadataLimitException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_METADATA_KEY_NOT_FOUND

        MESSAGE: The metadata key does not exist.
    *)

  ELAMetadataKeyNotFoundException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_TIME_MODIFIED

        MESSAGE: The system time has been tampered (backdated).
    *)

  ELATimeModifiedException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_VM

        MESSAGE: Application is being run inside a virtual machine / hypervisor,
        and activation has been disallowed in the VM.
    *)

  ELAVMException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_COUNTRY

        MESSAGE: Country is not allowed.
    *)

  ELACountryException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_IP

        MESSAGE: IP address is not allowed.
    *)

  ELAIPException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_RATE_LIMIT

        MESSAGE: Rate limit for API has reached, try again later.
    *)

  ELARateLimitException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_SERVER

        MESSAGE: Server error.
    *)

  ELAServerException = class(ELAException)
  public
    constructor Create;
  end;

    (*
        CODE: LA_E_CLIENT

        MESSAGE: Client error.
    *)

  ELAClientException = class(ELAException)
  public
    constructor Create;
  end;

implementation

uses
{$IFDEF DELPHI_UNITS_SCOPED}
  System.TypInfo, System.DateUtils, System.Classes, Winapi.Windows
{$ELSE}
  TypInfo, DateUtils, Classes, Windows
{$ENDIF}
  ;

const
  LexActivator_DLL = 'LexActivator.dll';

const
  LAFlagsToLongWord: array[TLAFlags] of LongWord = (1, 2);

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

    (*
        CODE: LA_OK

        MESSAGE: Success code.
    *)

  LA_OK = TLAStatusCode(0);

    (*
        CODE: LA_FAIL

        MESSAGE: Failure code.
    *)

  LA_FAIL = TLAStatusCode(1);

    (*
        CODE: LA_EXPIRED

        MESSAGE: The license has expired or system time has been tampered
        with. Ensure your date and time settings are correct.
    *)

  LA_EXPIRED = TLAStatusCode(20);

    (*
        CODE: LA_SUSPENDED

        MESSAGE: The license has been suspended.
    *)

  LA_SUSPENDED = TLAStatusCode(21);

    (*
        CODE: LA_GRACE_PERIOD_OVER

        MESSAGE: The grace period for server sync is over.
    *)

  LA_GRACE_PERIOD_OVER = TLAStatusCode(22);

    (*
        CODE: LA_TRIAL_EXPIRED

        MESSAGE: The trial has expired or system time has been tampered
        with. Ensure your date and time settings are correct.
    *)

  LA_TRIAL_EXPIRED = TLAStatusCode(25);

    (*
        CODE: LA_LOCAL_TRIAL_EXPIRED

        MESSAGE: The local trial has expired or system time has been tampered
        with. Ensure your date and time settings are correct.
    *)

  LA_LOCAL_TRIAL_EXPIRED = TLAStatusCode(26);

    (*
        CODE: LA_E_FILE_PATH

        MESSAGE: Invalid file path.
    *)

  LA_E_FILE_PATH = TLAStatusCode(40);

    (*
        CODE: LA_E_PRODUCT_FILE

        MESSAGE: Invalid or corrupted product file.
    *)

  LA_E_PRODUCT_FILE = TLAStatusCode(41);

    (*
        CODE: LA_E_PRODUCT_DATA

        MESSAGE: Invalid product data.
    *)

  LA_E_PRODUCT_DATA = TLAStatusCode(42);

    (*
        CODE: LA_E_PRODUCT_ID

        MESSAGE: The product id is incorrect.
    *)

  LA_E_PRODUCT_ID = TLAStatusCode(43);

    (*
        CODE: LA_E_SYSTEM_PERMISSION

        MESSAGE: Insufficent system permissions. Occurs when LA_SYSTEM flag is used
        but application is not run with admin privileges.
    *)

  LA_E_SYSTEM_PERMISSION = TLAStatusCode(44);

    (*
        CODE: LA_E_FILE_PERMISSION

        MESSAGE: No permission to write to file.
    *)

  LA_E_FILE_PERMISSION = TLAStatusCode(45);

    (*
        CODE: LA_E_WMIC

        MESSAGE: Fingerprint couldn't be generated because Windows Management
        Instrumentation (WMI) service has been disabled. This error is specific
        to Windows only.
    *)

  LA_E_WMIC = TLAStatusCode(46);

    (*
        CODE: LA_E_TIME

        MESSAGE: The difference between the network time and the system time is
        more than allowed clock offset.
    *)

  LA_E_TIME = TLAStatusCode(47);

    (*
        CODE: LA_E_INET

        MESSAGE: Failed to connect to the server due to network error.
    *)

  LA_E_INET = TLAStatusCode(48);

    (*
        CODE: LA_E_NET_PROXY

        MESSAGE: Invalid network proxy.
    *)

  LA_E_NET_PROXY = TLAStatusCode(49);

    (*
        CODE: LA_E_HOST_URL

        MESSAGE: Invalid Cryptlex host url.
    *)

  LA_E_HOST_URL = TLAStatusCode(50);

    (*
        CODE: LA_E_BUFFER_SIZE

        MESSAGE: The buffer size was smaller than required.
    *)

  LA_E_BUFFER_SIZE = TLAStatusCode(51);

    (*
        CODE: LA_E_APP_VERSION_LENGTH

        MESSAGE: App version length is more than 256 characters.
    *)

  LA_E_APP_VERSION_LENGTH = TLAStatusCode(52);

    (*
        CODE: LA_E_REVOKED

        MESSAGE: The license has been revoked.
    *)

  LA_E_REVOKED = TLAStatusCode(53);

    (*
        CODE: LA_E_LICENSE_KEY

        MESSAGE: Invalid license key.
    *)

  LA_E_LICENSE_KEY = TLAStatusCode(54);

    (*
        CODE: LA_E_LICENSE_TYPE

        MESSAGE: Invalid license type. Make sure floating license
        is not being used.
    *)

  LA_E_LICENSE_TYPE = TLAStatusCode(55);

    (*
        CODE: LA_E_OFFLINE_RESPONSE_FILE

        MESSAGE: Invalid offline activation response file.
    *)

  LA_E_OFFLINE_RESPONSE_FILE = TLAStatusCode(56);

    (*
        CODE: LA_E_OFFLINE_RESPONSE_FILE_EXPIRED

        MESSAGE: The offline activation response has expired.
    *)

  LA_E_OFFLINE_RESPONSE_FILE_EXPIRED = TLAStatusCode(57);

    (*
        CODE: LA_E_ACTIVATION_LIMIT

        MESSAGE: The license has reached it's allowed activations limit.
    *)

  LA_E_ACTIVATION_LIMIT = TLAStatusCode(58);

    (*
        CODE: LA_E_ACTIVATION_NOT_FOUND

        MESSAGE: The license activation was deleted on the server.
    *)

  LA_E_ACTIVATION_NOT_FOUND = TLAStatusCode(59);

    (*
        CODE: LA_E_DEACTIVATION_LIMIT

        MESSAGE: The license has reached it's allowed deactivations limit.
    *)

  LA_E_DEACTIVATION_LIMIT = TLAStatusCode(60);

    (*
        CODE: LA_E_TRIAL_NOT_ALLOWED

        MESSAGE: Trial not allowed for the product.
    *)

  LA_E_TRIAL_NOT_ALLOWED = TLAStatusCode(61);

    (*
        CODE: LA_E_TRIAL_ACTIVATION_LIMIT

        MESSAGE: Your account has reached it's trial activations limit.
    *)

  LA_E_TRIAL_ACTIVATION_LIMIT = TLAStatusCode(62);

    (*
        CODE: LA_E_MACHINE_FINGERPRINT

        MESSAGE: Machine fingerprint has changed since activation.
    *)

  LA_E_MACHINE_FINGERPRINT = TLAStatusCode(63);

    (*
        CODE: LA_E_METADATA_KEY_LENGTH

        MESSAGE: Metadata key length is more than 256 characters.
    *)

  LA_E_METADATA_KEY_LENGTH = TLAStatusCode(64);

    (*
        CODE: LA_E_METADATA_VALUE_LENGTH

        MESSAGE: Metadata value length is more than 256 characters.
    *)

  LA_E_METADATA_VALUE_LENGTH = TLAStatusCode(65);

    (*
        CODE: LA_E_ACTIVATION_METADATA_LIMIT

        MESSAGE: The license has reached it's metadata fields limit.
    *)

  LA_E_ACTIVATION_METADATA_LIMIT = TLAStatusCode(66);

    (*
        CODE: LA_E_TRIAL_ACTIVATION_METADATA_LIMIT

        MESSAGE: The trial has reached it's metadata fields limit.
    *)

  LA_E_TRIAL_ACTIVATION_METADATA_LIMIT = TLAStatusCode(67);

    (*
        CODE: LA_E_METADATA_KEY_NOT_FOUND

        MESSAGE: The metadata key does not exist.
    *)

  LA_E_METADATA_KEY_NOT_FOUND = TLAStatusCode(68);

    (*
        CODE: LA_E_TIME_MODIFIED

        MESSAGE: The system time has been tampered (backdated).
    *)

  LA_E_TIME_MODIFIED = TLAStatusCode(69);

    (*
        CODE: LA_E_VM

        MESSAGE: Application is being run inside a virtual machine / hypervisor,
        and activation has been disallowed in the VM.
    *)

  LA_E_VM = TLAStatusCode(80);

    (*
        CODE: LA_E_COUNTRY

        MESSAGE: Country is not allowed.
    *)

  LA_E_COUNTRY = TLAStatusCode(81);

    (*
        CODE: LA_E_IP

        MESSAGE: IP address is not allowed.
    *)

  LA_E_IP = TLAStatusCode(82);

    (*
        CODE: LA_E_RATE_LIMIT

        MESSAGE: Rate limit for API has reached, try again later.
    *)

  LA_E_RATE_LIMIT = TLAStatusCode(90);

    (*
        CODE: LA_E_SERVER

        MESSAGE: Server error.
    *)

  LA_E_SERVER = TLAStatusCode(91);

    (*
        CODE: LA_E_CLIENT

        MESSAGE: Client error.
    *)

  LA_E_CLIENT = TLAStatusCode(92);

(*********************************************************************************)

function Thin_SetProductFile(const filePath: PWideChar): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'SetProductFile';

procedure SetProductFile(const FilePath: UnicodeString);
begin
  if not ELAError.CheckOKFail(Thin_SetProductFile(PWideChar(FilePath))) then
    raise
    ELAFailException.Create('Failed to set the path of the Product.dat file');
end;

function Thin_SetProductData(const productData: PWideChar): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'SetProductData';

procedure SetProductData(const ProductData: UnicodeString);
begin
  if not ELAError.CheckOKFail(Thin_SetProductData(PWideChar(ProductData))) then
    raise
    ELAFailException.Create('Failed to embed the Product.dat file in the application');
end;

function Thin_SetProductId(const productId: PWideChar; flags: LongWord): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'SetProductId';

procedure SetProductId(const ProductId: UnicodeString; Flags: TLAFlags);
begin
  if not ELAError.CheckOKFail(Thin_SetProductId(PWideChar(ProductId), LAFlagsToLongWord[Flags])) then
    raise
    ELAFailException.CreateFmt('Failed to set the product id of the application ' +
      'to %s', [ProductId]);
end;

function Thin_SetLicenseKey(const licenseKey: PWideChar): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'SetLicenseKey';

procedure SetLicenseKey(const LicenseKey: UnicodeString);
begin
  if not ELAError.CheckOKFail(Thin_SetLicenseKey(PWideChar(LicenseKey))) then
    raise
    ELAFailException.Create('Failed to set the license key');
end;

type
  TLAThin_CallbackType = procedure (StatusCode: LongWord); cdecl;

function Thin_SetLicenseCallback(callback: TLAThin_CallbackType): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'SetLicenseCallback';

type
  TLALicenseCallbackKind =
    (lckNone,
     lckProcedure,
     lckMethod
     {$IFDEF DELPHI_HAS_CLOSURES}, lckClosure{$ENDIF});

var
  LALicenseCallbackKind: TLALicenseCallbackKind = lckNone;
  LAProcedureCallback: TLAProcedureCallback;
  LAMethodCallback: TLAMethodCallback;
  {$IFDEF DELPHI_HAS_CLOSURES}
  LAClosureCallback: TLAClosureCallback;
  {$ENDIF}
  LALicenseCallbackSynchronized: Boolean;
  LAStatusCode: TLAStatusCode;
  LALicenseCallbackMutex: TRTLCriticalSection;

type
  TLAThin_CallbackProxyClass = class
  public
    class procedure Invoke;
  end;

class procedure TLAThin_CallbackProxyClass.Invoke;
var
  KeyStatus: TLAKeyStatus;

  procedure DoInvoke(const Error: Exception);
  begin
    case LALicenseCallbackKind of
      lckNone: Exit;
      lckProcedure:
        if Assigned(LAProcedureCallback) then
          LAProcedureCallback(Error, KeyStatus);
      lckMethod:
        if Assigned(LAMethodCallback) then
          LAMethodCallback(Error, KeyStatus);
      {$IFDEF DELPHI_HAS_CLOSURES}
      lckClosure:
        if Assigned(LAClosureCallback) then
          LAClosureCallback(Error, KeyStatus);
      {$ENDIF}
    else
      // there should be default logging here like NSLog, but there is none in Delphi
    end;
  end;

var
  FailError: Exception;

begin
  try
    EnterCriticalSection(LALicenseCallbackMutex);
    try
      case LALicenseCallbackKind of
        lckNone: Exit;
        lckProcedure: if not Assigned(LAProcedureCallback) then Exit;
        lckMethod: if not Assigned(LAMethodCallback) then Exit;
        {$IFDEF DELPHI_HAS_CLOSURES}
        lckClosure: if not Assigned(LAClosureCallback) then Exit;
        {$ENDIF}
      else
        // there should be default logging here like NSLog, but there is none in Delphi
      end;

      try
        KeyStatus := ELAError.CheckKeyStatus(LAStatusCode);
      except
        on Error: ELAError do
        begin
          KeyStatus := lkException;
          DoInvoke(Error);
          Exit;
        end;
      end;

      FailError := nil;
      try
        FailError := ELAError.CreateByCode(LAStatusCode);
        DoInvoke(FailError);
      finally
        FreeAndNil(FailError);
      end;
    finally
      // This recursive mutex prevents the following scenario:
      //
      // (Main thread)    SetLicenseCallback
      // (Tangent thread) LAThin_CallbackProxy going to invoke X.OnLexActivator
      // (Main thread)    ResetLicenseCallback
      // (Main thread)    X.Free
      //
      // Main thread is not allowed to proceed to X.Free until callback is finished
      // On the other hand, if callback removes itself, recursive mutex will allow that
      LeaveCriticalSection(LALicenseCallbackMutex);
    end;
  except
    // there should be default logging here like NSLog, but there is none in Delphi
  end;
end;

procedure LAThin_CallbackProxy(StatusCode: LongWord); cdecl;
begin
  try
    EnterCriticalSection(LALicenseCallbackMutex);
    try
      case LALicenseCallbackKind of
        lckNone: Exit;
        lckProcedure: if not Assigned(LAProcedureCallback) then Exit;
        lckMethod: if not Assigned(LAMethodCallback) then Exit;
        {$IFDEF DELPHI_HAS_CLOSURES}
        lckClosure: if not Assigned(LAClosureCallback) then Exit;
        {$ENDIF}
      else
        // there should be default logging here like NSLog, but there is none in Delphi
      end;

      LAStatusCode := TLAStatusCode(StatusCode);
      if not LALicenseCallbackSynchronized then
      begin
        TLAThin_CallbackProxyClass.Invoke;
        Exit;
      end;
    finally
      LeaveCriticalSection(LALicenseCallbackMutex);
    end;

    // Race condition here
    //
    // Invoke should proably run exactly the same (captured) handler,
    // but instead it reenters mutex, and handler can be different at
    // that moment. For most sane use cases behavior should be sound
    // anyway.

    TThread.Synchronize(nil, TLAThin_CallbackProxyClass.Invoke);
  except
    // there should be default logging here like NSLog, but there is none in Delphi
  end;
end;

procedure SetLicenseCallback(Callback: TLAProcedureCallback; Synchronized: Boolean);
begin
  EnterCriticalSection(LALicenseCallbackMutex);
  try
    LAProcedureCallback := Callback;
    LALicenseCallbackSynchronized := Synchronized;
    LALicenseCallbackKind := lckProcedure;

    if not ELAError.CheckOKFail(Thin_SetLicenseCallback(LAThin_CallbackProxy)) then
      raise
      ELAFailException.Create('Failed to set server sync callback');
  finally
    LeaveCriticalSection(LALicenseCallbackMutex);
  end;
end;

procedure SetLicenseCallback(Callback: TLAMethodCallback; Synchronized: Boolean);
begin
  EnterCriticalSection(LALicenseCallbackMutex);
  try
    LAMethodCallback := Callback;
    LALicenseCallbackSynchronized := Synchronized;
    LALicenseCallbackKind := lckMethod;

    if not ELAError.CheckOKFail(Thin_SetLicenseCallback(LAThin_CallbackProxy)) then
      raise
      ELAFailException.Create('Failed to set server sync callback');
  finally
    LeaveCriticalSection(LALicenseCallbackMutex);
  end;
end;

{$IFDEF DELPHI_HAS_CLOSURES}
procedure SetLicenseCallback(Callback: TLAClosureCallback; Synchronized: Boolean);
begin
  EnterCriticalSection(LALicenseCallbackMutex);
  try
    LAClosureCallback := Callback;
    LALicenseCallbackSynchronized := Synchronized;
    LALicenseCallbackKind := lckClosure;

    if not ELAError.CheckOKFail(Thin_SetLicenseCallback(LAThin_CallbackProxy)) then
      raise
      ELAFailException.Create('Failed to set server sync callback');
  finally
    LeaveCriticalSection(LALicenseCallbackMutex);
  end;
end;
{$ENDIF}

procedure LAThin_CallbackDummy(StatusCode: LongWord); cdecl;
begin
  ;
end;

procedure ResetLicenseCallback;
begin
  EnterCriticalSection(LALicenseCallbackMutex);
  try
    LALicenseCallbackKind := lckNone;

    if not ELAError.CheckOKFail(Thin_SetLicenseCallback(LAThin_CallbackDummy)) then
      raise
      ELAFailException.Create('Failed to set server sync callback');
  finally
    LeaveCriticalSection(LALicenseCallbackMutex);
  end;
end;

function Thin_SetActivationMetadata(const key, value: PWideChar): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'SetActivationMetadata';

procedure SetActivationMetadata(const Key, Value: UnicodeString);
begin
  if not ELAError.CheckOKFail(Thin_SetActivationMetadata(PWideChar(Key), PWideChar(Value))) then
    raise
    ELAFailException.Create('Failed to set the activation metadata');
end;

function Thin_SetTrialActivationMetadata(const key, value: PWideChar): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'SetTrialActivationMetadata';

procedure SetTrialActivationMetadata(const Key, Value: UnicodeString);
begin
  if not ELAError.CheckOKFail(Thin_SetTrialActivationMetadata(PWideChar(Key), PWideChar(Value))) then
    raise
    ELAFailException.Create('Failed to set the trial activation metadata');
end;

function Thin_SetAppVersion(const appVersion: PWideChar): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'SetAppVersion';

procedure SetAppVersion(const AppVersion: UnicodeString);
begin
  if not ELAError.CheckOKFail(Thin_SetAppVersion(PWideChar(AppVersion))) then
    raise
    ELAFailException.CreateFmt('Failed to set the current app version of the ' +
    'application to %s', [AppVersion]);
end;

function Thin_SetNetworkProxy(const proxy: PWideChar): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'SetNetworkProxy';

procedure SetNetworkProxy(const Proxy: UnicodeString);
begin
  if not ELAError.CheckOKFail(Thin_SetNetworkProxy(PWideChar(Proxy))) then
    raise
    ELAFailException.CreateFmt('Failed to set the network proxy to ' +
    '%s', [Proxy]);
end;

function Thin_GetProductMetadata(const key: PWideChar; out value; length: LongWord): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'GetProductMetadata';

function GetProductMetadata(const Key: UnicodeString): UnicodeString;
var
  ErrorCode: TLAStatusCode;
  function Try256(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: array[0 .. 255] of WideChar;
  begin
    ErrorCode := Thin_GetProductMetadata(PWideChar(Key), Buffer, Length(Buffer));
    Result := ErrorCode <> LA_E_BUFFER_SIZE;
    if ErrorCode = LA_OK then OuterResult := Buffer;
  end;
  function TryHigh(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: UnicodeString;
    Size: Integer;
  begin
    Size := 512;
    repeat
      Size := Size * 2;
      SetLength(Buffer, 0);
      SetLength(Buffer, Size);
      ErrorCode := Thin_GetProductMetadata(PWideChar(Key), PWideChar(Buffer)^, Size);
      Result := ErrorCode <> LA_E_BUFFER_SIZE;
    until Result or (Size >= 128 * 1024);
    if ErrorCode = LA_OK then OuterResult := PWideChar(Buffer);
  end;
begin
  if not Try256(Result) then TryHigh(Result);
  if not ELAError.CheckOKFail(ErrorCode) then
    raise ELAFailException.CreateFmt('Failed to get the product metadata with key %s', [Key]);
end;

function Thin_GetLicenseMetadata(const key: UnicodeString; out value; length: LongWord): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'GetLicenseMetadata';

function GetLicenseMetadata(const Key: UnicodeString): UnicodeString;
var
  ErrorCode: TLAStatusCode;
  function Try256(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: array[0 .. 255] of WideChar;
  begin
    ErrorCode := Thin_GetLicenseMetadata(PWideChar(Key), Buffer, Length(Buffer));
    Result := ErrorCode <> LA_E_BUFFER_SIZE;
    if ErrorCode = LA_OK then OuterResult := Buffer;
  end;
  function TryHigh(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: UnicodeString;
    Size: Integer;
  begin
    Size := 512;
    repeat
      Size := Size * 2;
      SetLength(Buffer, 0);
      SetLength(Buffer, Size);
      ErrorCode := Thin_GetLicenseMetadata(PWideChar(Key), PWideChar(Buffer)^, Size);
      Result := ErrorCode <> LA_E_BUFFER_SIZE;
    until Result or (Size >= 128 * 1024);
    if ErrorCode = LA_OK then OuterResult := PWideChar(Buffer);
  end;
begin
  if not Try256(Result) then TryHigh(Result);
  if not ELAError.CheckOKFail(ErrorCode) then
    raise ELAFailException.CreateFmt('Failed to get the license metadata with key %s', [Key]);
end;

function Thin_GetLicenseKey(out licenseKey; length: LongWord): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'GetLicenseKey';

function GetLicenseKey: UnicodeString;
var
  ErrorCode: TLAStatusCode;
  function Try256(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: array[0 .. 255] of WideChar;
  begin
    ErrorCode := Thin_GetLicenseKey(Buffer, Length(Buffer));
    Result := ErrorCode <> LA_E_BUFFER_SIZE;
    if ErrorCode = LA_OK then OuterResult := Buffer;
  end;
  function TryHigh(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: UnicodeString;
    Size: Integer;
  begin
    Size := 512;
    repeat
      Size := Size * 2;
      SetLength(Buffer, 0);
      SetLength(Buffer, Size);
      ErrorCode := Thin_GetLicenseKey(PWideChar(Buffer)^, Size);
      Result := ErrorCode <> LA_E_BUFFER_SIZE;
    until Result or (Size >= 128 * 1024);
    if ErrorCode = LA_OK then OuterResult := PWideChar(Buffer);
  end;
begin
  if not Try256(Result) then TryHigh(Result);
  if not ELAError.CheckOKFail(ErrorCode) then
    raise ELAFailException.Create('Failed to get the license key used for activation');
end;

function Thin_GetLicenseExpiryDate(out expiryDate: LongWord): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'GetLicenseExpiryDate';

function GetLicenseExpiryDate: TDateTime;
var
  ExpiryDate: LongWord;
begin
  if not ELAError.CheckOKFail(Thin_GetLicenseExpiryDate(ExpiryDate)) then
    raise
    ELAFailException.Create('Failed to get the license expiry date timestamp');
  Result := UnixToDateTime(ExpiryDate);  
end;

function Thin_GetLicenseUserEmail(out email; length: LongWord): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'GetLicenseUserEmail';

function GetLicenseUserEmail: UnicodeString;
var
  ErrorCode: TLAStatusCode;
  function Try256(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: array[0 .. 255] of WideChar;
  begin
    ErrorCode := Thin_GetLicenseUserEmail(Buffer, Length(Buffer));
    Result := ErrorCode <> LA_E_BUFFER_SIZE;
    if ErrorCode = LA_OK then OuterResult := Buffer;
  end;
  function TryHigh(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: UnicodeString;
    Size: Integer;
  begin
    Size := 512;
    repeat
      Size := Size * 2;
      SetLength(Buffer, 0);
      SetLength(Buffer, Size);
      ErrorCode := Thin_GetLicenseUserEmail(PWideChar(Buffer)^, Size);
      Result := ErrorCode <> LA_E_BUFFER_SIZE;
    until Result or (Size >= 128 * 1024);
    if ErrorCode = LA_OK then OuterResult := PWideChar(Buffer);
  end;
begin
  if not Try256(Result) then TryHigh(Result);
  if not ELAError.CheckOKFail(ErrorCode) then
    raise ELAFailException.Create('Failed to get the email associated with license user');
end;

function Thin_GetLicenseUserName(out name; length: LongWord): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'GetLicenseUserName';

function GetLicenseUserName: UnicodeString;
var
  ErrorCode: TLAStatusCode;
  function Try256(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: array[0 .. 255] of WideChar;
  begin
    ErrorCode := Thin_GetLicenseUserName(Buffer, Length(Buffer));
    Result := ErrorCode <> LA_E_BUFFER_SIZE;
    if ErrorCode = LA_OK then OuterResult := Buffer;
  end;
  function TryHigh(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: UnicodeString;
    Size: Integer;
  begin
    Size := 512;
    repeat
      Size := Size * 2;
      SetLength(Buffer, 0);
      SetLength(Buffer, Size);
      ErrorCode := Thin_GetLicenseUserName(PWideChar(Buffer)^, Size);
      Result := ErrorCode <> LA_E_BUFFER_SIZE;
    until Result or (Size >= 128 * 1024);
    if ErrorCode = LA_OK then OuterResult := PWideChar(Buffer);
  end;
begin
  if not Try256(Result) then TryHigh(Result);
  if not ELAError.CheckOKFail(ErrorCode) then
    raise ELAFailException.Create('Failed to get the name associated with license user');
end;

function Thin_GetLicenseType(out name; length: LongWord): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'GetLicenseType';

function GetLicenseType: UnicodeString;
var
  ErrorCode: TLAStatusCode;
  function Try256(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: array[0 .. 255] of WideChar;
  begin
    ErrorCode := Thin_GetLicenseType(Buffer, Length(Buffer));
    Result := ErrorCode <> LA_E_BUFFER_SIZE;
    if ErrorCode = LA_OK then OuterResult := Buffer;
  end;
  function TryHigh(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: UnicodeString;
    Size: Integer;
  begin
    Size := 512;
    repeat
      Size := Size * 2;
      SetLength(Buffer, 0);
      SetLength(Buffer, Size);
      ErrorCode := Thin_GetLicenseType(PWideChar(Buffer)^, Size);
      Result := ErrorCode <> LA_E_BUFFER_SIZE;
    until Result or (Size >= 128 * 1024);
    if ErrorCode = LA_OK then OuterResult := PWideChar(Buffer);
  end;
begin
  if not Try256(Result) then TryHigh(Result);
  if not ELAError.CheckOKFail(ErrorCode) then
    raise ELAFailException.Create('Failed to get the license type');
end;

function Thin_GetActivationMetadata(const key: PWideChar; out value; length: LongWord): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'GetActivationMetadata';

function GetActivationMetadata(const Key: UnicodeString): UnicodeString;
var
  ErrorCode: TLAStatusCode;
  function Try256(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: array[0 .. 255] of WideChar;
  begin
    ErrorCode := Thin_GetActivationMetadata(PWideChar(Key), Buffer, Length(Buffer));
    Result := ErrorCode <> LA_E_BUFFER_SIZE;
    if ErrorCode = LA_OK then OuterResult := Buffer;
  end;
  function TryHigh(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: UnicodeString;
    Size: Integer;
  begin
    Size := 512;
    repeat
      Size := Size * 2;
      SetLength(Buffer, 0);
      SetLength(Buffer, Size);
      ErrorCode := Thin_GetActivationMetadata(PWideChar(Key), PWideChar(Buffer)^, Size);
      Result := ErrorCode <> LA_E_BUFFER_SIZE;
    until Result or (Size >= 128 * 1024);
    if ErrorCode = LA_OK then OuterResult := PWideChar(Buffer);
  end;
begin
  if not Try256(Result) then TryHigh(Result);
  if not ELAError.CheckOKFail(ErrorCode) then
    raise ELAFailException.CreateFmt('Failed to get the activation metadata with key %s', [Key]);
end;

function Thin_GetTrialActivationMetadata(const key: PWideChar; out value; length: LongWord): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'GetTrialActivationMetadata';

function GetTrialActivationMetadata(const Key: UnicodeString): UnicodeString;
var
  ErrorCode: TLAStatusCode;
  function Try256(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: array[0 .. 255] of WideChar;
  begin
    ErrorCode := Thin_GetTrialActivationMetadata(PWideChar(Key), Buffer, Length(Buffer));
    Result := ErrorCode <> LA_E_BUFFER_SIZE;
    if ErrorCode = LA_OK then OuterResult := Buffer;
  end;
  function TryHigh(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: UnicodeString;
    Size: Integer;
  begin
    Size := 512;
    repeat
      Size := Size * 2;
      SetLength(Buffer, 0);
      SetLength(Buffer, Size);
      ErrorCode := Thin_GetTrialActivationMetadata(PWideChar(Key), PWideChar(Buffer)^, Size);
      Result := ErrorCode <> LA_E_BUFFER_SIZE;
    until Result or (Size >= 128 * 1024);
    if ErrorCode = LA_OK then OuterResult := PWideChar(Buffer);
  end;
begin
  if not Try256(Result) then TryHigh(Result);
  if not ELAError.CheckOKFail(ErrorCode) then
    raise ELAFailException.CreateFmt('Failed to get the trial activation metadata with key %s', [Key]);
end;

function Thin_GetTrialExpiryDate(out trialExpiryDate: LongWord): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'GetTrialExpiryDate';

function GetTrialExpiryDate: TDateTime;
var
  TrialExpiryDate: LongWord;
begin
  if not ELAError.CheckOKFail(Thin_GetTrialExpiryDate(TrialExpiryDate)) then
    raise
    ELAFailException.Create('Failed to get the trial expiry date timestamp');
  Result := UnixToDateTime(TrialExpiryDate);
end;

function Thin_GetTrialId(out trialId; length: LongWord): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'GetTrialId';

function GetTrialId: UnicodeString;
var
  ErrorCode: TLAStatusCode;
  function Try256(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: array[0 .. 255] of WideChar;
  begin
    ErrorCode := Thin_GetTrialId(Buffer, Length(Buffer));
    Result := ErrorCode <> LA_E_BUFFER_SIZE;
    if ErrorCode = LA_OK then OuterResult := Buffer;
  end;
  function TryHigh(var OuterResult: UnicodeString): Boolean;
  var
    Buffer: UnicodeString;
    Size: Integer;
  begin
    Size := 512;
    repeat
      Size := Size * 2;
      SetLength(Buffer, 0);
      SetLength(Buffer, Size);
      ErrorCode := Thin_GetTrialId(PWideChar(Buffer)^, Size);
      Result := ErrorCode <> LA_E_BUFFER_SIZE;
    until Result or (Size >= 128 * 1024);
    if ErrorCode = LA_OK then OuterResult := PWideChar(Buffer);
  end;
begin
  if not Try256(Result) then TryHigh(Result);
  if not ELAError.CheckOKFail(ErrorCode) then
    raise ELAFailException.Create('Failed to get the trial activation id');
end;

function Thin_GetLocalTrialExpiryDate(out trialExpiryDate: LongWord): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'GetLocalTrialExpiryDate';

function GetLocalTrialExpiryDate: TDateTime;
var
  TrialExpiryDate: LongWord;
begin
  if not ELAError.CheckOKFail(Thin_GetLocalTrialExpiryDate(TrialExpiryDate)) then
    raise
    ELAFailException.Create('Failed to get the trial expiry date timestamp');
  Result := UnixToDateTime(TrialExpiryDate);
end;

function Thin_ActivateLicense: TLAStatusCode; cdecl;
  external LexActivator_DLL name 'ActivateLicense';

function ActivateLicense: TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_ActivateLicense);
end;

function Thin_ActivateLicenseOffline(const filePath: PWideChar): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'ActivateLicenseOffline';

function ActivateLicenseOffline(const FilePath: UnicodeString): TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_ActivateLicenseOffline(PWideChar(FilePath)));
end;

function Thin_GenerateOfflineActivationRequest(const filePath: PWideChar): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'GenerateOfflineActivationRequest';

procedure GenerateOfflineActivationRequest(const FilePath: UnicodeString);
begin
  if not ELAError.CheckOKFail(Thin_GenerateOfflineActivationRequest(PWideChar(FilePath))) then
    raise
    ELAFailException.Create('Failed to generate the offline activation request');
end;

function Thin_DeactivateLicense: TLAStatusCode; cdecl;
  external LexActivator_DLL name 'DeactivateLicense';

function DeactivateLicense: TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_DeactivateLicense);
end;

function Thin_GenerateOfflineDeactivationRequest(const filePath: PWideChar): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'GenerateOfflineDeactivationRequest';

procedure GenerateOfflineDeactivationRequest(const FilePath: UnicodeString);
begin
  if not ELAError.CheckOKFail(Thin_GenerateOfflineDeactivationRequest(PWideChar(FilePath))) then
    raise
    ELAFailException.Create('Failed to generate the offline deactivation request');
end;

function Thin_IsLicenseGenuine: TLAStatusCode; cdecl;
  external LexActivator_DLL name 'IsLicenseGenuine';

function IsLicenseGenuine: TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_IsLicenseGenuine);
end;

function Thin_IsLicenseValid: TLAStatusCode; cdecl;
  external LexActivator_DLL name 'IsLicenseValid';

function IsLicenseValid: TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_IsLicenseValid);
end;

function Thin_ActivateTrial: TLAStatusCode; cdecl;
  external LexActivator_DLL name 'ActivateTrial';

function ActivateTrial: TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_ActivateTrial);
end;

function Thin_ActivateTrialOffline(const filePath: PWideChar): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'ActivateTrialOffline';

function ActivateTrialOffline(const FilePath: UnicodeString): TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_ActivateTrialOffline(PWideChar(FilePath)));
end;

function Thin_GenerateOfflineTrialActivationRequest(const filePath: PWideChar): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'GenerateOfflineTrialActivationRequest';

procedure GenerateOfflineTrialActivationRequest(const FilePath: UnicodeString);
begin
  if not ELAError.CheckOKFail(Thin_GenerateOfflineTrialActivationRequest(PWideChar(FilePath))) then
    raise
    ELAFailException.Create('Failed to generate the offline trial activation request');
end;

function Thin_IsTrialGenuine: TLAStatusCode; cdecl;
  external LexActivator_DLL name 'IsTrialGenuine';

function IsTrialGenuine: TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_IsTrialGenuine);
end;

function Thin_ActivateLocalTrial(trialLength: LongWord): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'ActivateLocalTrial';

function ActivateLocalTrial(TrialLength: LongWord): TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_ActivateLocalTrial(TrialLength));
end;

function Thin_IsLocalTrialGenuine: TLAStatusCode; cdecl;
  external LexActivator_DLL name 'IsLocalTrialGenuine';

function IsLocalTrialGenuine: TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_IsLocalTrialGenuine);
end;

function Thin_ExtendLocalTrial(trialExtensionLength: LongWord): TLAStatusCode; cdecl;
  external LexActivator_DLL name 'ExtendLocalTrial';

function ExtendLocalTrial(TrialExtensionLength: LongWord): TLAKeyStatus;
begin
  Result := ELAError.CheckKeyStatus(Thin_ExtendLocalTrial(TrialExtensionLength));
end;

function Thin_Reset: TLAStatusCode; cdecl;
  external LexActivator_DLL name 'Reset';

procedure LAReset;
begin
  if not ELAError.CheckOKFail(Thin_Reset) then
    raise
    ELAFailException.Create('Failed to reset the activation and trial data');
end;

class function ELAError.CreateByCode(ErrorCode: TLAStatusCode): ELAError;
begin
  case ErrorCode of
    LA_OK: Result := nil;
    LA_FAIL: Result := ELAFailException.Create;
    LA_EXPIRED: Result := ELAExpiredError.Create;
    LA_SUSPENDED: Result := ELASuspendedError.Create;
    LA_GRACE_PERIOD_OVER: Result := ELAGracePeriodOverError.Create;
    LA_TRIAL_EXPIRED: Result := ELATrialExpiredError.Create;
    LA_LOCAL_TRIAL_EXPIRED: Result := ELALocalTrialExpiredError.Create;
    LA_E_FILE_PATH: Result := ELAFilePathException.Create;
    LA_E_PRODUCT_FILE: Result := ELAProductFileException.Create;
    LA_E_PRODUCT_DATA: Result := ELAProductDataException.Create;
    LA_E_PRODUCT_ID: Result := ELAProductIdException.Create;
    LA_E_SYSTEM_PERMISSION: Result := ELASystemPermissionException.Create;
    LA_E_FILE_PERMISSION: Result := ELAFilePermissionException.Create;
    LA_E_WMIC: Result := ELAWMICException.Create;
    LA_E_TIME: Result := ELATimeException.Create;
    LA_E_INET: Result := ELAInetException.Create;
    LA_E_NET_PROXY: Result := ELANetProxyException.Create;
    LA_E_HOST_URL: Result := ELAHostURLException.Create;
    LA_E_BUFFER_SIZE: Result := ELABufferSizeException.Create;
    LA_E_APP_VERSION_LENGTH: Result := ELAAppVersionLengthException.Create;
    LA_E_REVOKED: Result := ELARevokedException.Create;
    LA_E_LICENSE_KEY: Result := ELALicenseKeyException.Create;
    LA_E_LICENSE_TYPE: Result := ELALicenseTypeException.Create;
    LA_E_OFFLINE_RESPONSE_FILE: Result := ELAOfflineResponseFileException.Create;
    LA_E_OFFLINE_RESPONSE_FILE_EXPIRED: Result := ELAOfflineResponseFileExpiredException.Create;
    LA_E_ACTIVATION_LIMIT: Result := ELAActivationLimitException.Create;
    LA_E_ACTIVATION_NOT_FOUND: Result := ELAActivationNotFoundException.Create;
    LA_E_DEACTIVATION_LIMIT: Result := ELADeactivationLimitException.Create;
    LA_E_TRIAL_NOT_ALLOWED: Result := ELATrialNotAllowedException.Create;
    LA_E_TRIAL_ACTIVATION_LIMIT: Result := ELATrialActivationLimitException.Create;
    LA_E_MACHINE_FINGERPRINT: Result := ELAMachineFingerprintException.Create;
    LA_E_METADATA_KEY_LENGTH: Result := ELAMetadataKeyLengthException.Create;
    LA_E_METADATA_VALUE_LENGTH: Result := ELAMetadataValueLengthException.Create;
    LA_E_ACTIVATION_METADATA_LIMIT: Result := ELAActivationMetadataLimitException.Create;
    LA_E_TRIAL_ACTIVATION_METADATA_LIMIT: Result := ELATrialActivationMetadataLimitException.Create;
    LA_E_METADATA_KEY_NOT_FOUND: Result := ELAMetadataKeyNotFoundException.Create;
    LA_E_TIME_MODIFIED: Result := ELATimeModifiedException.Create;
    LA_E_VM: Result := ELAVMException.Create;
    LA_E_COUNTRY: Result := ELACountryException.Create;
    LA_E_IP: Result := ELAIPException.Create;
    LA_E_RATE_LIMIT: Result := ELARateLimitException.Create;
    LA_E_SERVER: Result := ELAServerException.Create;
    LA_E_CLIENT: Result := ELAClientException.Create;

  else
    Result := ELAUnknownErrorCodeException.Create(ErrorCode);
  end;
end;

// check for LA_OK, otherwise raise exception
class procedure ELAError.Check(ErrorCode: TLAStatusCode);
begin
  // E2441 Inline function declared in interface section must not use local
  // symbol 'LA_OK'.
  if ErrorCode <> 0 {LA_OK} then
    raise CreateByCode(ErrorCode);
end;

class function ELAError.CheckOKFail(ErrorCode: TLAStatusCode): Boolean;
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

class function ELAError.CheckKeyStatus(ErrorCode: TLAStatusCode): TLAKeyStatus;
begin
  case ErrorCode of
    LA_OK: Result := lkOK;
    LA_EXPIRED: Result := lkExpired;
    LA_SUSPENDED: Result := lkSuspended;
    LA_GRACE_PERIOD_OVER: Result := lkGracePeriodOver;
    LA_TRIAL_EXPIRED: Result := lkTrialExpired;
    LA_LOCAL_TRIAL_EXPIRED: Result := lkLocalTrialExpired;
    LA_FAIL: Result := lkFail;
  else
    raise CreateByCode(ErrorCode);
  end;
end;

constructor ELAUnknownErrorCodeException.Create(AErrorCode: TLAStatusCode);
begin
  inherited CreateFmt('LexActivator error %d', [AErrorCode]);
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
  FKeyStatus := lkFail;
end;

class function ELAKeyStatusError.CreateByKeyStatus(KeyStatus: TLAKeyStatus): ELAError;
begin
  case KeyStatus of
    lkOK: Result := nil;
    lkExpired: Result := ELAExpiredError.Create;
    lkSuspended: Result := ELASuspendedError.Create;
    lkGracePeriodOver: Result := ELAGracePeriodOverError.Create;
    lkTrialExpired: Result := ELATrialExpiredError.Create;
    lkLocalTrialExpired: Result := ELALocalTrialExpiredError.Create;
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
      Result := ELAKeyStatusError.CreateFmt('Key state is %d',
        [Integer(KeyStatus)]);
      ELAKeyStatusError(Result).FKeyStatus := KeyStatus;
    end;
  end;
end;

constructor ELAExpiredError.Create;
begin
  inherited Create('The license has expired or system time has been tampered ' +
    'with. Ensure your date and time settings are correct');
  FErrorCode := LA_EXPIRED;
  FKeyStatus := lkExpired;
end;

constructor ELASuspendedError.Create;
begin
  inherited Create('The license has been suspended');
  FErrorCode := LA_SUSPENDED;
  FKeyStatus := lkSuspended;
end;

constructor ELAGracePeriodOverError.Create;
begin
  inherited Create('The grace period for server sync is over');
  FErrorCode := LA_GRACE_PERIOD_OVER;
  FKeyStatus := lkGracePeriodOver;
end;

constructor ELATrialExpiredError.Create;
begin
  inherited Create('The trial has expired or system time has been tampered ' +
    'with. Ensure your date and time settings are correct');
  FErrorCode := LA_TRIAL_EXPIRED;
  FKeyStatus := lkTrialExpired;
end;

constructor ELALocalTrialExpiredError.Create;
begin
  inherited Create('The local trial has expired or system time has been tampered ' +
    'with. Ensure your date and time settings are correct');
  FErrorCode := LA_LOCAL_TRIAL_EXPIRED;
  FKeyStatus := lkLocalTrialExpired;
end;

constructor ELAFilePathException.Create;
begin
  inherited Create('Invalid file path');
  FErrorCode := LA_E_FILE_PATH;
end;

constructor ELAProductFileException.Create;
begin
  inherited Create('Invalid or corrupted product file');
  FErrorCode := LA_E_PRODUCT_FILE;
end;

constructor ELAProductDataException.Create;
begin
  inherited Create('Invalid product data');
  FErrorCode := LA_E_PRODUCT_DATA;
end;

constructor ELAProductIdException.Create;
begin
  inherited Create('The product id is incorrect');
  FErrorCode := LA_E_PRODUCT_ID;
end;

constructor ELASystemPermissionException.Create;
begin
  inherited Create('Insufficent system permissions');
  FErrorCode := LA_E_SYSTEM_PERMISSION;
end;

constructor ELAFilePermissionException.Create;
begin
  inherited Create('No permission to write to file');
  FErrorCode := LA_E_FILE_PERMISSION;
end;

constructor ELAWMICException.Create;
begin
  inherited Create('Fingerprint couldn''t be generated because Windows Management ' +
    'Instrumentation (WMI) service has been disabled');
  FErrorCode := LA_E_WMIC;
end;

constructor ELATimeException.Create;
begin
  inherited Create('The difference between the network time and the system time is ' +
    'more than allowed clock offset.');
  FErrorCode := LA_E_TIME;
end;

constructor ELAInetException.Create;
begin
  inherited Create('Failed to connect to the server due to network error');
  FErrorCode := LA_E_INET;
end;

constructor ELANetProxyException.Create;
begin
  inherited Create('Invalid network proxy');
  FErrorCode := LA_E_NET_PROXY;
end;

constructor ELAHostURLException.Create;
begin
  inherited Create('Invalid Cryptlex host url');
  FErrorCode := LA_E_HOST_URL;
end;

constructor ELABufferSizeException.Create;
begin
  inherited Create('The buffer size was smaller than required');
  FErrorCode := LA_E_BUFFER_SIZE;
end;

constructor ELAAppVersionLengthException.Create;
begin
  inherited Create('App version length is more than 256 characters');
  FErrorCode := LA_E_APP_VERSION_LENGTH;
end;

constructor ELARevokedException.Create;
begin
  inherited Create('The license has been revoked');
  FErrorCode := LA_E_REVOKED;
end;

constructor ELALicenseKeyException.Create;
begin
  inherited Create('Invalid license key');
  FErrorCode := LA_E_LICENSE_KEY;
end;

constructor ELALicenseTypeException.Create;
begin
  inherited Create('Invalid license type. Make sure floating license ' +
    'is not being used');
  FErrorCode := LA_E_LICENSE_TYPE;
end;

constructor ELAOfflineResponseFileException.Create;
begin
  inherited Create('Invalid offline activation response file');
  FErrorCode := LA_E_OFFLINE_RESPONSE_FILE;
end;

constructor ELAOfflineResponseFileExpiredException.Create;
begin
  inherited Create('The offline activation response has expired');
  FErrorCode := LA_E_OFFLINE_RESPONSE_FILE_EXPIRED;
end;

constructor ELAActivationLimitException.Create;
begin
  inherited Create('The license has reached its allowed activations limit');
  FErrorCode := LA_E_ACTIVATION_LIMIT;
end;

constructor ELAActivationNotFoundException.Create;
begin
  inherited Create('The license activation was deleted on the server');
  FErrorCode := LA_E_ACTIVATION_NOT_FOUND;
end;

constructor ELADeactivationLimitException.Create;
begin
  inherited Create('The license has reached its allowed deactivations limit');
  FErrorCode := LA_E_DEACTIVATION_LIMIT;
end;

constructor ELATrialNotAllowedException.Create;
begin
  inherited Create('Trial not allowed for the product');
  FErrorCode := LA_E_TRIAL_NOT_ALLOWED;
end;

constructor ELATrialActivationLimitException.Create;
begin
  inherited Create('Your account has reached its trial activations limit');
  FErrorCode := LA_E_TRIAL_ACTIVATION_LIMIT;
end;

constructor ELAMachineFingerprintException.Create;
begin
  inherited Create('Machine fingerprint has changed since activation');
  FErrorCode := LA_E_MACHINE_FINGERPRINT;
end;

constructor ELAMetadataKeyLengthException.Create;
begin
  inherited Create('Metadata key length is more than 256 characters');
  FErrorCode := LA_E_METADATA_KEY_LENGTH;
end;

constructor ELAMetadataValueLengthException.Create;
begin
  inherited Create('Metadata value length is more than 256 characters');
  FErrorCode := LA_E_METADATA_VALUE_LENGTH;
end;

constructor ELAActivationMetadataLimitException.Create;
begin
  inherited Create('The license has reached its metadata fields limit');
  FErrorCode := LA_E_ACTIVATION_METADATA_LIMIT;
end;

constructor ELATrialActivationMetadataLimitException.Create;
begin
  inherited Create('The trial has reached its metadata fields limit');
  FErrorCode := LA_E_TRIAL_ACTIVATION_METADATA_LIMIT;
end;

constructor ELAMetadataKeyNotFoundException.Create;
begin
  inherited Create('The metadata key does not exist');
  FErrorCode := LA_E_METADATA_KEY_NOT_FOUND;
end;

constructor ELATimeModifiedException.Create;
begin
  inherited Create('The system time has been tampered (backdated)');
  FErrorCode := LA_E_TIME_MODIFIED;
end;

constructor ELAVMException.Create;
begin
  inherited Create('Application is being run inside a virtual machine / hypervisor, ' +
    'and activation has been disallowed in the VM');
  FErrorCode := LA_E_VM;
end;

constructor ELACountryException.Create;
begin
  inherited Create('Country is not allowed');
  FErrorCode := LA_E_COUNTRY;
end;

constructor ELAIPException.Create;
begin
  inherited Create('IP address is not allowed');
  FErrorCode := LA_E_IP;
end;

constructor ELARateLimitException.Create;
begin
  inherited Create('Rate limit for API has reached, try again later');
  FErrorCode := LA_E_RATE_LIMIT;
end;

constructor ELAServerException.Create;
begin
  inherited Create('Server error');
  FErrorCode := LA_E_SERVER;
end;

constructor ELAClientException.Create;
begin
  inherited Create('Client error');
  FErrorCode := LA_E_CLIENT;
end;

initialization
  InitializeCriticalSection(LALicenseCallbackMutex);
finalization
  try ResetLicenseCallback; except end;
  DeleteCriticalSection(LALicenseCallbackMutex);
end.

