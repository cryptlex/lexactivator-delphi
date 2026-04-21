{$IFNDEF FPC}
  {$WARN UNSAFE_TYPE OFF} // Pointer
{$ENDIF}

unit LexActivator.DelphiFeatures;

interface

{$IFDEF FPC}
  {$DEFINE DELPHI_HAS_UINT64}
  {$DEFINE DELPHI_HAS_INLINE}
  {$DEFINE DELPHI_CLASS_CAN_BE_ABSTRACT}
  {$DEFINE DELPHI_HAS_RECORDS}
  {$DEFINE DELPHI_IS_UNICODE}
  {$DEFINE DELPHI_HAS_RTTI}
  {$DEFINE DELPHI_HAS_INTPTR}
{$ELSE}
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
  {$DEFINE DELPHI_HAS_UNITNAME}
{$IFEND}

{$IF CompilerVersion >= 21.0}
  {$DEFINE DELPHI_HAS_RTTI}
{$IFEND}

{$IF CompilerVersion >= 23.0}
  {$DEFINE DELPHI_HAS_INTPTR}
  {$DEFINE DELPHI_UNITS_SCOPED}
{$IFEND}
{$ENDIF}

{$IFNDEF DELPHI_IS_UNICODE}
type
  UnicodeString = type WideString;

function UTF8ToUnicodeString(const S: UTF8String): UnicodeString; {$IFDEF DELPHI_HAS_INLINE} inline; {$ENDIF}
// Decode is deprecated, Encode is not
// because Encode can decide on its argument type

{$ENDIF}

function TClass_UnitName(Item: TClass): string;

{$IFNDEF DELPHI_HAS_UINT64}
type
  UInt64 = type Int64;
{$ENDIF}

{$IFNDEF DELPHI_HAS_INTPTR}
type
  IntPtr = type LongInt;
  UIntPtr = type LongWord;
{$ENDIF}

implementation

uses
{$IFDEF DELPHI_UNITS_SCOPED}
  System.TypInfo
{$ELSE}
  TypInfo
{$ENDIF}
  ;

{$IFNDEF DELPHI_IS_UNICODE}
function UTF8ToUnicodeString(const S: UTF8String): UnicodeString;
begin
  Result := UTF8Decode(S);
end;
{$ENDIF}

function TClass_UnitName(Item: TClass): string;
{$IFDEF DELPHI_HAS_UNITNAME}
begin
  Result := Item.UnitName;
end;
{$ELSE}
{$IFDEF FPC}
begin
  // FPC TypInfo layout differs from Delphi; return stable fallback.
  if Assigned(Item) then
    Result := Item.ClassName
  else
    Result := '';
end;
{$ELSE}
var
  ClassInfo: Pointer;
  TypeData: PTypeData;
begin
  Result := '';
  if not Assigned(Item) then Exit;
  ClassInfo := Item.ClassInfo;
  if not Assigned(ClassInfo) then Exit;
  TypeData := GetTypeData(Item.ClassInfo);
  if not Assigned(TypeData) then Exit;
  Result := TypeData.UnitName;
end;
{$ENDIF}
{$ENDIF}

end.
