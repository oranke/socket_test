{-----------------------------------------------------------------------------
 Unit Name: zhKeyList
 Author:    oranke_f
 Date:      2010-10-3
 Purpose:   키로 자료를 저장하는 구조 정의. 정수키 리스트. 캐시 리스트 등.
 History:

  2010-03-10
    중복된 정의를 막기 위해 객체지향적으로 구현 해 본다.
    TBaseKeyList 에서 파생된 U32, I64, Str, Text 키리스트를 구현.
    Iterate 기능 추가.

  2010-03-11
    필요하면 유니트 외부에서도 상속받을 수 있도록 TBaseKeyList의 변수를 정리.      

  2010-04-22
    String  -> AnsiStrion 으로
    PString -> PAnsiString 으로.
    PChar   -> PAnsiChar 로.

    해싱함수인 MemHash3에 기본값을 줄 수 있도록 함.
    여러개의 메모리를 굳이 연결하지 않고도 연결된것으로 간주해 해싱할 수 있음.       

  2010-05-04
    TWideStrKeyList 추가.


-----------------------------------------------------------------------------}

unit zhKeyList;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  Windows, SysUtils,

{$IFDEF VER300}
  System.AnsiStrings,
{$ENDIF}

  zhTypes;

type

//---------------------------------------------------------------------------
  PPNodePtr = ^PNodePtr;
  PNodePtr  = Pointer;
  PKeyPtr   = Pointer;

  PPDataPtr = ^PDataPtr;
  PDataPtr  = Pointer;


  // 이터레이터 함수.
  TKeyIterateFunc = function(aUserData: Pointer; aKeyPtr: Pointer;
    aDataPPtr: PPDataPtr): Boolean;
  TKeyIterateMethod = function(aUserData: Pointer; aKeyPtr: Pointer;
    aDataPPtr: PPDataPtr): Boolean of object;

  // 최상위 버켓의 배열형.
  PBucketArray = ^TBucketArray;
  TBucketArray = array [0..MaxInt div SizeOf(PNodePtr) - 1] of PNodePtr;

  // 베이스 키 리스트.
  TBaseKeyList = class
  private
    fBucketArray: PBucketArray;
    fBucketCount: U32;    // 버킷의 갯수
    fDataCount: U32;      // 리스트에 저장된 데이터의 갯수
    fLeftDelete: Boolean;

    function FindNodePtr(const aKeyPtr: PKeyPtr): PPNodePtr;

    function IterateNode(const aNode: PNodePtr; const aUserData: Pointer;
      const aIterateFunc: TKeyIterateFunc): Boolean;
    function IterateMethodNode(const aNode: PNodePtr; const aUserData: Pointer;
      const aIterateMethod: TKeyIterateMethod): Boolean;

    procedure DeleteNode(var q: PNodePtr);
    procedure DeleteNodes(var q: PNodePtr);

    function GetKeyPtrData(const aKeyPtr: PKeyPtr): Pointer;
    procedure SetKeyPtrData(const aKeyPtr: PKeyPtr; const Value: Pointer);
  protected
    // 키 포인터에서 버킷값 얻기.
    function GetBucketIdx(const aKeyPtr: PKeyPtr): U32; virtual; abstract;

    // 검색키와 노드내 키의 비교. 검색키가 크면 양수, 작으면 음수, 같으면 0 리턴.
    function CompareNode(const aKeyPtr: PKeyPtr; const aNodePtr: PNodePtr): I32; virtual; abstract;

    function GetKeyPtr(const aNodePtr: PNodePtr): PKeyPtr; virtual; abstract;
    function GetDataPPtr(const aNodePtr: PNodePtr): PPDataPtr; virtual; abstract;
    function GetLNodePPtr(const aNodePtr: PNodePtr): PPNodePtr; virtual; abstract;
    function GetRNodePPtr(aNodePtr: PNodePtr): PPNodePtr; virtual; abstract;

    procedure SetKeyPtr(const aNodePtr: PNodePtr; const aKeyPtr: PKeyPtr); virtual; abstract;
    procedure SetDataPtr(const aNodePtr: PNodePtr; aDataPtr: PDataPtr); virtual; abstract;
    procedure SetLNodePtr(const aNodePtr, aLeftPtr: PNodePtr); virtual; abstract;
    procedure SetRNodePtr(const aNodePtr, aRightPtr: PNodePtr); virtual; abstract;

    function AllocNode: PNodePtr; virtual; abstract;
    procedure FreeNode(aNode: PNodePtr); virtual; abstract;

    procedure AddKeyProc(const aNodePPtr: PPNodePtr; const aKeyPtr: PKeyPtr; const aData);
  protected
    property BucketCount: U32 read fBucketCount;
  public
    constructor Create(const aBucketCount: U32); virtual;
    destructor Destroy; override;

    function AddKey(const aKeyPtr: PKeyPtr; const aData): Boolean; overload; 
    function AddKey(const aKeyPtr: PKeyPtr; const aDataPtr: Pointer): Boolean; overload; 
    function RemoveKey(const aKeyPtr: PKeyPtr): PDataPtr;
    function HasKey(const aKeyPtr: PKeyPtr): Boolean;
    //function GetKeyData(const aKeyPtr: PKeyPtr): PDataPtr;

    procedure Clear;

    procedure Iterate(aUserData: Pointer; aIterateFunc: TKeyIterateFunc);
    procedure IterateMethod(aUserData: Pointer; aIterateMethod: TKeyIterateMethod);

  public
    property Count: U32 read fDataCount;
    property KeyPtrData[const aKeyPtr: PKeyPtr]: Pointer read GetKeyPtrData write SetKeyPtrData;
  end;


//---------------------------------------------------------------------------
// 언사인드 정수형 키 리스트.
// 해시리스트 기법을 차용, U32키를 사용해 자료를 관리하는 데이터구조.

  PPU32Node = ^PU32Node;
  PU32Node = ^TU32Node;
  TU32Node = record
    Key  : U32;
    Data : Pointer;
    Left : PU32Node;
    Right: PU32Node;
  end;

  TU32KeyList = class(TBaseKeyList)
  private
    function GetData(const aKey: U32): Pointer;
    procedure SetData(const aKey: U32; const Value: Pointer);
  protected
    function GetBucketIdx(const aKeyPtr: PKeyPtr): U32; override;

    function CompareNode(const aKeyPtr: PKeyPtr; const aNodePtr: PNodePtr): I32; override;

    function GetKeyPtr(const aNodePtr: PNodePtr): PKeyPtr; override;
    function GetDataPPtr(const aNodePtr: PNodePtr): PPDataPtr; override;
    function GetLNodePPtr(const aNodePtr: PNodePtr): PPNodePtr; override;
    function GetRNodePPtr(aNodePtr: PNodePtr): PPNodePtr; override;

    procedure SetKeyPtr(const aNodePtr: PNodePtr; const aKeyPtr: PKeyPtr); override;
    procedure SetDataPtr(const aNodePtr: PNodePtr; aDataPtr: PDataPtr); override;
    procedure SetLNodePtr(const aNodePtr, aLeftPtr: PNodePtr); override;
    procedure SetRNodePtr(const aNodePtr, aRightPtr: PNodePtr); override;

    function AllocNode: PNodePtr; override;
    procedure FreeNode(aNode: PNodePtr); override;
  public
    function Add(const aKey: U32; const aData{: Pointer}): Boolean;
    function Has(const aKey: U32): Boolean;
    function Remove(const aKey: U32): PDataPtr;
  public
    property Data[const aKey: U32]: Pointer read GetData write SetData; default;
  end;


//---------------------------------------------------------------------------
// Int64 키 리스트.

  PPI64Node = ^PI64Node;
  PI64Node = ^TI64Node;
  TI64Node = record
    Key  : I64;
    Data : Pointer;
    Left : PI64Node;
    Right: PI64Node;
  end;

  TI64KeyList = class(TBaseKeyList)
  private
    function GetData(const aKey: I64): Pointer;
    procedure SetData(const aKey: I64; const Value: Pointer);
  protected
    function GetBucketIdx(const aKeyPtr: PKeyPtr): U32; override;

    function CompareNode(const aKeyPtr: PKeyPtr; const aNodePtr: PNodePtr): I32; override;

    function GetKeyPtr(const aNodePtr: PNodePtr): PKeyPtr; override;
    function GetDataPPtr(const aNodePtr: PNodePtr): PPDataPtr; override;
    function GetLNodePPtr(const aNodePtr: PNodePtr): PPNodePtr; override;
    function GetRNodePPtr(aNodePtr: PNodePtr): PPNodePtr; override;

    procedure SetKeyPtr(const aNodePtr: PNodePtr; const aKeyPtr: PKeyPtr); override;
    procedure SetDataPtr(const aNodePtr: PNodePtr; aDataPtr: PDataPtr); override;
    procedure SetLNodePtr(const aNodePtr, aLeftPtr: PNodePtr); override;
    procedure SetRNodePtr(const aNodePtr, aRightPtr: PNodePtr); override;

    function AllocNode: PNodePtr; override;
    procedure FreeNode(aNode: PNodePtr); override;
  public
    function Add(const aKey: I64; const aData{: Pointer}): Boolean;
    function Has(const aKey: I64): Boolean;
    function Remove(const aKey: I64): PDataPtr;
  public
    property Data[const aKey: I64]: Pointer read GetData write SetData; default;
  end;


//---------------------------------------------------------------------------
// Str 키 리스트. 대소문자 구분함.

  // 이 구조체 키 문자열의 자동해제를 위해 AllocNode, FreeNode 에서 New, Dispose를 사용한다.
  PPStrNode = ^PStrNode;
  PStrNode = ^TStrNode;
  TStrNode = record
    Key  : AnsiString;
    Data : Pointer;
    Left : PStrNode;
    Right: PStrNode;
  end;

  TStrKeyList = class(TBaseKeyList)
  private
    function GetData(const aKey: AnsiString): Pointer; virtual;
    procedure SetData(const aKey: AnsiString; const Value: Pointer); virtual;
  protected
    function GetBucketIdx(const aKeyPtr: PKeyPtr): U32; override;

    function CompareNode(const aKeyPtr: PKeyPtr; const aNodePtr: PNodePtr): I32; override;

    function GetKeyPtr(const aNodePtr: PNodePtr): PKeyPtr; override;
    function GetDataPPtr(const aNodePtr: PNodePtr): PPDataPtr; override;
    function GetLNodePPtr(const aNodePtr: PNodePtr): PPNodePtr; override;
    function GetRNodePPtr(aNodePtr: PNodePtr): PPNodePtr; override;

    procedure SetKeyPtr(const aNodePtr: PNodePtr; const aKeyPtr: PKeyPtr); override;
    procedure SetDataPtr(const aNodePtr: PNodePtr; aDataPtr: PDataPtr); override;
    procedure SetLNodePtr(const aNodePtr, aLeftPtr: PNodePtr); override;
    procedure SetRNodePtr(const aNodePtr, aRightPtr: PNodePtr); override;

    function AllocNode: PNodePtr; override;
    procedure FreeNode(aNode: PNodePtr); override;
  public
    function Add(const aKey: AnsiString; const aData): Boolean; overload; virtual;
    //function Add(const aKey: AnsiString; const aDataPtr: Pointer): Boolean; overload; virtual;
    function Has(const aKey: AnsiString): Boolean; virtual;
    function Remove(const aKey: AnsiString): PDataPtr; virtual;
  public
    property Data[const aKey: AnsiString]: Pointer read GetData write SetData; default;
  end;

//---------------------------------------------------------------------------
// Text 키 리스트. 대소문자 구분 없음.

  TTextKeyList = class(TStrKeyList)
  private
    function GetData(const aKey: AnsiString): Pointer; override;
    procedure SetData(const aKey: AnsiString; const Value: Pointer); override;
  protected
  public
    function Add(const aKey: AnsiString; const aData): Boolean; override;
    function Has(const aKey: AnsiString): Boolean; override;
    function Remove(const aKey: AnsiString): PDataPtr; override;
  end;

//---------------------------------------------------------------------------
// WStr 키 리스트. 대소문자 구분함.

  PPWideStrNode = ^PWideStrNode;
  PWideStrNode = ^TWideStrNode;
  TWideStrNode = record
    Key  : WideString;
    Data : Pointer;
    Left : PStrNode;
    Right: PStrNode;
  end;

  TWideStrKeyList = class(TBaseKeyList)
  private
    function GetData(const aKey: WideString): Pointer; virtual;
    procedure SetData(const aKey: WideString; const Value: Pointer); virtual;
  protected
    function GetBucketIdx(const aKeyPtr: PKeyPtr): U32; override;

    function CompareNode(const aKeyPtr: PKeyPtr; const aNodePtr: PNodePtr): I32; override;

    function GetKeyPtr(const aNodePtr: PNodePtr): PKeyPtr; override;
    function GetDataPPtr(const aNodePtr: PNodePtr): PPDataPtr; override;
    function GetLNodePPtr(const aNodePtr: PNodePtr): PPNodePtr; override;
    function GetRNodePPtr(aNodePtr: PNodePtr): PPNodePtr; override;

    procedure SetKeyPtr(const aNodePtr: PNodePtr; const aKeyPtr: PKeyPtr); override;
    procedure SetDataPtr(const aNodePtr: PNodePtr; aDataPtr: PDataPtr); override;
    procedure SetLNodePtr(const aNodePtr, aLeftPtr: PNodePtr); override;
    procedure SetRNodePtr(const aNodePtr, aRightPtr: PNodePtr); override;

    function AllocNode: PNodePtr; override;
    procedure FreeNode(aNode: PNodePtr); override;
  public
    function Add(const aKey: WideString; const aData): Boolean; virtual;
    function Has(const aKey: WideString): Boolean; virtual;
    function Remove(const aKey: WideString): PDataPtr; virtual;
  public
    property Data[const aKey: WideString]: Pointer read GetData write SetData; default;
  end;



// 해싱 함수들.
function StrHash(const s: AnsiString): U32;
function WideStrHash(const ws: WideString): U32;
//nction TextHash(const s: AnsiString): U32;

function MemHash1(const aMem: Pointer; aSize: I32): U32;
function MemHash2(const aMem: Pointer; const aSize: I32): U32;
function MemHash3(const aMem: Pointer; const aSize: I32; const aStart: U32 = 0): U32;


implementation


function StrHash(const s: AnsiString): U32;
begin
  //Result := MemHash(PAnsiChar(s), Length(s));
  Result := MemHash3(PAnsiChar(s), Length(s));
end;

function WideStrHash(const ws: WideString): U32;
begin
  Result := MemHash3(PWideChar(ws), Length(ws) * SizeOf(WideChar)); 
end;

// 기본적인 Hash 코드. 
function MemHash1(const aMem: Pointer; aSize: I32): U32;
const
  C_LongBits = 32;
  C_OneEight = 4;
  C_ThreeFourths = 24;
  C_HighBits = $F0000000;
var
  p: PAnsiChar;
  temp: Cardinal;
begin
  Result := 0;
  p := aMem;

  while aSize > 0 do
  begin
    Result := (Result shl C_OneEight) + Ord(p^);
    temp := Result and C_HighBits;
    if temp <> 0 then
      Result := (Result xor (temp shr C_ThreeFourths)) and (not C_HighBits);
    Dec(aSize);
    Inc(p);
  end;
end;

// 감자님의 해싱 코드
// http://www.gpgstudy.com/forum/viewtopic.php?topic=795
// 출처는 http://www.cs.yorku.ca/~oz/hash.html 의 djb2 방식.
(*
unsigned long GetHashCode( const char* PString )
{
	   unsigned long i,len;
	   unsigned long ch;
	   unsigned long result;

	   len     = strlen( PString );
	   result = 5381;
	   for( i=0; i<len; i++ )
	   {
	   	   ch = (unsigned long)PString[i];
	   	   result = ((result<< 5) + result) + ch; // hash * 33 + ch
	   }

	   return result;
}
*)
// 위 코드를 사용함. 테스트 결과 약간 약간 나은 정도.
function MemHash2(const aMem: Pointer; const aSize: I32): U32;
var
  i: I32;
begin
  Result := 5381;
  for i:= 0 to aSize -1 do
    Result := ((Result shl 5) + Result) + PU8Array(aMem)[i]; // hash * 33 + ch
end;

// http://www.cs.yorku.ca/~oz/hash.html 의 sdbm 방식.
// 가장 효율이 좋다고 알려짐. 실제 테스트 해 본 결과는 썩 괜찮군..
// 이 물건을 표준으로 사용하세~~
function MemHash3(const aMem: Pointer; const aSize: I32; const aStart: U32): U32;
var
  i: I32;
begin
  Result := aStart;
  for i:= 0 to aSize-1 do
    Result := PU8Array(aMem)[i] + (Result shl 6) + (Result shl 16) - Result;
end;



{ TBaseKeyList }

constructor TBaseKeyList.Create(const aBucketCount: U32);
begin
  if aBucketCount < 4 then fBucketCount := 4 else fBucketCount := aBucketCount;
  fDataCount := 0;

  fBucketArray:= AllocMem(SizeOf(PNodePtr) * fBucketCount);
end;

destructor TBaseKeyList.Destroy;
begin
  Clear;

  FreeMem(fBucketArray);
  inherited;
end;


function TBaseKeyList.FindNodePtr(const aKeyPtr: PKeyPtr): PPNodePtr;
var
  i : I32;
  ppn : PPNodePtr;
begin
  i := GetBucketIdx(aKeyPtr);// 

  ppn := @fBucketArray[i];

  if ppn^ <> nil then
  while true do
  begin
    i := CompareNode(aKeyPtr, ppn^);

    // 검색키가 노드내 키보다 작으면 왼쪽으로
    if i < 0 then
    begin
      ppn := GetLNodePPtr(ppn^);
    end else
    // 검색키가 노드내 키보다 크면 오른쪽으로
    if i > 0 then
    begin
      ppn := GetRNodePPtr(ppn^);
    end else
      Break;
    
    if ppn^ = nil then
      Break;
  end;

  Result := ppn;
end;

function TBaseKeyList.IterateNode(const aNode: PNodePtr; const aUserData: Pointer;
  const aIterateFunc: TKeyIterateFunc): Boolean;
begin
  if aNode <> nil then
  begin

    Result := aIterateFunc(aUserData, GetKeyPtr(aNode), GetDataPPtr(aNode));
    if not Result then Exit;

    Result := IterateNode(GetLNodePPtr(aNode)^, aUserData, aIterateFunc);
    if not Result then Exit;

    Result := IterateNode(GetRNodePPtr(aNode)^, aUserData, aIterateFunc);
    if not Result then Exit;
  end else
    Result := True;

end;

function TBaseKeyList.IterateMethodNode(const aNode: PNodePtr; const aUserData: Pointer;
  const aIterateMethod: TKeyIterateMethod): Boolean;
begin
  if aNode <> nil then
  begin
    Result := aIterateMethod(aUserData, GetKeyPtr(aNode), GetDataPPtr(aNode));
    if not Result then Exit;

    Result := IterateMethodNode(GetLNodePPtr(aNode)^, aUserData, aIterateMethod);
    if not Result then Exit;

    Result := IterateMethodNode(GetRNodePPtr(aNode)^, aUserData, aIterateMethod);
    if not Result then Exit;
  end else
    Result := True;
end;



procedure TBaseKeyList.DeleteNode(var q: PNodePtr);
var
  t, r, s: PNodePtr;
begin
  fLeftDelete := not fLeftDelete;

  t := q;

  if FLeftDelete then
  begin
    if GetRNodePPtr(t)^ = nil then
      q := GetLNodePPtr(t)^
    else
    begin
      r := GetRNodePPtr(t)^;
      if GetLNodePPtr(r)^ = nil then
      begin
        SetLNodePtr(r, GetLNodePPtr(t)^);
        q := r;
      end else
      begin
        s := GetLNodePPtr(r)^;
        if GetLNodePPtr(s)^ <> nil then
          repeat
            r := s;
            s := GetLNodePPtr(r)^;
          until GetLNodePPtr(s)^ = nil;
        SetLNodePtr(s, GetLNodePPtr(t)^);
        SetLNodePtr(r, GetRNodePPtr(s)^);
        SetRNodePtr(s, GetRNodePPtr(t)^);
        q := s;
      end;
    end;
  end else
  begin
    if GetLNodePPtr(t)^ = nil then
      q := GetRNodePPtr(t)^
    else
    begin
      r := GetLNodePPtr(t)^;
      if GetRNodePPtr(r)^ = nil then
      begin
        SetRNodePtr(r, GetRNodePPtr(t)^);
        q := r;
      end else
      begin
        s := GetRNodePPtr(r)^;
        if GetRNodePPtr(s)^ <> nil then
          repeat
            r := s;
            s := GetRNodePPtr(r)^;
          until GetRNodePPtr(s)^ = nil;
        { now, s = symmetric predecessor of q }
        SetRNodePtr(s, GetRNodePPtr(t)^);
        SetRNodePtr(r, GetLNodePPtr(s)^);
        SetLNodePtr(s, GetLNodePPtr(t)^);
        q := s;
      end;
    end;
  end;

  Dec(fDataCount);
  FreeNode(t);
end;

procedure TBaseKeyList.DeleteNodes(var q: PNodePtr);
var
  pn: PNodePtr;
begin
  pn := GetLNodePPtr(q)^;
  if pn <> nil then DeleteNodes(pn);

  pn := GetRNodePPtr(q)^;
  if pn <> nil then DeleteNodes(pn);

  FreeNode(q);
  q := nil;
end;

function TBaseKeyList.GetKeyPtrData(const aKeyPtr: PKeyPtr): Pointer;
var
  ppn: PPNodePtr;
begin
  ppn := FindNodePtr(aKeyPtr);

  if ppn^ <> nil then
    Result := GetDataPPtr(ppn^)^
  else
    Result := nil;
end;

procedure TBaseKeyList.SetKeyPtrData(const aKeyPtr: PKeyPtr; const Value: Pointer);
var
  ppn: PPNodePtr;
begin
  ppn := FindNodePtr(aKeyPtr);

  if ppn^ <> nil then
    SetDataPtr(ppn^, Value)
  else
    AddKeyProc(ppn, aKeyPtr, Value);
end;


procedure TBaseKeyList.AddKeyProc(const aNodePPtr: PPNodePtr;
  const aKeyPtr: PKeyPtr; const aData);
begin
  aNodePPtr^ := AllocNode;
  Inc(fDataCount);
  SetKeyPtr(aNodePPtr^, aKeyPtr);
  SetDataPtr(aNodePPtr^, PDataPtr(aData));
end;


function TBaseKeyList.AddKey(const aKeyPtr: PKeyPtr; const aData): Boolean;
var
  ppn: PPNodePtr;
begin
  Result := false;

  ppn := FindNodePtr(aKeyPtr);

  //WriteLn('AddKey : ', Integer(ppn));

  if ppn^ = nil then
  begin
    { add }
    ppn^ := AllocNode;
    { we increment after in case of exception }
    Inc(fDataCount);
    SetKeyPtr(ppn^, aKeyPtr);
    SetDataPtr(ppn^, PDataPtr(aData));

    Result := True;
  end;
end;

function TBaseKeyList.AddKey(const aKeyPtr: PKeyPtr; const aDataPtr: Pointer): Boolean;
var
  ppn: PPNodePtr;
begin
  Result := false;

  ppn := FindNodePtr(aKeyPtr);

  //WriteLn('AddKey : ', Integer(ppn));

  if ppn^ = nil then
  begin
    { add }
    ppn^ := AllocNode;
    { we increment after in case of exception }
    Inc(fDataCount);
    SetKeyPtr(ppn^, aKeyPtr);
    SetDataPtr(ppn^, aDataPtr);

    Result := True;
  end;
end;


function TBaseKeyList.RemoveKey(const aKeyPtr: PKeyPtr): PDataPtr;
var
  ppn: PPNodePtr;
begin
  ppn := FindNodePtr(aKeyPtr);

  if ppn^ <> nil then
  begin
    Result := GetDataPPtr(ppn^)^;
    DeleteNode(ppn^);
  end else
    Result := nil;
end;

function TBaseKeyList.HasKey(const aKeyPtr: PKeyPtr): Boolean;
var
  ppn: PPNodePtr;
begin
  ppn := FindNodePtr(aKeyPtr);
  Result := ppn^ <> nil;
end;
{
function TBaseKeyList.GetKeyData(const aKeyPtr: PKeyPtr): PDataPtr;
var
  ppn: PPNodePtr;
begin
  ppn := FindNodePtr(aKeyPtr);

  if ppn^ <> nil then
    Result := GetDataPtr(ppn^)
  else
    Result := nil;
end;
}
procedure TBaseKeyList.Clear;
var
  i: I32;
  ppn: PPNodePtr;
begin
  for i := 0 to fBucketCount - 1 do
  begin
    ppn := @fBucketArray[i]; 
    if ppn^ <> nil then
      DeleteNodes(ppn^);
  end;
  fDataCount := 0;
end;

procedure TBaseKeyList.Iterate(aUserData: Pointer; aIterateFunc: TKeyIterateFunc);
var
  i: I32;
begin
  for i := 0 to fBucketCount - 1 do
    if not IterateNode(fBucketArray[i], aUserData, aIterateFunc) then
      Break;
end;

procedure TBaseKeyList.IterateMethod(aUserData: Pointer; aIterateMethod: TKeyIterateMethod);
var
  i: I32;
begin
  for i := 0 to fBucketCount - 1 do
    if not IterateMethodNode(fBucketArray[i], aUserData, aIterateMethod) then
      Break;
end;



{ TU32KeyList }


function TU32KeyList.GetBucketIdx(const aKeyPtr: PKeyPtr): U32;
begin
  Result := PU32(aKeyPtr)^ mod BucketCount;
end;

function TU32KeyList.CompareNode(const aKeyPtr: PKeyPtr; const aNodePtr: PNodePtr): I32;
begin
  // 검색키와 노드내 키의 비교. 검색키가 크면 양수, 작으면 음수, 같으면 0 리턴.
  if PU32(aKeyPtr)^ > PU32Node(aNodePtr)^.Key then
    Result := 1
  else
  if PU32(aKeyPtr)^ < PU32Node(aNodePtr)^.Key then
    Result := -1
  else
    Result := 0;
end;

function TU32KeyList.GetKeyPtr(const aNodePtr: PNodePtr): PKeyPtr;
begin
  Result := @PU32Node(aNodePtr).Key
end;

function TU32KeyList.GetDataPPtr(const aNodePtr: PNodePtr): PPDataPtr;
begin
  Result := @PU32Node(aNodePtr).Data;
end;

function TU32KeyList.GetLNodePPtr(const aNodePtr: PNodePtr): PPNodePtr;
begin
  Result := @PU32Node(aNodePtr).Left;
end;

function TU32KeyList.GetRNodePPtr(aNodePtr: PNodePtr): PPNodePtr;
begin
  Result := @PU32Node(aNodePtr).Right;
end;

procedure TU32KeyList.SetKeyPtr(const aNodePtr: PNodePtr; const aKeyPtr: PKeyPtr);
begin
  PU32Node(aNodePtr).Key := PU32(aKeyPtr)^;
end;

procedure TU32KeyList.SetDataPtr(const aNodePtr: PNodePtr; aDataPtr: PDataPtr);
begin
  PU32Node(aNodePtr).Data := aDataPtr;
end;

procedure TU32KeyList.SetLNodePtr(const aNodePtr, aLeftPtr: PNodePtr);
begin
  PU32Node(aNodePtr).Left := aLeftPtr;
end;

procedure TU32KeyList.SetRNodePtr(const aNodePtr, aRightPtr: PNodePtr);
begin
  PU32Node(aNodePtr).Right := aRightPtr;
end;

function TU32KeyList.AllocNode: PNodePtr;
begin
  Result := AllocMem(SizeOf(TU32Node));
  //New(PU32Node(Result));
  //FillChar(Result^, SizeOf(TU32Node), #0);
  //PU32Node(Result)^.Left  := nil;
  //PU32Node(Result)^.Right := nil;
end;

procedure TU32KeyList.FreeNode(aNode: PNodePtr);
begin
  FreeMem(aNode);
  //Dispose(PU32Node(aNode));
end;

function TU32KeyList.GetData(const aKey: U32): Pointer;
begin
  Result := GetKeyPtrData(@aKey);
end;


procedure TU32KeyList.SetData(const aKey: U32; const Value: Pointer);
begin
  SetKeyPtrData(@aKey, Value);
end;

function TU32KeyList.Add(const aKey: U32; const aData): Boolean;
begin
  Result := AddKey(@aKey, aData);
end;

function TU32KeyList.Has(const aKey: U32): Boolean;
begin
  Result := HasKey(@aKey);
end;

function TU32KeyList.Remove(const aKey: U32): PDataPtr;
begin
  Result := RemoveKey(@aKey);
end;



{ TI64KeyList }

function TI64KeyList.GetBucketIdx(const aKeyPtr: PKeyPtr): U32;
begin
  Result := PI64(aKeyPtr)^ mod BucketCount;
end;

function TI64KeyList.CompareNode(const aKeyPtr: PKeyPtr; const aNodePtr: PNodePtr): I32;
begin
  // 검색키와 노드내 키의 비교. 검색키가 크면 양수, 작으면 음수, 같으면 0 리턴.
  if PI64(aKeyPtr)^ > PI64Node(aNodePtr)^.Key then
    Result := 1
  else
  if PI64(aKeyPtr)^ < PI64Node(aNodePtr)^.Key then
    Result := -1
  else
    Result := 0;
end;


function TI64KeyList.GetKeyPtr(const aNodePtr: PNodePtr): PKeyPtr;
begin
  Result := @PI64Node(aNodePtr).Key
end;

function TI64KeyList.GetDataPPtr(const aNodePtr: PNodePtr): PPDataPtr;
begin
  Result := @PI64Node(aNodePtr).Data;
end;

function TI64KeyList.GetLNodePPtr(const aNodePtr: PNodePtr): PPNodePtr;
begin
  Result := @PI64Node(aNodePtr).Left;
end;

function TI64KeyList.GetRNodePPtr(aNodePtr: PNodePtr): PPNodePtr;
begin
  Result := @PI64Node(aNodePtr).Right;
end;

procedure TI64KeyList.SetKeyPtr(const aNodePtr: PNodePtr; const aKeyPtr: PKeyPtr);
begin
  PI64Node(aNodePtr).Key := PI64(aKeyPtr)^;
end;

procedure TI64KeyList.SetDataPtr(const aNodePtr: PNodePtr; aDataPtr: PDataPtr);
begin
  PI64Node(aNodePtr).Data := aDataPtr;
end;

procedure TI64KeyList.SetLNodePtr(const aNodePtr, aLeftPtr: PNodePtr);
begin
  PI64Node(aNodePtr).Left := aLeftPtr;
end;

procedure TI64KeyList.SetRNodePtr(const aNodePtr, aRightPtr: PNodePtr);
begin
  PI64Node(aNodePtr).Right := aRightPtr;
end;

function TI64KeyList.AllocNode: PNodePtr;
begin
  Result := AllocMem(SizeOf(TI64Node));
end;

procedure TI64KeyList.FreeNode(aNode: PNodePtr);
begin
  FreeMem(aNode);
end;

function TI64KeyList.GetData(const aKey: I64): Pointer;
begin
  Result := GetKeyPtrData(@aKey);
end;

procedure TI64KeyList.SetData(const aKey: I64; const Value: Pointer);
begin
  SetKeyPtrData(@aKey, Value);
end;

function TI64KeyList.Add(const aKey: I64; const aData): Boolean;
begin
  Result := AddKey(@aKey, aData);
end;

function TI64KeyList.Has(const aKey: I64): Boolean;
begin
  Result := HasKey(@aKey);
end;

function TI64KeyList.Remove(const aKey: I64): PDataPtr;
begin
  Result := RemoveKey(@aKey);
end;


{ TStrKeyList }

function TStrKeyList.GetBucketIdx(const aKeyPtr: PKeyPtr): U32;
begin
  Result := StrHash(PAnsiString(aKeyPtr)^) mod BucketCount;
end;

function TStrKeyList.CompareNode(const aKeyPtr: PKeyPtr; const aNodePtr: PNodePtr): I32;
begin
  // 검색키와 노드내 키의 비교. 검색키가 크면 양수, 작으면 음수, 같으면 0 리턴.
  Result := AnsiCompareStr(PAnsiString(aKeyPtr)^, PStrNode(aNodePtr)^.Key);
end;

function TStrKeyList.GetKeyPtr(const aNodePtr: PNodePtr): PKeyPtr;
begin
  Result := @PStrNode(aNodePtr).Key
end;

function TStrKeyList.GetDataPPtr(const aNodePtr: PNodePtr): PPDataPtr;
begin
  Result := @PStrNode(aNodePtr).Data;
end;

function TStrKeyList.GetLNodePPtr(const aNodePtr: PNodePtr): PPNodePtr;
begin
  Result := @PStrNode(aNodePtr).Left;
end;

function TStrKeyList.GetRNodePPtr(aNodePtr: PNodePtr): PPNodePtr;
begin
  Result := @PStrNode(aNodePtr).Right;
end;

procedure TStrKeyList.SetKeyPtr(const aNodePtr: PNodePtr; const aKeyPtr: PKeyPtr);
begin
  PStrNode(aNodePtr).Key := PAnsiString(aKeyPtr)^;
end;

procedure TStrKeyList.SetDataPtr(const aNodePtr: PNodePtr; aDataPtr: PDataPtr);
begin
  PStrNode(aNodePtr).Data := aDataPtr;
end;

procedure TStrKeyList.SetLNodePtr(const aNodePtr, aLeftPtr: PNodePtr);
begin
  PStrNode(aNodePtr).Left := aLeftPtr;
end;

procedure TStrKeyList.SetRNodePtr(const aNodePtr, aRightPtr: PNodePtr);
begin
  PStrNode(aNodePtr).Right := aRightPtr;
end;

function TStrKeyList.AllocNode: PNodePtr;
begin
  Result := AllocMem(SizeOf(TStrNode));
  PStrNode(Result)^.Key := '';
  //New(PStrNode(Result));
end;

procedure TStrKeyList.FreeNode(aNode: PNodePtr);
begin
  PStrNode(aNode)^.Key := '';
  FreeMem(aNode);
  //Dispose(PStrNode(aNode));
end;


function TStrKeyList.GetData(const aKey: AnsiString): Pointer;
begin
  Result := GetKeyPtrData(@aKey);
end;

procedure TStrKeyList.SetData(const aKey: AnsiString; const Value: Pointer);
begin
  SetKeyPtrData(@aKey, Value);
end;

function TStrKeyList.Add(const aKey: AnsiString; const aData): Boolean;
begin
  Result := AddKey(@aKey, aData);
end;
{
function TStrKeyList.Add(const aKey: AnsiString; const aDataPtr: Pointer): Boolean;
begin
  Result := AddKey(@aKey, aDataPtr);
end;
}

function TStrKeyList.Has(const aKey: AnsiString): Boolean;
begin
  Result := HasKey(@aKey);
end;

function TStrKeyList.Remove(const aKey: AnsiString): PDataPtr;
begin
  Result := RemoveKey(@aKey);
end;


{ TTextKeyList }

function TTextKeyList.GetData(const aKey: AnsiString): Pointer;
begin
  Result := inherited GetData(UpperCase(aKey));
end;

procedure TTextKeyList.SetData(const aKey: AnsiString; const Value: Pointer);
begin
  inherited SetData(UpperCase(aKey), Value);
end;

function TTextKeyList.Add(const aKey: AnsiString; const aData): Boolean;
begin
  Result := inherited Add(UpperCase(aKey), aData);
end;

function TTextKeyList.Has(const aKey: AnsiString): Boolean;
begin
  Result := inherited Has(UpperCase(aKey));
end;

function TTextKeyList.Remove(const aKey: AnsiString): PDataPtr;
begin
  Result := inherited Remove(UpperCase(aKey));
end;


{ TWideStrKeyList }

function TWideStrKeyList.GetBucketIdx(const aKeyPtr: PKeyPtr): U32;
begin
  Result := WideStrHash(PWideString(aKeyPtr)^) mod BucketCount;
end;

function TWideStrKeyList.CompareNode(const aKeyPtr: PKeyPtr; const aNodePtr: PNodePtr): I32;
begin
  // 검색키와 노드내 키의 비교. 검색키가 크면 양수, 작으면 음수, 같으면 0 리턴.
  Result := WideCompareStr(PWideString(aKeyPtr)^, PWideStrNode(aNodePtr)^.Key);
end;

function TWideStrKeyList.GetKeyPtr(const aNodePtr: PNodePtr): PKeyPtr;
begin
  Result := @PWideStrNode(aNodePtr).Key
end;

function TWideStrKeyList.GetDataPPtr(const aNodePtr: PNodePtr): PPDataPtr;
begin
  Result := @PWideStrNode(aNodePtr).Data;
end;

function TWideStrKeyList.GetLNodePPtr(const aNodePtr: PNodePtr): PPNodePtr;
begin
  Result := @PWideStrNode(aNodePtr).Left;
end;

function TWideStrKeyList.GetRNodePPtr(aNodePtr: PNodePtr): PPNodePtr;
begin
  Result := @PWideStrNode(aNodePtr).Right;
end;

procedure TWideStrKeyList.SetKeyPtr(const aNodePtr: PNodePtr; const aKeyPtr: PKeyPtr);
begin
  PWideStrNode(aNodePtr).Key := PWideString(aKeyPtr)^;
end;

procedure TWideStrKeyList.SetDataPtr(const aNodePtr: PNodePtr; aDataPtr: PDataPtr);
begin
  PWideStrNode(aNodePtr).Data := aDataPtr;
end;

procedure TWideStrKeyList.SetLNodePtr(const aNodePtr, aLeftPtr: PNodePtr);
begin
  PWideStrNode(aNodePtr).Left := aLeftPtr;
end;

procedure TWideStrKeyList.SetRNodePtr(const aNodePtr, aRightPtr: PNodePtr);
begin
  PWideStrNode(aNodePtr).Right := aRightPtr;
end;

function TWideStrKeyList.AllocNode: PNodePtr;
begin
  Result := AllocMem(SizeOf(TWideStrNode));
  PWideStrNode(Result)^.Key := '';
end;

procedure TWideStrKeyList.FreeNode(aNode: PNodePtr);
begin
  PWideStrNode(aNode)^.Key := '';
  FreeMem(aNode);
end;

function TWideStrKeyList.GetData(const aKey: WideString): Pointer;
begin
  Result := GetKeyPtrData(@aKey);
end;

procedure TWideStrKeyList.SetData(const aKey: WideString; const Value: Pointer);
begin
  SetKeyPtrData(@aKey, Value);
end;

function TWideStrKeyList.Add(const aKey: WideString; const aData): Boolean;
begin
  Result := AddKey(@aKey, aData);
end;

function TWideStrKeyList.Has(const aKey: WideString): Boolean;
begin
  Result := HasKey(@aKey);
end;

function TWideStrKeyList.Remove(const aKey: WideString): PDataPtr;
begin
  Result := RemoveKey(@aKey);
end;



initialization

finalization

end.


