{-----------------------------------------------------------------------------
 Unit Name: CSockDll
 Author:    oranke_f
 Date:      2010-09-01
 Purpose:
    CSock.dll, CSockE.dll 에 대한 인터페이스 유니트.

 History:
  2012-09-19
    OpenConnection, CloseConnection 에 aPending 인자 추가.
    연결 및 연결해제를 틱에서 처리하게 되어있는 구조를
    호출 즉시 처리할 수 있게 수정함.
 
-----------------------------------------------------------------------------}

{$DEFINE UseGetFuncAddr}
{$DEFINE ScramblePacket}

unit CSockDll;

interface

uses
  zhTypes, WinSock;

const
  // 접근불가. Len+PacketID+Reason(2). Len = 1+2 = 3
  // Reason은 접근불가 사유. 뭐 65536가지는 넘지 않겠지..
  // 여기서는 F000 이후만 사용하고 나머지는 어플에서 정의해서 쓴다. 
  ACCESS_DENINE = $FF;
    ADID_WRONGPACKET     = $F0A0; // 잘못된패킷. 날아오지 않아야 할 패킷의 경우..
      ADID_ZEROPKT        = $F0A1; // 유저에게는 WRONGPACKET을 전송. 서버이벤트로는 세부값을 사용.
      ADID_TOOLARGEPKT    = $F0A2;
      ADID_SOMUCHPKT      = $F0A3; // 너무 많은 패킷.

    ADID_EXCEPTION       = $F00C; // 처리에서 예외가 발생한 패킷.

    ADID_SVRBUG          = $F0FA; // 서버의 수행오류..
    ADID_SERVER_FULL     = $F0FB; // 서버 접속 풀.
    ADID_EMERGENCY       = $F0FC; // 긴급종료.

    ADID_OTHER           = $FFFF; // 나열되지 않은 기타이유.


  // 요청 거부. Len+PID+Reason
  REQ_DENINE  = $FE;
    RDID_OTHER           = $FFFF; // 나열되지 않은 기타이유.

type
  TConnEventStatus = (ctConnected, ctDisconnected, ctConnectFailed);

  TAccDenineEvent  = procedure (aReason: U16; aCode: U32; aParam: Pointer); stdcall;
  TReqDenineEvent  = procedure (aReason: U16; aCode: U32; aParam: Pointer); stdcall;
  TPacketEvent     = procedure (aPID: U8; aDataLen: U16; aData: PU8Array; aParam: Pointer); stdcall;
  TConnectEvent    = procedure (aStatus: TConnEventStatus; aParam: Pointer); stdcall;

{$IFDEF UseGetFuncAddr}
type
  TCreateClient = function (
    const aClientName: PAnsiChar;
    aKey: U32;
    aAccDenineEvent  : TAccDenineEvent;
    aReqDenineEvent  : TReqDenineEvent;
    aPacketEvent     : TPacketEvent;
    aConnectEvent    : TConnectEvent;
    aParam: Pointer
  ) : Boolean; stdcall;

  TDeleteClient = function (const aClientName: PAnsiChar): Boolean; stdcall;
  TSetCurClient = TDeleteClient;
  TGetClientCount = function : I32; stdcall;

  TIsConnected  = function : Boolean; stdcall;
  TIsEnableToSend = function (const aDataSize: WORD): Boolean; stdcall;

  TOpenConnection   = procedure (const aIP: PAnsiChar; aPort: WORD; aPending: Boolean = true); stdcall;
  TOpenConnection2  = procedure (const aIP: TIPData; aPort: WORD; aPending: Boolean = true); stdcall;
  TCloseConnection  = procedure (aPending: Boolean = true); stdcall;
  TTickClient       = function (): Boolean; stdcall;
  TTickAllClient    = procedure (); stdcall;

  TToSvr_SendPacketB = function (
    const aPID: U8;
    const aBufCount: Integer;
    const aBufLens: PU8Array;
    const aBufs: PPointerArray
  ): Boolean; stdcall;

  TToSvr_SendPacket2B = function (
    const aPID, aSPID: U8;
    const aBufCount: Integer;
    const aBufLens: PU8Array;
    const aBufs: PPointerArray
  ): Boolean; stdcall;

  TToSvr_SendPacketW = function (
    const aPID: U8;
    const aBufCount: Integer;
    const aBufLens: PU16Array;
    const aBufs: PPointerArray
  ): Boolean; stdcall;

  TToSvr_SendPacket2W = function (
    const aPID, aSPID: U8;
    const aBufCount: U8;
    const aBufLens: PU16Array;
    const aBufs: PPointerArray
  ): Boolean; stdcall;


  TToSvr_BeginPacket = function (const aPID: U8): Boolean; stdcall;
  TToSvr_BeginPacket2 = function (const aPID, aSPID: U8): Boolean; stdcall;
  TToSvr_SendBuffer = function (const pBuf: Pointer; buflen: U16): Boolean; stdcall;
  TToSvr_SendBufferW = function (
    const aBufCount: U8;
    const aBufLens: PU16Array;
    const aBufs: PPointerArray
  ): Boolean; stdcall;
  TToSvr_EndPacket = function (): Boolean; stdcall;


var
  CreateClient    : TCreateClient = nil;
  DeleteClient    : TDeleteClient = nil;
  SetCurClient    : TSetCurClient = nil;
  GetClientCount  : TGetClientCount = nil; 

  IsConnected     : TIsConnected = nil;
  IsEnableToSend  : TIsEnableToSend = nil;

  OpenConnection  : TOpenConnection  = nil;
  OpenConnection2 : TOpenConnection2 = nil;
  CloseConnection : TCloseConnection = nil;
  TickClient      : TTickClient      = nil;
  TickAllClient   : TTickAllClient   = nil;

  ToSvr_SendPacketB  : TToSvr_SendPacketB  = nil;
  ToSvr_SendPacket2B : TToSvr_SendPacket2B = nil;
  ToSvr_SendPacketW  : TToSvr_SendPacketW  = nil;
  ToSvr_SendPacket2W : TToSvr_SendPacket2W = nil;

  ToSvr_BeginPacket   : TToSvr_BeginPacket = nil;
  ToSvr_BeginPacket2  : TToSvr_BeginPacket2 = nil;
  ToSvr_SendBuffer    : TToSvr_SendBuffer = nil;
  ToSvr_SendBufferW   : TToSvr_SendBufferW = nil;
  ToSvr_EndPacket     : TToSvr_EndPacket = nil;

{$ELSE}
// 캐시 제어.
function CreateClient(
  const aClientName: PAnsiChar;
  aKey: U32;
  aAccDenineEvent  : TAccDenineEvent;
  aReqDenineEvent  : TReqDenineEvent;
  aPacketEvent     : TPacketEvent;
  aConnectEvent    : TConnectEvent;
  aParam: Pointer
) : Boolean; stdcall;
function DeleteClient(const aClientName: PAnsiChar): Boolean; stdcall;
function SetCurClient(const aClientName: PAnsiChar): Boolean; stdcall;

// 클라이언트 제어
function IsConnected(): Boolean; stdcall;
function IsEnableToSend(const aDataSize: WORD): Boolean; stdcall;

procedure OpenConnection(const aIP: PAnsiChar; aPort: WORD; aPending: Boolean = true); stdcall;
procedure OpenConnection2(const aIP: TIPData; aPort: WORD; aPending: Boolean = true); stdcall;
procedure CloseConnection(aPending: Boolean = true); stdcall;
function TickClient(): Boolean; stdcall;
procedure TickAllClient(); stdcall;

function ToSvr_SendPacketB(
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: PU8Array;
  const aBufs: PPointerArray
): Boolean; stdcall;

function ToSvr_SendPacket2B(
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: PU8Array;
  const aBufs: PPointerArray
): Boolean; stdcall;

function ToSvr_SendPacketW(
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: PU16Array;
  const aBufs: PPointerArray
): Boolean; stdcall;

function ToSvr_SendPacket2W(
  const aPID, aSPID: U8;
  const aBufCount: U8;
  const aBufLens: PU16Array;
  const aBufs: PPointerArray
): Boolean; stdcall;

function ToSvr_BeginPacket(const aPID: U8): Boolean; stdcall;
function ToSvr_BeginPacket2(const aPID, aSPID: U8): Boolean; stdcall;
function ToSvr_SendBuffer(const pBuf: Pointer; buflen: U16): Boolean; stdcall;
function ToSvr_SendBufferW(
  const aBufCount: Integer;
  const aBufLens: PU16Array;
  const aBufs: PPointerArray
): Boolean; stdcall;
function ToSvr_EndPacket(): Boolean; stdcall;

{$ENDIF of UseGetFuncAddr}


// 델파이에서 쓰기 편하게..
function ToSvr_SendPacketBArr(
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Byte;
  const aBufs: array of Pointer
): Boolean;

function ToSvr_SendPacket2BArr(
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Byte;
  const aBufs: array of Pointer
): Boolean;

function ToSvr_SendPacketWArr(
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;

function ToSvr_SendPacket2WArr(
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;

function ToSvr_SendBufferWArr(
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;

function HostToIP(const aHost: AnsiString): TIPData;

implementation

const
{$IFDEF ScramblePacket}
  CSock_Dll = 'CSockE.dll';
{$ELSE}
  CSock_Dll = 'CSock.dll';
{$ENDIF}

{$IFDEF UseGetFuncAddr}
function GetFuncs(const aFuncKey: U32): Pointer; stdcall; external CSock_Dll name 'GetFuncs';
{$ELSE}
function CreateClient; external CSock_Dll name 'CreateClient';
function DeleteClient; external CSock_Dll name 'DeleteClient';
function SetCurClient; external CSock_Dll name 'SetCurClient';

function IsConnected; external CSock_Dll name 'IsConnected';
function IsEnableToSend; external CSock_Dll name 'IsEnableToSend';
procedure OpenConnection; external CSock_Dll name 'OpenConnection';
procedure OpenConnection2; external CSock_Dll name 'OpenConnection2';
procedure CloseConnection; external CSock_Dll name 'CloseConnection';
function TickClient; external CSock_Dll name 'TickClient';
procedure TickAllClient; external CSock_Dll name 'TickAllClient';

function ToSvr_SendPacketB; external CSock_Dll name 'ToSvr_SendPacketB';
function ToSvr_SendPacket2B; external CSock_Dll name 'ToSvr_SendPacket2B';
function ToSvr_SendPacketW; external CSock_Dll name 'ToSvr_SendPacketW';
function ToSvr_SendPacket2W; external CSock_Dll name 'ToSvr_SendPacket2W';

function ToSvr_BeginPacket; external CSock_Dll name 'ToSvr_BeginPacket';
function ToSvr_BeginPacket2; external CSock_Dll name 'ToSvr_BeginPacket2';
function ToSvr_SendBuffer; external CSock_Dll name 'ToSvr_SendBuffer';
function ToSvr_SendBufferW; external CSock_Dll name 'ToSvr_SendBufferW';
function ToSvr_EndPacket(): Boolean; external CSock_Dll name 'ToSvr_EndPacket';

{$ENDIF}

function ToSvr_SendPacketBArr(
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Byte;
  const aBufs: array of Pointer
): Boolean;
begin
  Result := ToSvr_SendPacketB(aPID, aBufCount, @aBufLens[0], @aBufs[0]);
end;

function ToSvr_SendPacket2BArr(
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Byte;
  const aBufs: array of Pointer
): Boolean;
var
  pBufLens, pBufs: Pointer;
begin
  //Result := ToSvr_SendPacket2B(aPID, aSPID, aBufCount, @aBufLens[0], @aBufs[0]);
  if Length(aBufLens) > 0 then pBufLens := @aBufLens[0] else pBufLens := nil;
  if Length(aBufs) > 0 then pBufs := @aBufs[0] else pBufs := nil;
  Result := ToSvr_SendPacket2B(aPID, aSPID, aBufCount, pBufLens, pBufs);
end;

function ToSvr_SendPacketWArr(
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;
var
  pBufLens, pBufs: Pointer;
begin
  //Result := ToSvr_SendPacketW(aPID, aBufCount, @aBufLens[0], @aBufs[0]);
  if Length(aBufLens) > 0 then pBufLens := @aBufLens[0] else pBufLens := nil;
  if Length(aBufs) > 0 then pBufs := @aBufs[0] else pBufs := nil;
  Result := ToSvr_SendPacketW(aPID, aBufCount, pBufLens, pBufs);
end;

function ToSvr_SendPacket2WArr(
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;
var
  pBufLens, pBufs: Pointer;
begin
  //Result := ToSvr_SendPacket2W(aPID, aSPID, aBufCount, @aBufLens[0], @aBufs[0]);
  if Length(aBufLens) > 0 then pBufLens := @aBufLens[0] else pBufLens := nil;
  if Length(aBufs) > 0 then pBufs := @aBufs[0] else pBufs := nil;
  Result := ToSvr_SendPacket2W(aPID, aSPID, aBufCount, pBufLens, pBufs);
end;


function ToSvr_SendBufferWArr(
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;
var
  pBufLens, pBufs: Pointer;
begin
  //Result := ToSvr_SendBufferW(aBufCount, @aBufLens[0], @aBufs[0]);
  if Length(aBufLens) > 0 then pBufLens := @aBufLens[0] else pBufLens := nil;
  if Length(aBufs) > 0 then pBufs := @aBufs[0] else pBufs := nil;
  Result := ToSvr_SendBufferW(aBufCount, pBufLens, pBufs);
end;

function HostToIP(const aHost: AnsiString): TIPData;
var
  wsData   : TWSAData;
  HostEnt  : PHostEnt;
begin
  WSAStartup($0101, wsData);
  try
    hostEnt := GetHostByName(PAnsiChar(aHost));

    if Assigned(HostEnt) and
       Assigned(HostEnt^.H_Addr_List) and
       Assigned(HostEnt^.H_Addr_List^) then
      Result := PIPData(HostEnt^.H_Addr_List^)^
    else
      Result.LID := U32(-1);
  finally
    WSACleanup;
  end;
end;

initialization
{$IFDEF UseGetFuncAddr}
  CreateClient    := GetFuncs($31FE8FC7);
  DeleteClient    := GetFuncs($2F1DFAB6);
  SetCurClient    := GetFuncs($F8463809);
  GetClientCount  := GetFuncs($39C89E0E);

  IsConnected     := GetFuncs($7161419F);
  IsEnableToSend  := GetFuncs($73D45C70);

  OpenConnection  := GetFuncs($69B10188);
  OpenConnection2 := GetFuncs($041760AA);
  CloseConnection := GetFuncs($D611D556);
  TickClient      := GetFuncs($169B7788);
  TickAllClient   := GetFuncs($C53B96CF);

  ToSvr_SendPacketB  := GetFuncs($32145C27);
  ToSvr_SendPacket2B := GetFuncs($AF19A9EB);
  ToSvr_SendPacketW  := GetFuncs($32145C3C);
  ToSvr_SendPacket2W := GetFuncs($AF19AA00);

  ToSvr_BeginPacket   := GetFuncs($727E90C6);
  ToSvr_BeginPacket2  := GetFuncs($BDEBA0EC);
  ToSvr_SendBuffer    := GetFuncs($D1459A33);
  ToSvr_SendBufferW   := GetFuncs($1A53F2E4);
  ToSvr_EndPacket     := GetFuncs($7F1EF8B8);
{$ENDIF}


finalization

end.

