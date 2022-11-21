{-----------------------------------------------------------------------------
 Unit Name: SSockDll
 Author:    oranke_f
 Date:      2010-08-30
 Purpose:
    SSock.dll, SSockE.dll 에 대한 인터페이스 유니트.
    
 History:
-----------------------------------------------------------------------------}


{$DEFINE ScramblePacket}

//!! SSock.dll, SSockE.dll 에 대한 인터페이스 유니트
unit SSockDll;

interface

uses
  zhTypes, SysUtils;

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
  // 이 콜백은 서버의 Tick을 호출한 쓰레드에서 호출된다. 
  TPacketEvent    = procedure (aConnIndex: U16; aPID: U8; aDataLen: U16; aData: PU8Array; aParam: Pointer); stdcall;
  TAcceptEvent    = procedure (aConnIndex: U16; aPeer: PAnsiChar; aParam: Pointer); stdcall;
  TChkOpenEvent   = procedure (aConnIndex: U16; aParam: Pointer); stdcall;
  TAccDenineEvent = procedure (aConnIndex: U16; aReason: U16; aCode: U32; aPeer: PAnsiChar; aParam: Pointer); stdcall;

// 캐시 제어.
function CreateServer(
  const aServerName: PAnsiChar;
  aCount, aPort: U16;
  aKey: U32;
  aPacketEvent: TPacketEvent;
  aAcceptEvent: TAcceptEvent;
  aChkOpenEvent: TChkOpenEvent;
  aAccDenineEvent: TAccDenineEvent;
  aParam: Pointer
) : Boolean; stdcall;
function DeleteServer(const aServerName: PAnsiChar): Boolean; stdcall;
function SetCurServer(const aServerName: PAnsiChar): Boolean; stdcall;

// 서버 제어
function IsStarted: Boolean; stdcall;
function GetConnectionCount: Word; stdcall;
function GetAvailableConnCount: Word; stdcall;
function StartServer(): Boolean; stdcall;
function PreStopServer(): Boolean; stdcall;
function StopServer(): Boolean; stdcall;
function TickServer(const aCurrentTick: U32): Boolean; stdcall;
procedure TickAllServer(const aCurrentTick: U32); stdcall;

// 커넥션 제어.
procedure SetConnValidTime(aConnIndex: U16; const aValue: U32); stdcall;
procedure SetConnInfinite(aConnIndex: U16); stdcall;
procedure SetAccepted(aConnIndex: U16); stdcall;
function GetAccepted(aConnIndex: U16): Boolean; stdcall;

function GetConnPeer(aConnIndex: U16): U32; stdcall;

function IsEnableToSend(aConnIndex: U16; const aDataSize: WORD): Boolean; stdcall;

procedure ToCli_AccessDenine(aConnIndex, aReason: U16; aCode: U32 = 0); stdcall;   // 접속 끊을 때.
procedure ToCli_RequestDenine(aConnIndex, aReason: U16; aCode: U32 = 0); stdcall;  // 요청 거부할 때.

function ToCli_SendPacketB(
  aConnIndex: U16;
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: PU8Array;
  const aBufs: PPointerArray
): Boolean; stdcall;

function ToCli_SendPacket2B(
  aConnIndex: U16;
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: PU8Array;
  const aBufs: PPointerArray
): Boolean; stdcall;

function ToCli_SendPacketW(
  aConnIndex: U16;
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: PU16Array;
  const aBufs: PPointerArray
): Boolean; stdcall;

function ToCli_SendPacket2W(
  aConnIndex: U16;
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: PU16Array;
  const aBufs: PPointerArray
): Boolean; stdcall;

function ToCli_BeginPacket(aConnIndex: U16; const aPID: U8): Boolean; stdcall;
function ToCli_BeginPacket2(aConnIndex: U16; const aPID, aSPID: U8): Boolean; stdcall;
function ToCli_SendBuffer(aConnIndex: U16; const pBuf: Pointer; buflen: U16): Boolean; stdcall;
function ToCli_SendBufferW(
  aConnIndex: U16;
  const aBufCount: Integer;
  const aBufLens: PU16Array;
  const aBufs: PPointerArray
): Boolean; stdcall;
function ToCli_EndPacket(aConnIndex: U16): Boolean; stdcall;



// 델파이에서 쓰기 편하게...
function ToCli_SendPacketBArr(
  aConnIndex: U16;
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Byte;
  const aBufs: array of Pointer
): Boolean; stdcall;

function ToCli_SendPacket2BArr(
  aConnIndex: U16;
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Byte;
  const aBufs: array of Pointer
): Boolean; stdcall;

function ToCli_SendPacketWArr(
  aConnIndex: U16;
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;  stdcall;

function ToCli_SendPacket2WArr(
  aConnIndex: U16;
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean; stdcall;


function ToCli_SendBufferWArr(
  aConnIndex: U16;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean; stdcall; 


implementation

const
{$IFDEF ScramblePacket}
  SSock_Dll = 'SSockE.dll';
{$ELSE}
  SSock_Dll = 'SSock.dll';
{$ENDIF}  

function CreateServer; external SSock_Dll name 'CreateServer';
function DeleteServer; external SSock_Dll name 'DeleteServer';
function SetCurServer; external SSock_Dll name 'SetCurServer';

function IsStarted; external SSock_Dll name 'IsStarted';
function GetConnectionCount; external SSock_Dll name 'GetConnectionCount';
function GetAvailableConnCount; external SSock_Dll name 'GetAvailableConnCount';
function StartServer; external SSock_Dll name 'StartServer';
function PreStopServer; external SSock_Dll name 'PreStopServer';
function StopServer; external SSock_Dll name 'StopServer';
function TickServer; external SSock_Dll name 'TickServer';
procedure TickAllServer; external SSock_Dll name 'TickAllServer';

procedure SetConnValidTime; external SSock_Dll name 'SetConnValidTime';
procedure SetConnInfinite; external SSock_Dll name 'SetConnInfinite';
procedure SetAccepted; external SSock_Dll name 'SetAccepted';
function GetAccepted; external SSock_Dll name 'GetAccepted';

function GetConnPeer; external SSock_Dll name 'GetConnPeer';
function IsEnableToSend; external SSock_Dll name 'IsEnableToSend';

procedure ToCli_AccessDenine; external SSock_Dll name 'ToCli_AccessDenine';
procedure ToCli_RequestDenine; external SSock_Dll name 'ToCli_RequestDenine';

function ToCli_SendPacketB; external SSock_Dll name 'ToCli_SendPacketB';
function ToCli_SendPacket2B; external SSock_Dll name 'ToCli_SendPacket2B';
function ToCli_SendPacketW; external SSock_Dll name 'ToCli_SendPacketW';
function ToCli_SendPacket2W; external SSock_Dll name 'ToCli_SendPacket2W';

function ToCli_BeginPacket; external SSock_Dll name 'ToCli_BeginPacket';
function ToCli_BeginPacket2; external SSock_Dll name 'ToCli_BeginPacket2';
function ToCli_SendBuffer; external SSock_Dll name 'ToCli_SendBuffer';
function ToCli_SendBufferW; external SSock_Dll name 'ToCli_SendBufferW';
function ToCli_EndPacket; external SSock_Dll name 'ToCli_EndPacket';


function ToCli_SendPacketBArr(
  aConnIndex: U16;
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Byte;
  const aBufs: array of Pointer
): Boolean;
var
  pBufLens, pBufs: Pointer;
begin
  //Result := ToCli_SendPacketB(aConnIndex, aPID, aBufCount, @aBufLens[0], @aBufs[0]);
  if Length(aBufLens) > 0 then pBufLens := @aBufLens[0] else pBufLens := nil;
  if Length(aBufs) > 0 then pBufs := @aBufs[0] else pBufs := nil;
  Result := ToCli_SendPacketB(aConnIndex, aPID, aBufCount, pBufLens, pBufs);
end;

function ToCli_SendPacket2BArr(
  aConnIndex: U16;
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Byte;
  const aBufs: array of Pointer
): Boolean;
var
  pBufLens, pBufs: Pointer;
begin
  //Result := ToCli_SendPacket2B(aConnIndex, aPID, aSPID, aBufCount, @aBufLens[0], @aBufs[0]);
  if Length(aBufLens) > 0 then pBufLens := @aBufLens[0] else pBufLens := nil;
  if Length(aBufs) > 0 then pBufs := @aBufs[0] else pBufs := nil;
  Result := ToCli_SendPacket2B(aConnIndex, aPID, aSPID, aBufCount, pBufLens, pBufs);
end;

function ToCli_SendPacketWArr(
  aConnIndex: U16;
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;
var
  pBufLens, pBufs: Pointer;
begin
  //Result := ToCli_SendPacketW(aConnIndex, aPID, aBufCount, @aBufLens[0], @aBufs[0]);
  if Length(aBufLens) > 0 then pBufLens := @aBufLens[0] else pBufLens := nil;
  if Length(aBufs) > 0 then pBufs := @aBufs[0] else pBufs := nil;
  Result := ToCli_SendPacketW(aConnIndex, aPID, aBufCount, pBufLens, pBufs);
end;

function ToCli_SendPacket2WArr(
  aConnIndex: U16;
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;
var
  pBufLens, pBufs: Pointer;
begin
  //Result := ToCli_SendPacket2W(aConnIndex, aPID, aSPID, aBufCount, @aBufLens[0], @aBufs[0]);
  if Length(aBufLens) > 0 then pBufLens := @aBufLens[0] else pBufLens := nil;
  if Length(aBufs) > 0 then pBufs := @aBufs[0] else pBufs := nil;
  Result := ToCli_SendPacket2W(aConnIndex, aPID, aSPID, aBufCount, pBufLens, pBufs);
end;

function ToCli_SendBufferWArr(
  aConnIndex: U16;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;
var
  pBufLens, pBufs: Pointer;
begin
  //Result := ToCli_SendBufferW(aConnIndex, aBufCount, @aBufLens[0], @aBufs[0]);
  if Length(aBufLens) > 0 then pBufLens := @aBufLens[0] else pBufLens := nil;
  if Length(aBufs) > 0 then pBufs := @aBufs[0] else pBufs := nil;
  Result := ToCli_SendBufferW(aConnIndex, aBufCount, pBufLens, pBufs);
end;

(*
// const 배열을 전달.
function ToCli_SendPacket2CArr(
  aConnIndex: U16;
  const aPID, aSPID: U8;
  const aArray: array of const
): Boolean;
var
  i: Integer;
  l, h: Integer;
begin
  l := Low(aArray);
  h := High(aArray);

  IncBufSizeW(h - l + 1);

  for i:= l to h do
  case aArray[i].VType of
    vtInteger     :
    begin
      ugBufLensW[i-l] := SizeOf(Integer);
      ugBufs    [i-l] := @aArray[i].VInteger;
    end;

    vtBoolean     :
    begin
      ugBufLensW[i-l] := SizeOf(Boolean);
      ugBufs    [i-l] := @aArray[i].VBoolean;
    end;

    vtChar        :
    begin
      ugBufLensW[i-l] := SizeOf(Char);
      ugBufs    [i-l] := @aArray[i].VChar;
    end;

    vtExtended    :
    begin
      ugBufLensW[i-l] := SizeOf(Extended);
      ugBufs    [i-l] := aArray[i].VExtended;
    end;

    vtString      : // ShortString. 길이와 함께 던진다. 
    begin
      ugBufLensW[i-l] := Ord(aArray[i].VString^[0]) +1;
      ugBufs    [i-l] := @aArray[i].VString^[0];
    end;

    vtPChar       :
    begin
      ugBufLensW[i-l] := StrLen(aArray[i].VPChar);
      ugBufs    [i-l] := aArray[i].VPChar;
    end;

    vtWideChar    :
    begin
      ugBufLensW[i-l] := SizeOf(WideChar);
      ugBufs    [i-l] := @aArray[i].VWideChar;
    end;

    vtPWideChar   :
    begin
      ugBufLensW[i-l] := Length(WideString(aArray[i].VPWideChar)) * SizeOf(WideChar);
      ugBufs    [i-l] := aArray[i].VPWideChar;
    end;

    vtAnsiString  :
    begin
      ugBufLensW[i-l] := Length(AnsiString(aArray[i].VAnsiString));
      ugBufs    [i-l] := PAnsiChar(AnsiString(aArray[i].VAnsiString));
    end;

    vtWideString  :
    begin
      ugBufLensW[i-l] := Length(WideString(aArray[i].VWideString)) * SizeOf(WideChar);
      ugBufs    [i-l] := PWideChar(WideString(aArray[i].VWideString));
    end;

    vtInt64       :
    begin
      ugBufLensW[i-l] := SizeOf(Int64);
      ugBufs    [i-l] := aArray[i].VInt64;
    end;
  else
    begin
      ugBufLensW[i-l] := 0;
      ugBufs    [i-l] := nil;
    end;
  end;

  Result := ToCli_SendPacket2W(aConnIndex, aPID, aSPID, h - l + 1, @ugBufLensW[0], @ugBufs[0]);
end;
*)

initialization

finalization

end.


