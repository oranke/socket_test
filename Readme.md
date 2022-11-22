# 소켓 테스트

소켓 DLL 을 테스트하기 위한 프로젝트.  
델파이 7에서 빌드를 기준으로 한다.  


## 기본 개념

U8, I8, U32, I32 등의 자료형은 `zhTypes.pas` 에 정의되어 있다. 

하나의 패킷은 2바이트 길이값 + 1바이트 패킷아이디 + 데이터 로 구분한다. 
ScramblePacket 모드로 빌드된 DLL은 패킷 단위로 번호를 붙여 인코딩하고 검증용 해시를 첨부한다. 

DLL에 정의된 패킷 구조는 다음과 같다. 


```pas
  TPacketRec = packed record
    Len : U16; // PID+Data의 길이를 합친 값.
    PID : U8;  // 패킷 아이디.
    case Integer of
      0: (Data: TU8Array);
{$IFDEF ScramblePacket}
      1: (HashVal: U32; Number: U8; Data2: TU8Array); // 해싱+넘버링 할 때 정보.
      2: (HashArr: Array[0..3] of Byte);
{$ENDIF}
  end;
```

서버측에서 클라로 패킷을 보낼때는 ToCli_XXX 함수를 사용하며, 클라측에서 서버로 보낼때는 ToSvr_XXXX 함수를 사용한다. 
패킷을 수신하면 PacketEvent 콜백이 실행되며 여기에 패킷아이디(PID)와 디코딩된 패킷 데이터, 그리고 패킷 길이가 전달된다. 

```pas
TPacketEvent = procedure (aPID: U8; aDataLen: U16; aData: PU8Array; aParam: Pointer); stdcall;
```

## 서버 DLL 사용법. 

* DLL 이름 : SSockE.dll
* 제어 유니트 : SSockDll.pas

### 생성 및 제거

하나의 프로그램은 여러개의 서버를 생성할 수 있다. 
각 서버는 ServerName 으로 구분된다. 

```pas
// 서버 생성
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

// 서버 제거
function DeleteServer(const aServerName: PAnsiChar): Boolean; stdcall;

// 현재 서버 설정
function SetCurServer(const aServerName: PAnsiChar): Boolean; stdcall;

// 서버 시작 / 종료
function StartServer(): Boolean; stdcall;
function StopServer(): Boolean; stdcall;

```

* aPacketEvent - 클라에서 ToSvr_XXXX() 로 패킷 전송시 발생
* aAcceptEvent - 소켓 연결시 발생
* aChkOpenEvent - 소켓 연결 종료시 발생
* aAccDenineEvent - 클라에 접근거부, ToCli_AccessDenine() 함수를 호출했을 때 발생

각 클라이언트는 이 이벤트로 전달되는 aConnIndex 파라미터로 구분할 수 있다. 


### 데이터 전달

클라이언트의 데이터 전달에서 다시 설명.  
여기서는 함수들만 나열한다.  

**패킷 전송 함수**

```pas
// 데이터 각 엔트리 길이값이 바이트로 충분할 때
function ToCli_SendPacketBArr(
  aConnIndex: U16;
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Byte;
  const aBufs: array of Pointer
): Boolean; stdcall;

// PID, SPID 구조일 때 편하게 쓰기위해
function ToCli_SendPacket2BArr(
  aConnIndex: U16;
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Byte;
  const aBufs: array of Pointer
): Boolean; stdcall;

// 데이터 각 엔트리 길이값이 워드값이어야 할 때.
function ToCli_SendPacketWArr(
  aConnIndex: U16;
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;  stdcall;

// PID, SPID 구조일 때 편하게 쓰기위해
function ToCli_SendPacket2WArr(
  aConnIndex: U16;
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean; stdcall;

```

**가변패킷 전송함수**

```pas

// PID 패킷 시작.
function ToCli_BeginPacket(aConnIndex: U16; const aPID: U8): Boolean; stdcall;

// PID, SPID 패킷 시작
function ToCli_BeginPacket2(aConnIndex: U16; const aPID, aSPID: U8): Boolean; stdcall;

// 데이터 전송
function ToCli_SendBufferWArr(
  aConnIndex: U16;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean; stdcall; 

// 패킷 종료
function ToCli_EndPacket(aConnIndex: U16): Boolean; stdcall;

```


### 기타 

```pas
// 접속 끊을 때.
// 클라에서 AccessDenine 콜백 발생 후 접속 종료.
procedure ToCli_AccessDenine(aConnIndex, aReason: U16; aCode: U32 = 0); stdcall;  

// 요청 거부할 때.
// 클라에서 RequestDenine 콜백 발생. 
procedure ToCli_RequestDenine(aConnIndex, aReason: U16; aCode: U32 = 0); stdcall;  

// 총 접속 허용수 리턴
function GetConnectionCount: Word; stdcall;

// 남은 접속 허용수 리턴
function GetAvailableConnCount: Word; stdcall;

```

### 참고사항 

* 접속 후 3초이내에 최초 패킷이 없으면 접속 종료.  
* 접속 중 30초 이내에 아무 패킷이 없으면 접속 종료.  


## 클라 DLL 사용법. 

* DLL 이름 : CSockE.dll
* 제어 유니트 : CSockDll.pas

클라용 DLL의 함수들은 직접 노출하지 않고 GetFuncs() 를 사용해 동적으로 바인딩한다.  
이는 해킹을 귀찮게 하려는 목적이다.  
CSockDll_Static.pas 를 사용하면 CSockE.dll 을 실행파일 내부에 포함시킨다.  
DLL 자체도 감춰지므로 해킹이 좀 더 귀찮아진다.  

CSockDll_Static.pas 유니트를 사용하려면 DelphiZLib 가 필요하다.  
https://github.com/grandchef/delphizlib


### 생성 및 제거. 라이프타임 관리.

하나의 프로그램은 여러개의 클라이언트를 생성할 수 있다. 
각 클라이언트들은 ClientName 으로 구분된다. 

```pas
// 클라이언트 생성
function CreateClient(
  const aClientName: PAnsiChar;
  aKey: U32;
  aAccDenineEvent  : TAccDenineEvent;
  aReqDenineEvent  : TReqDenineEvent;
  aPacketEvent     : TPacketEvent;
  aConnectEvent    : TConnectEvent;
  aParam: Pointer
) : Boolean; stdcall;

// 클라이언트 제거
function DeleteClient(const aClientName: PAnsiChar): Boolean; stdcall;

// 현재 클라이언트 설정
function SetCurClient(const aClientName: PAnsiChar): Boolean; stdcall;

// 현재 클라이언트에 틱 공급
function TickClient(): Boolean; stdcall;

// 전체 클라이언트에 틱 공급
procedure TickAllClient(); stdcall;

```

**생성함수 - CreateClient()** 

접근거부, 요청거부, 패킷수신, 연결알림 함수를 콜백으로 전달받는다.  
이 이벤트는 틱을 공급하는 쓰레드, 즉 TickClient()를 호출하는 쓰레드에서 호출된다. 이는 서버도 마찬가지이다. 
이 때 Thread-Safe 하도록 연결정보에는 락이 걸린다. 


```pas

// 서버가 ToCli_AccessDenine() 을 호출할 때 전송.
// 이 패킷을 받으면 곧 연결이 끊어진다. 
procedure AccDenineEvent (aReason: U16; aCode: U32; aParam: Pointer); stdcall;
begin
  WriteLn('억세스 거부 : ', aReason, ', ', aCode);
end;

// 서버가 ToCli_RequestDenine() 을 호출할 때 전송.
procedure ReqDenineEvent (aReason: U16; aCode: U32; aParam: Pointer); stdcall;
begin
  WriteLn('요청 거부 : ', aReason, ', ', aCode);
end;

// 서버가 ToCli_XXXX 로 전송한 패킷이 도달할 때 발생. 
procedure PacketEvent(aPID: U8; aDataLen: U16; aData: PU8Array; aParam: Pointer); stdcall;
begin
  WriteLn('패킷전송 : ', aPID, '. Len: ', aDataLen);
end;

procedure ConnectEvent   (aStatus: TConnEventStatus; aParam: Pointer); stdcall;
begin
  case aStatus of
    ctConnected    : WriteLn('연결 성공');
    ctDisconnected : WriteLn('연결 끊김');
    ctConnectFailed: WriteLn('연결 실패');
  end;
end;

........

  // 클라이언트 생성
  CreateClient('ChatCli',
    1234567890, // 서버와 클라가 동일해야 함.
    // 이벤트 콜백. 
    AccDenineEvent, ReqDenineEvent, PacketEvent, ConnectEvent,
    CallbackParam // 콜백에 전달할 파라미터. 
  );

  SetCurClient('ChatCli');

```

서버는 접속 후 3초동안 아무런 행위가 없으면, 소켓 홀딩 공격으로 판단하고 접속을 끊는다.  
따라서 ConnectEvent 에서 소켓 연결 확인시 바로 로그온 처리를 하자. (Client2.exe 참고)

서버에서 데이터가 TPacketRec 단위로 전달되면, DLL은 전달된 데이터의 인코딩을 풀고 PacketEvent 를 발생시킨다.  
이 때 PID 는 ToCli_SendXXX 등의 인자값으로 전달되는 PID 값을 의미한다.  


### 연결 및 종료

```pas
// 연결
procedure OpenConnection(const aIP: PAnsiChar; aPort: WORD; aPending: Boolean = true); stdcall;

// 접속 종료
procedure CloseConnection(aPending: Boolean = true); stdcall;

```

클라이언트가 연결을 시도하면 해당 쓰레드는 연결이 성공, 또는 실패할 때 까지 블로킹된다.  
aPending 인자가 켜지면 이 블로킹이 발생하지 않는다. (기본값 true)   
연결 확인은 ConnectEvent 에서 확인할 수 있다.  


### 데이터 전달

패킷은 TPacketRec 의 PID (Packet ID) 로 구분되므로 큰 분류로 255가지가 된다. 
하지만 실제 구현에서는 이것으로 모자라므로, 일반적으로 Data 영역의 첫번째 바이트를 SPID (Sub Packet ID) 로 정의해 소분류로 사용한다. 

로그인 대분류 아래 일반 로그인과 어드민 로그인을 정의한 경우
(ChatPacket.pas 참고)

```pas
const
  REQ_LOGIN = $01;
    // SPID
    LGN_NORMAL = $01;
    {
      버전(4) + NameLen + Name + KeyLen + Key
    }

    LGN_ADMIN = $02;
    {
      관리자로 접속.
      버전(4) + NameLen + Name + KeyLen + Key

      이 프로젝트에서는 사용하지 않음.
    }
```

이를 통해 이론적으로 65536 - 256 * 2 가지의 패킷종류를 구분지을 수 있으며, 이는 어지간한 중대형 온라인 게임에 충분한 종류이다. 
(PID $FF는 Access Denine, $FE는 Request Denine 으로 사용한다.)

PID 만으로 이루어진 패킷은 ToSvr_SendPacketBArr()/ToSvr_SendPacketWArr() 함수를, SPID 로 세분화한 정보는 ToSvr_SendPacket2BArr()/ToSvr_SendPacket2WArr() 함수를 사용해 데이터를 전송한다.


**패킷 전송 함수**

```pas
// 데이터 각 엔트리 길이값이 바이트로 충분할 때
function ToSvr_SendPacketBArr(
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Byte;
  const aBufs: array of Pointer
): Boolean;

// PID, SPID 구조일 때 편하게 쓰기위해
function ToSvr_SendPacket2BArr(
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Byte;
  const aBufs: array of Pointer
): Boolean;

// 데이터 각 엔트리 길이값이 워드값이어야 할 때.
function ToSvr_SendPacketWArr(
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;

// PID, SPID 구조일 때 편하게 쓰기위해
function ToSvr_SendPacket2WArr(
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;
```

**가변패킷 전송함수**

가변 패킷을 위해 Begin ~ SendBuffer ~ End 형식도 준비되어있다.

```pas
function ToSvr_BeginPacket(const aPID: U8): Boolean; stdcall;
function ToSvr_BeginPacket2(const aPID, aSPID: U8): Boolean; stdcall;

function ToSvr_SendBufferWArr(
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;

function ToSvr_EndPacket(): Boolean; stdcall;
```


예를 들어 다음과 같은 구조체를 서버에 전송할 때, (PID는 33, SPID는 01 로 가정)

```pas
  TMyRec = packed record
    Data1, Data2, Data3 : Integer;
  end; 
```

아래 세 가지 방법은 서버 입장에서 모두 동일한 패킷을 전달받게 된다. 

```pas
// 방법 1
var
  MyRec: TMyRec; 
begin
  MyRec.Data1 := 1; 
  MyRec.Data2 := 2; 
  MyRec.Data3 := 3; 

  ToSvr_SendPacket2WArr(33, 01, 
    1, 
    [SizeOf(TMyRec)],
    [@MyRec]
  );
end;
```

```pas
// 방법 2
var
  Data1, Data2, Data3: Integer; 
begin
  Data1 := 1; 
  Data2 := 2; 
  Data3 := 3; 

  ToSvr_SendPacket2WArr(33, 01, 
    3, 
    [SizeOf(Integer), SizeOf(Integer), SizeOf(Integer)],
    [@Data1, @Data2, @Data3]
  );
end;
```


```pas
// 방법 3
var
  Data: Integer; 
begin
  ToSvr_BeginPacket2(33, 01); 

  Data := 1; 
  ToSvr_SendBufferWArr(1, [SizeOf(Integer)], [@Data]);

  Data := 2; 
  ToSvr_SendBufferWArr(1, [SizeOf(Integer)], [@Data]);

  Data := 3; 
  ToSvr_SendBufferWArr(1, [SizeOf(Integer)], [@Data]);

  ToSvr_EndPacket();
end;
```


샘플은 Client.exe 를 참고. 



