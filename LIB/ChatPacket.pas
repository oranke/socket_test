{-----------------------------------------------------------------------------
 Unit Name: ChatPacket
 Author:    oranke
 Date:      2022-11-21
 Purpose:
   채팅서버 <-> 클라이언트 사이의 통신패킷 정의 유니트.

 History:

-----------------------------------------------------------------------------}


unit ChatPacket;

interface

uses
  Windows, zhTypes;

const
  // 패킷의 버전.
  PKT_VERSION : U32 = 2022112101;

const
  PKT_XOR_KEY : U32 = $A33B2EF0; // 패킷 인코딩 / 디코딩 키.


{///////////////////////////////////////////////////////////////////////////////

  접근거부, 요청거부 상수들 정의.
  $F000 이상의 값은 통신 DLL에 이미 정의되어있음.

///////////////////////////////////////////////////////////////////////////////}

  // Access Denine reasons.
  ADID_NONE         = $0000;  // 에러없음.
  ADID_INVALID_USER = $0001;  // 유저에러


  // Request Denine reasons.
  RDID_NONE         = $0000;  // 거부없음.
  // 요청거부시 요청패킷과 서브아이디를 MakeWord로 Word로 빚어 전송한다.


{///////////////////////////////////////////////////////////////////////////////

  대부분의 패킷은 PID와 그에 딸린 SPID로 구분된다.
  SPID를 전송하는 함수는 통신 DLL 내에 구현되어 있다.

///////////////////////////////////////////////////////////////////////////////}

type
  //!! 패킷 처리후 응답.
  PPacketProcRet = ^TPacketProcRet;
  TPacketProcRet = packed record
    ADID, RDID: Word;
    Code: U32;
  end;



//----------------------------------------------------------------
// 로그인 요청 패킷
//----------------------------------------------------------------

const
  REQ_LOGIN = $01;
    // SPID
    LGN_NORMAL = $01;
    {
      버전(4) + NameLen + Name + KeyLen + Key
      버전(4) + KeyLen + Key
    }

    LGN_ADMIN = $02;
    {
      관리자로 접속.
      버전(4) + NameLen + Name + KeyLen + Key

      이 프로젝트에서는 사용하지 않음.
    }


type
  // 클라이언트 타입. 일반, 어드민, 마스터.
  // 마스터는 어드민들의 추가 및 제거 가능. -> 이 프로젝트에서는 사용하지 않음.
  TClientType = (cliTypeNormal, cliTypeManager, cliTypeMaster);

  TAcceptLogonRec = packed record
    Flag: U8;
    CliType: TClientType;
  end;


  PReqLogonPkt = ^TReqLogonPkt;
  TReqLogonPkt = packed record
    case Integer of
      // C->S
      0:
        (
          SPID : U8;
          Version: U32;
          LogonData: TU8Array;
        );

      // S->C
      1:
      (
        AcceptInfo: TAcceptLogonRec;
        UpdateFileID: ShortString;
      );
  end;
    
//----------------------------------------------------------------
// 로그아웃 요청 패킷. --> 이 프로젝트에서는 사용하지 않음. 
//----------------------------------------------------------------

const
  REQ_LOGOUT = $02; {
    C->S. 정보 없음. 로그오프 요청.
    S->C. 정보 없음. 접속 종료.
  }

  
//----------------------------------------------------------------
//!! 핑 요청.
//----------------------------------------------------------------

const
  REQ_PING   = $03; {
    현재 사용중인 서버 DLL은 30초 동안 아무런 패킷이 없으면 접속을 끊는다.
    10초 단위로 이 패킷을 날려 클라의 유효함을 서버에 알린다.

    데이터 없음.
  }
  
  

//----------------------------------------------------------------
// 채팅 요청 패킷 
//----------------------------------------------------------------

const
  REQ_CHAT = $10; {
    C->S
      문자열길이(1) + 채팅 문자열

    S->C
      채팅타입(1) + 전송자 길이 및 문자열 + 채팅 문자열 
  }

type  
  TChatType = (ctNormal, ctNotify);

  PReqChatPkt = ^TReqChatPkt;
  TReqChatPkt = packed record
    case Integer of
      // C->S
      0:
        (
          ChatData_CS: TU8Array;
        );
      1:
        (
          ChatType: TChatType;
          ChatData_SC: TU8Array;
        );
  end; 


implementation

initialization

finalization

end.
