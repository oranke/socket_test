{-----------------------------------------------------------------------------
 Unit Name: ChatPacket
 Author:    oranke
 Date:      2022-11-21
 Purpose:
   ä�ü��� <-> Ŭ���̾�Ʈ ������ �����Ŷ ���� ����Ʈ.

 History:

-----------------------------------------------------------------------------}


unit ChatPacket;

interface

uses
  Windows, zhTypes;

const
  // ��Ŷ�� ����.
  PKT_VERSION : U32 = 2022112101;

const
  PKT_XOR_KEY : U32 = $A33B2EF0; // ��Ŷ ���ڵ� / ���ڵ� Ű.


{///////////////////////////////////////////////////////////////////////////////

  ���ٰź�, ��û�ź� ����� ����.
  $F000 �̻��� ���� ��� DLL�� �̹� ���ǵǾ�����.

///////////////////////////////////////////////////////////////////////////////}

  // Access Denine reasons.
  ADID_NONE         = $0000;  // ��������.
  ADID_INVALID_USER = $0001;  // ��������


  // Request Denine reasons.
  RDID_NONE         = $0000;  // �źξ���.
  // ��û�źν� ��û��Ŷ�� ������̵� MakeWord�� Word�� ���� �����Ѵ�.


{///////////////////////////////////////////////////////////////////////////////

  ��κ��� ��Ŷ�� PID�� �׿� ���� SPID�� ���еȴ�.
  SPID�� �����ϴ� �Լ��� ��� DLL ���� �����Ǿ� �ִ�.

///////////////////////////////////////////////////////////////////////////////}

type
  //!! ��Ŷ ó���� ����.
  PPacketProcRet = ^TPacketProcRet;
  TPacketProcRet = packed record
    ADID, RDID: Word;
    Code: U32;
  end;



//----------------------------------------------------------------
// �α��� ��û ��Ŷ
//----------------------------------------------------------------

const
  REQ_LOGIN = $01;
    // SPID
    LGN_NORMAL = $01;
    {
      ����(4) + NameLen + Name + KeyLen + Key
      ����(4) + KeyLen + Key
    }

    LGN_ADMIN = $02;
    {
      �����ڷ� ����.
      ����(4) + NameLen + Name + KeyLen + Key

      �� ������Ʈ������ ������� ����.
    }


type
  // Ŭ���̾�Ʈ Ÿ��. �Ϲ�, ����, ������.
  // �����ʹ� ���ε��� �߰� �� ���� ����. -> �� ������Ʈ������ ������� ����.
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
// �α׾ƿ� ��û ��Ŷ. --> �� ������Ʈ������ ������� ����. 
//----------------------------------------------------------------

const
  REQ_LOGOUT = $02; {
    C->S. ���� ����. �α׿��� ��û.
    S->C. ���� ����. ���� ����.
  }

  
//----------------------------------------------------------------
//!! �� ��û.
//----------------------------------------------------------------

const
  REQ_PING   = $03; {
    ���� ������� ���� DLL�� 30�� ���� �ƹ��� ��Ŷ�� ������ ������ ���´�.
    10�� ������ �� ��Ŷ�� ���� Ŭ���� ��ȿ���� ������ �˸���.

    ������ ����.
  }
  
  

//----------------------------------------------------------------
// ä�� ��û ��Ŷ 
//----------------------------------------------------------------

const
  REQ_CHAT = $10; {
    C->S
      ���ڿ�����(1) + ä�� ���ڿ�

    S->C
      ä��Ÿ��(1) + ������ ���� �� ���ڿ� + ä�� ���ڿ� 
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
