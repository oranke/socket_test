{-----------------------------------------------------------------------------
 Unit Name: MainUnit
 Author:    ����ĳ��
 Date:      2022-11-18
 Purpose:
    Ŭ�� ���� DLL �׽�Ʈ.
    ���� ä�� Ŭ���̾�Ʈ

 History:
-----------------------------------------------------------------------------}

{$IOCHECKS OFF}

unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  ExtCtrls, XPMan,

  zhTypes, 
  CSockDll,
  ChatPacket;

type
  TMainForm = class(TForm)
    Connect_Button: TButton;
    SvrIP_Edit: TEdit;
    SvrPort_Edit: TEdit;
    TickTimer: TTimer;
    XPManifest1: TXPManifest;
    Label1: TLabel;
    ID_Edit: TEdit;
    History_Memo: TMemo;
    ChatText_Edit: TEdit;
    ChatSend_Button: TButton;
    procedure TickTimerTimer(Sender: TObject);
    procedure Connect_ButtonClick(Sender: TObject);
    procedure ChatSend_ButtonClick(Sender: TObject);
  private
    { Private declarations }
    fNextPingTime: TTick;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

//uses
  //;
  
var
  ugCurrentTime: DWORD;
  ugLastGetTime: DWORD;
  //ugDateTime: TDateTime;

procedure InitTime;
begin
  ugCurrentTime := 1; //TTick(MAXINT) + 100;//
  ugLastGetTime := GetTickCount();
end;

procedure UpdateTime;
var
  NewGetTime: DWord;
begin
  NewGetTime := GetTickCount;
  //Inc(ugCurrentTime, DWORD(NewGetTime - ugLastGetTime) div 3);
  Inc(ugCurrentTime, DWORD(NewGetTime - ugLastGetTime));
  ugLastGetTime := NewGetTime;

  //SetConsoleTitle(PAnsiChar(IntToStr(CurrentTime)));
end;

function CurrentTime: TTick;
begin
  Result := ugCurrentTime;
end;


procedure AccDenineEvent (aReason: U16; aCode: U32; aParam: Pointer); stdcall;
begin
  WriteLn('�＼�� �ź� : ', aReason, ', ', aCode);
  with TMainForm(aParam) do
  begin
    History_Memo.Lines.Add('�＼�� �ź� ' + IntToStr(aReason) + ', ' + IntToStr(aCode));
  end;
end;

procedure ReqDenineEvent (aReason: U16; aCode: U32; aParam: Pointer); stdcall;
begin
  WriteLn('��û �ź� : ', aReason, ', ', aCode);
  with TMainForm(aParam) do
  begin
    History_Memo.Lines.Add('��û �ź� ' + IntToStr(aReason) + ', ' + IntToStr(aCode));
  end;
end;

procedure PacketEvent (aPID: U8; aDataLen: U16; aData: PU8Array; aParam: Pointer); stdcall;
var
  Offset: Integer; 
  SenderID: AnsiString;
  ChatStr: AnsiString;
begin
  WriteLn('��Ŷ���� : ', aPID, '. Len: ', aDataLen);

  with TMainForm(aParam) do
  case aPID of
    REQ_LOGIN:
    with PReqLogonPkt(aData)^ do
    begin
      if Boolean(AcceptInfo.Flag) then
      begin
        History_Memo.Lines.Add('�α��� ���');

        // 10�� �� �� ����
        fNextPingTime := CurrentTime + 10 * 1000;
      end else
        History_Memo.Lines.Add('�α��� �ź�');
      //TAcceptLogonRec(aData^).

    end;

    REQ_CHAT:
    with PReqChatPkt(aData)^ do
    begin
      Offset := ChatData_SC[0] + 1;

      SenderID := PShortString(@ChatData_SC[0])^;
      ChatStr  := PShortString(@ChatData_SC[Offset])^;

      case ChatType of
        ctNormal:
        begin
          History_Memo.Lines.Add(Format('[ä��: %s] %s', [SenderID, ChatStr])); 
        end;

        ctNotify:
        begin
          History_Memo.Lines.Add(Format('[����] %s', [ChatStr])); 
        end;
      end; 


    end;

  end; 
end;

procedure ConnectEvent   (aStatus: TConnEventStatus; aParam: Pointer); stdcall;
var
  UserID, Key: AnsiString;
  UserID_Len, Key_Len: U8;
begin
  with TMainForm(aParam) do
  begin
    fNextPingTime:= TTick(-1);

    case aStatus of
      ctConnected    :
      begin
        WriteLn('���� ����');
        History_Memo.Lines.Add('���� ����');

        UserID := ID_Edit.Text;
        Key    := '123456';

        UserID_Len := U8(Length(UserID));
        Key_Len := U8(Length(Key));

        // �α׿� ��Ŷ ����
        ToSvr_SendPacket2WArr(REQ_LOGIN, LGN_NORMAL, 5,
        // Version      IDLen       ID          KeyLen      Key
          [SizeOf(U32), SizeOf(U8), UserID_Len, SizeOf(U8), Key_Len],
          [@ChatPacket.PKT_VERSION, @UserID_Len, PAnsiChar(UserID), @Key_Len, PAnsiChar(Key)]
        );
      end;
    
      ctDisconnected :
      begin
        WriteLn('���� ����');
        History_Memo.Lines.Add('���� ����');
      end;

      ctConnectFailed:
      begin
        WriteLn('���� ����');
        History_Memo.Lines.Add('���� ����');
      end;
    end;
  end;
end;


{ TMainForm }

constructor TMainForm.Create(aOwner: TComponent);
begin
  inherited;

  fNextPingTime:= TTick(-1);

  // Ŭ���̾�Ʈ ����
  CreateClient('ChatCli',
    ChatPacket.PKT_XOR_KEY,
    AccDenineEvent, ReqDenineEvent, PacketEvent, ConnectEvent,
    Self
  );

  SetCurClient('ChatCli');

  Randomize; 
  ID_Edit.Text := 'user_' + IntToHex(Random(65536), 4); 
end;

destructor TMainForm.Destroy;
begin
  // Ŭ���̾�Ʈ ����
  DeleteClient('ChatCli');

  inherited;
end;

// Ŭ���̾�Ʈ ��⿡ ƽ��
procedure TMainForm.TickTimerTimer(Sender: TObject);
begin
  UpdateTime;

  if fNextPingTime < CurrentTime then
  begin
    // �� �����ϰ�
    ToSvr_SendPacketWArr(REQ_PING, 0, [], []);
    // ���� �� �ð� ����.
    fNextPingTime := CurrentTime + 10 * 1000;
  end;

  TickAllClient;
end;

procedure TMainForm.Connect_ButtonClick(Sender: TObject);
begin
  if IsConnected then
    CloseConnection
  else
    OpenConnection(PChar(SvrIP_Edit.Text), StrToIntDef(SvrPort_Edit.Text, 10102));
end;

procedure TMainForm.ChatSend_ButtonClick(Sender: TObject);
var
  ChatStr: AnsiString;
  ChatLen: U8; 
begin
  ChatStr := ChatText_Edit.Text;
  ChatLen := U8(Length(ChatStr));
  if ChatLen <= 0 then Exit;

  ToSvr_SendPacketWArr(REQ_CHAT, 2,
    [SizeOf(U8), ChatLen],
    [@ChatLen, PChar(ChatStr)]
  );
end;

initialization
  InitTime;

end.
