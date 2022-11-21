{-----------------------------------------------------------------------------
 Unit Name: MainUnit
 Author:    오랑캐꽃
 Date:      2022-11-18
 Purpose:
    서버 소켓 DLL 테스트.
    간이 채팅 서버

 History:
-----------------------------------------------------------------------------}

{$IOCHECKS OFF}

{$DEFINE DEBUG_BUILD}

unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, XPMan, StdCtrls,

  zhTypes, zhKeyList,
  SSockDll,
  ChatPacket;

type
  TMainForm = class(TForm)
    TickTimer: TTimer;
    XPManifest1: TXPManifest;
    Start_Button: TButton;
    ConnListBox: TListBox;
    Label2: TLabel;
    NotiText_Edit: TEdit;
    NotiSend_Button: TButton;
    procedure TickTimerTimer(Sender: TObject);
    procedure Start_ButtonClick(Sender: TObject);
    procedure ConnListBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure NotiSend_ButtonClick(Sender: TObject);
    procedure ConnListBoxDblClick(Sender: TObject);
  private
    { Private declarations }
    fConnIndexList: TList;
    fConnIndexMap : TU32KeyList; 
    

    //function GetCon
    procedure NewConnection(aConnIndex: U16; const aPeer: String);
    procedure RemConnection(aConnIndex: U16);

    function GetConnListIndex(aConnIndex: U16): Integer; 
  protected
    function PrepareLogin(const aConnIndex: LongInt; const aReqLogonPkt: PReqLogonPkt; aPktLen: Integer): TPacketProcRet;
    function ProcessPing(const aConnIndex: LongInt; aPktLen: Integer): TPacketProcRet;
    function ProcessChat(const aConnIndex: LongInt; const aReqChatPkt: PReqChatPkt; aPktLen: Integer): TPacketProcRet;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  ShowCliUnit;

type
  TConnInfo = class
  private
    fConnIndex : U16;
    fUserID    : AnsiString;
    fPeer      : AnsiString;
    fShowCliForm: TShowCliForm;
  protected
  public
    constructor Create(aConnIndex: U16; const aPeer: AnsiString);
    destructor Destroy; override;

    procedure SetUserID(const aUserID: AnsiString);

    property ConnIndex: U16 read fConnIndex;
    property UserID: AnsiString read fUserID;
    property Peer: AnsiString read fPeer;
  end;

{ TConnInfo }

constructor TConnInfo.Create(aConnIndex: U16; const aPeer: AnsiString);
begin
  fConnIndex:= aConnIndex;
  fPeer     := aPeer;

  fShowCliForm:= TShowCliForm.Create(nil);
  fShowCliForm.SetInfo(aConnIndex, aPeer);
end;

destructor TConnInfo.Destroy;
begin
  fShowCliForm.Free; 

  inherited;
end;

procedure TConnInfo.SetUserID(const aUserID: AnsiString);
begin
  fUserID   := aUserID;
  fShowCliForm.SetUser(aUserID);
end;

//-----------------------------------------------------------------------------
procedure PacketEvent(aConnIndex: U16; aPID: U8; aDataLen: U16; aData: PU8Array; aParam: Pointer); stdcall;
var
  ppr: TPacketProcRet;
begin
{$IFDEF DEBUG_BUILD}
  WriteLn(aConnIndex, ' 패킷입력 : ', aPID, '. Len: ', aDataLen);
{$ENDIF}

  with TMainForm(aParam) do
  case aPID of
    REQ_LOGIN   : ppr := PrepareLogin(aConnIndex, @aData[0], aDataLen);
    REQ_PING    : ppr := ProcessPing(aConnIndex, aDataLen);
    REQ_CHAT    : ppr := ProcessChat(aConnIndex, @aData[0], aDataLen);
  else
    ppr.ADID := ADID_WRONGPACKET;
  end;

  if ppr.ADID <> 0 then
  begin
    ToCli_AccessDenine(aConnIndex, ppr.ADID, ppr.Code);
  end else
  if ppr.RDID <> 0 then
  begin
    ToCli_RequestDenine(aConnIndex, ppr.RDID, ppr.Code);
  end;

end;

procedure AcceptEvent(aConnIndex: U16; aPeer: PAnsiChar; aParam: Pointer); stdcall;
begin
  WriteLn(aConnIndex, ' 억셉트 from ', aPeer);

  TMainForm(aParam).NewConnection(aConnIndex, aPeer);
end;

procedure ChkOpenEvent(aConnIndex: U16; aParam: Pointer); stdcall;
begin
  WriteLn(aConnIndex, ' 체크오픈');

  TMainForm(aParam).RemConnection(aConnIndex);
end;

procedure AccDenineEvent(aConnIndex: U16; aReason: U16; aCode: U32; aPeer: PAnsiChar; aParam: Pointer); stdcall;
begin
  WriteLn(aConnIndex, ' 억세스 디나인: ', aReason, ', ', aCode, '. from ', aPeer);

end;


{ TMainForm }

constructor TMainForm.Create(aOwner: TComponent);
begin
  inherited;

  fConnIndexList:= TList.Create;
  fConnIndexMap := TU32KeyList.Create(100);

  // 서버 생성
  CreateServer('ChatSvr',
    100,                    // 클라이언트 갯수
    10102,                  // 바인딩 포트
    ChatPacket.PKT_XOR_KEY, // 인코딩 키
    PacketEvent,
    AcceptEvent,
    ChkOpenEvent,
    AccDenineEvent,
    Self
  );

  SetCurServer('ChatSvr');

  //AllocConsole();
end;

destructor TMainForm.Destroy;
var
  i: Integer;
begin
  // 서버 제거
  DeleteServer('ChatSvr');

  // 연결목록 정리 
  for i:= 0 to fConnIndexList.Count-1 do
    TConnInfo(fConnIndexList[i]).Free; 
  fConnIndexList.Free; 

  fConnIndexMap.Free; 
  inherited;
end;

procedure TMainForm.NewConnection(aConnIndex: U16; const aPeer: String);
var
  connInfo : TConnInfo;
begin
  connInfo := TConnInfo.Create(aConnIndex, aPeer);
  fConnIndexList.Add(connInfo);
  fConnIndexMap[aConnIndex] := connInfo; 

  ConnListBox.Count := fConnIndexList.Count;
  ConnListBox.Invalidate;
end;

procedure TMainForm.RemConnection(aConnIndex: U16);
var
  i: Integer;
begin
  i := GetConnListIndex(aConnIndex);
  if i >= 0 then
  begin
    TConnInfo(fConnIndexList[i]).Free;
    fConnIndexList.Delete(i);
    if fConnIndexMap.Has(aConnIndex) then fConnIndexMap.Remove(aConnIndex);

    ConnListBox.Count := fConnIndexList.Count;
    ConnListBox.Invalidate;
  end;
end;

function TMainForm.GetConnListIndex(aConnIndex: U16): Integer;
var
  i: Integer; 
begin
  Result := -1;
  for i:= 0 to fConnIndexList.Count-1 do
  if TConnInfo(fConnIndexList[i]).ConnIndex = aConnIndex then
  begin
    Result := i;
    Exit;
  end; 

end;

// 서버에 틱을 공급.
procedure TMainForm.TickTimerTimer(Sender: TObject);
begin
  TickAllServer(GetTickCount);
end;

procedure TMainForm.Start_ButtonClick(Sender: TObject);
begin
  if not IsStarted then
  begin
    StartServer;
    Caption := '서버 테스트 - 채팅서버 (시작됨)';
  end else
  begin
    StopServer;
    Caption := '서버 테스트 - 채팅서버';
  end;
end;

function TMainForm.PrepareLogin(const aConnIndex: Integer;
  const aReqLogonPkt: PReqLogonPkt; aPktLen: Integer): TPacketProcRet;
var
  connInfo: TConnInfo;
  //i: Integer;

  PLogonData: PU8Array;
  HeadLen: Integer;

  Offset: Integer;
  UserID: AnsiString;
  Key: AnsiString;

  AcceptLogonRec: TAcceptLogonRec; 
  //ClientVersion: U32;
begin
{$IFDEF DEBUG_BUILD}
  WriteLn('로그인 요청: ', aConnIndex);
{$ENDIF}

  Result.RDID := RDID_NONE;
  Result.ADID := ADID_WRONGPACKET;

  {
  i := GetConnListIndex(aConnIndex);
  if i < 0 then Exit;
  connInfo := fConnIndexList[i];
  }
  connInfo :=fConnIndexMap[aConnIndex];
  if not Assigned(connInfo) then Exit; 

  with aReqLogonPkt^ do
  begin
    PLogonData := @LogonData[0];
    //ClientVersion := Version;
    //         SPID  Version
    HeadLen := 1 +   SizeOf(U32);

    case SPID of
      LGN_NORMAL:
      begin
        Offset := PLogonData^[0] + 1;

        // 클라로부터 패킷길이 검증.
        //                       IDLen ID               KeyLen Key
        if aPktLen <> HeadLen +  1 +   PLogonData^[0] + 1 +    PLogonData^[Offset] then Exit;

        UserID := PShortString(@PLogonData^[0])^;
        Key    := PShortString(@PLogonData^[Offset])^;

      {$IFDEF DEBUG_BUILD}
        WriteLn('일반 클라 로그인');
        WriteLn('아이디 : ', UserID);
        WriteLn('키 : ', Key);
      {$ENDIF}

        // 유저아이디와 클라 처리. 여기서는 아이디만 채워지면 그냥 허용.
        if Length(UserID) > 0 then
        begin
          // 클라에 로그인 허용 알림.
          AcceptLogonRec.Flag := 1;
          AcceptLogonRec.CliType := cliTypeNormal;
          ToCli_SendPacketWArr(aConnIndex, REQ_LOGIN, 1, [SizeOf(TAcceptLogonRec)], [@AcceptLogonRec]);
          connInfo.SetUserID(UserID);

          connInfo.fShowCliForm.History_Memo.Lines.Add('접속 확인 ' + UserID);
          
          ConnListBox.Invalidate;
        end else
        begin
          // 클라에 로그인 실패 알림.
          AcceptLogonRec.Flag := 0;
          AcceptLogonRec.CliType := cliTypeNormal;
          ToCli_SendPacketWArr(aConnIndex, REQ_LOGIN, 1, [SizeOf(TAcceptLogonRec)], [@AcceptLogonRec]);

          connInfo.fShowCliForm.History_Memo.Lines.Add('접속 실패 ');
          // 억세스 디나인도 전송.
          ToCli_AccessDenine(aConnIndex, ADID_INVALID_USER);
        end;
      end;

    end;


  end;

  Result.ADID := ADID_NONE;

end;

function TMainForm.ProcessPing(const aConnIndex: Integer; aPktLen: Integer): TPacketProcRet;
var
  connInfo: TConnInfo;
begin
  Result.RDID := RDID_NONE;
  Result.ADID := ADID_WRONGPACKET;

  if aPktLen <> 0 then Exit;

  //i := GetConnListIndex(aConnIndex);
  //if i < 0 then Exit;
  //connInfo := fConnIndexList[i];
  connInfo :=fConnIndexMap[aConnIndex];
  if not Assigned(connInfo) then Exit;

  if Length(connInfo.UserID) = 0 then Exit;

{$IFDEF DEBUG_BUILD}
  WriteLn('핑 전송 ', connInfo.UserID);
{$ENDIF}

  connInfo.fShowCliForm.History_Memo.Lines.Add('핑 전송');

  Result.ADID := ADID_NONE;
end;

function TMainForm.ProcessChat(const aConnIndex: Integer; const aReqChatPkt: PReqChatPkt; aPktLen: Integer): TPacketProcRet;
var
  connInfo: TConnInfo;

  ChatType : TChatType;

  ChatStr: AnsiString;
  ChatLen: U8;

  SenderID: AnsiString;
  SenderLen: U8;

  i: Integer;
begin
  Result.RDID := RDID_NONE;
  Result.ADID := ADID_WRONGPACKET;

  connInfo :=fConnIndexMap[aConnIndex];
  if not Assigned(connInfo) then Exit;

  if Length(connInfo.UserID) = 0 then Exit;

  // 클라로부터 패킷길이 검증.
  //            Len  Msg
  if aPktLen <> 1 +  aReqChatPkt^.ChatData_CS[0] then Exit;

  ChatType := ctNormal;
  
  ChatStr := PShortString(@aReqChatPkt^.ChatData_CS[0])^;
  ChatLen := U8(Length(ChatStr));

  connInfo.fShowCliForm.History_Memo.Lines.Add('채팅 요청: ' + ChatStr);

  SenderID  := connInfo.UserID;
  SenderLen := U8(Length(SenderID));

  for i:= 0 to fConnIndexList.Count-1 do
  with TConnInfo(fConnIndexList[i]) do
  if Length(UserID) > 0 then
  begin
    ToCli_SendPacketWArr(ConnIndex, REQ_CHAT, 5,
    // ChatType           Sender                 Msg
      [SizeOf(TChatType), SizeOf(U8), SenderLen, SizeOf(U8), ChatLen],
      [@ChatType, @SenderLen, PAnsiChar(SenderID), @ChatLen, PAnsiChar(ChatStr)]
    );
  end;

{$IFDEF DEBUG_BUILD}
  WriteLn('채팅 전송 ', connInfo.UserID, ', ', ChatStr);
{$ENDIF}


  Result.ADID := ADID_NONE;
end;



procedure TMainForm.ConnListBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  connInfo: TConnInfo;
  str: AnsiString;
  DrawRt: TRect;
begin
  with TListBox(Control).Canvas do
  begin
    if Index < 0 then Exit;
    if Index >= fConnIndexList.Count then Exit;

    FillRect(Rect);

    connInfo := fConnIndexList[Index];

    DrawRt := Classes.Rect(Rect.Left+1, Rect.Top+1, Rect.Right div 2 -1, Rect.Bottom-1);
    str := Format('%d - %s', [connInfo.ConnIndex, connInfo.Peer]);
    DrawText(Handle, PAnsiChar(str), Length(str), DrawRt, DT_NOPREFIX	or DT_LEFT or DT_VCENTER or DT_SINGLELINE);

    if Length(connInfo.UserID) > 0 then
    begin
      DrawRt := Classes.Rect(Rect.Right div 2 +1, Rect.Top+1, Rect.Right -1, Rect.Bottom-1);
      str := Format('ID: %s', [connInfo.UserID]);
      DrawText(Handle, PAnsiChar(str), Length(str), DrawRt, DT_NOPREFIX	or DT_LEFT or DT_VCENTER or DT_SINGLELINE);
    end;


  end;
end;

procedure TMainForm.ConnListBoxDblClick(Sender: TObject);
var
  connInfo: TConnInfo;
begin
  if ConnListBox.ItemIndex < 0 then Exit;
  if ConnListBox.ItemIndex >= fConnIndexList.Count then Exit;

  connInfo := fConnIndexList[ConnListBox.ItemIndex];
  connInfo.fShowCliForm.Show;
//
end;

procedure TMainForm.NotiSend_ButtonClick(Sender: TObject);
var
  NotiMsg: AnsiString;
  NotiLen: U8;
  SenderLen: U8;

  ChatType: TChatType; 

  i: Integer;
begin
  NotiMsg := NotiText_Edit.Text;
  NotiLen := U8(Length(NotiMsg));

  SenderLen := 0;

  ChatType := ctNotify;

  for i:= 0 to fConnIndexList.Count-1 do
  with TConnInfo(fConnIndexList[i]) do
  if Length(UserID) > 0 then
  begin
    ToCli_SendPacketWArr(ConnIndex, REQ_CHAT, 4,
    // ChatType           Sender(0)               Msg
      [SizeOf(TChatType), SizeOf(U8), SizeOf(U8), NotiLen],
      [@ChatType, @SenderLen, @NotiLen, PAnsiChar(NotiMsg)]
    );
  end;
end;


end.
