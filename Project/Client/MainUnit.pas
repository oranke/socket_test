unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  CSockDll,
  zhTypes, ExtCtrls, XPMan;

type
  TMainForm = class(TForm)
    Connect_Button: TButton;
    RecreateCli_Button: TButton;
    ConnKey_Edit: TEdit;
    SvrIP_Edit: TEdit;
    SvrPort_Edit: TEdit;
    Packet1_Button: TButton;
    Packet2_Button: TButton;
    Packet3_Button: TButton;
    Packet4_Button: TButton;
    TickTimer: TTimer;
    XPManifest1: TXPManifest;
    procedure TickTimerTimer(Sender: TObject);
    procedure RecreateCli_ButtonClick(Sender: TObject);
    procedure Connect_ButtonClick(Sender: TObject);
    procedure Packet1_ButtonClick(Sender: TObject);
    procedure Packet2_ButtonClick(Sender: TObject);
    procedure Packet3_ButtonClick(Sender: TObject);
    procedure Packet4_ButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}


procedure AccDenineEvent (aReason: U16; aCode: U32; aParam: Pointer); stdcall;
begin
  WriteLn('�＼�� �ź� : ', aReason, ', ', aCode);
end;

procedure ReqDenineEvent (aReason: U16; aCode: U32; aParam: Pointer); stdcall;
begin
  WriteLn('��û �ź� : ', aReason, ', ', aCode);
end;

procedure PacketEvent    (aPID: U8; aDataLen: U16; aData: PU8Array; aParam: Pointer); stdcall;
begin
  WriteLn('��Ŷ���� : ', aPID, '. Len: ', aDataLen);
end;

procedure ConnectEvent   (aStatus: TConnEventStatus; aParam: Pointer); stdcall;
begin
  case aStatus of
    ctConnected    : WriteLn('���� ����');
    ctDisconnected : WriteLn('���� ����');
    ctConnectFailed: WriteLn('���� ����');
  end;
end;


{ TMainForm }

constructor TMainForm.Create(aOwner: TComponent);
begin
  inherited;

  // Ŭ���̾�Ʈ ����
  CreateClient('TestCli',
    $FEFF8744,
    //$3AA04B0F,
    AccDenineEvent, ReqDenineEvent, PacketEvent, ConnectEvent,
    Self
  );

  SetCurClient('TestCli');

  AllocConsole();
  
end;

destructor TMainForm.Destroy;
begin
  // Ŭ���̾�Ʈ ����
  DeleteClient('TestCli');

  inherited;
end;

// Ŭ���̾�Ʈ ��⿡ ƽ��
procedure TMainForm.TickTimerTimer(Sender: TObject);
begin
  TickAllClient;
end;


// ���ο� Ű�� Ŭ���̾�Ʈ �����
procedure TMainForm.RecreateCli_ButtonClick(Sender: TObject);
var
  PacketXORKey: U32; 
begin
  PacketXORKey := U32(StrToInt64Def(ConnKey_Edit.Text, $FEFF8744));
  DeleteClient('TestCli');

  CreateClient('TestCli',
    PacketXORKey,
    AccDenineEvent, ReqDenineEvent, PacketEvent, ConnectEvent,
    Self
  );

  SetCurClient('TestCli');
end;

procedure TMainForm.Connect_ButtonClick(Sender: TObject);
begin
  if IsConnected then
    CloseConnection
  else
    OpenConnection(PChar(SvrIP_Edit.Text), StrToIntDef(SvrPort_Edit.Text, 10101));
end;

// PID - $03 �� ��Ŷ ���� �׽�Ʈ.
procedure TMainForm.Packet1_ButtonClick(Sender: TObject);
var
  Data: Integer;
  DataLen: WORD;

  Bufs: Pointer;
begin
  Data := 12345;
  DataLen := SizeOf(Integer);

  Bufs := @Data;

  WriteLn('����! PID $03');
  ToSvr_SendPacketW($03, 1, @DataLen, @Bufs);
end;

// ������ ��Ŷ�� ��ƿ��Ƽ �Լ��� ����� ���� ���ϰ�
procedure TMainForm.Packet2_ButtonClick(Sender: TObject);
var
  Data: Integer;
  DataLen: WORD;
begin
  Data := 12345;
  DataLen := SizeOf(Integer);

  WriteLn('����! PID $03');
  ToSvr_SendPacketWArr($03, 1, [DataLen], [@Data]);
end;

// PID - $04 ���� �׽�Ʈ.
procedure TMainForm.Packet3_ButtonClick(Sender: TObject);
var
  BufLen: array of Word;
  Buffs : array of Pointer;
  k : U32; 
begin
  SetLength(BufLen, 2);
  SetLength(Buffs, 2);

  k := 32;
  BufLen[0] := SizeOf(U32);
  BufLen[1] := SizeOf(U32);
  Buffs[0] := @k;
  Buffs[1] := @k;

  if IsEnableToSend(SizeOf(U32) * 2) then
  begin
    WriteLn('����! PID $04');
    ToSvr_SendPacketWArr($04, 2, BufLen, Buffs);
  end;
end;

// begin~end ������ �������� ���� �׽�Ʈ.
procedure TMainForm.Packet4_ButtonClick(Sender: TObject);
var
  k: U32;
begin
  //if IsEnableToSend(100) then
  begin
    WriteLN('����! PID $05');
    WriteLn(ToSvr_BeginPacket($05));

    WriteLn(ToSvr_SendBuffer(@k, SizeOf(U32)));
    WriteLn(ToSvr_SendBuffer(@k, SizeOf(U32)));
    WriteLn(ToSvr_SendBuffer(@k, SizeOf(U32)));

    WriteLn(ToSvr_EndPacket);
  end;
end;

end.
