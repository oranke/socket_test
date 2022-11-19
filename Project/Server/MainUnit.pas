{-----------------------------------------------------------------------------
 Unit Name: MainUnit
 Author:    ����ĳ��
 Date:      2022-11-18
 Purpose:
    ���� ���� DLL �׽�Ʈ.

 History:
-----------------------------------------------------------------------------}


unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,

  SSockDll, zhTypes,

  ExtCtrls, XPMan, StdCtrls;

type
  TMainForm = class(TForm)
    TickTimer: TTimer;
    XPManifest1: TXPManifest;
    Start_Button: TButton;
    CliConnID_Edit: TEdit;
    Label1: TLabel;
    Packet01_Button: TButton;
    Packet02_Button: TButton;
    AccessDenine_Button: TButton;
    procedure TickTimerTimer(Sender: TObject);
    procedure Start_ButtonClick(Sender: TObject);
    procedure Packet01_ButtonClick(Sender: TObject);
    procedure Packet02_ButtonClick(Sender: TObject);
    procedure AccessDenine_ButtonClick(Sender: TObject);
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


procedure PacketEvent(aConnIndex: U16; aPID: U8; aDataLen: U16; aData: PU8Array; aParam: Pointer); stdcall;
begin
  WriteLn(aConnIndex, ' ��Ŷ�Է� : ', aPID, '. Len: ', aDataLen);
end;

procedure AcceptEvent(aConnIndex: U16; aPeer: PAnsiChar; aParam: Pointer); stdcall;
begin
  WriteLn(aConnIndex, ' ���Ʈ from ', aPeer);

  TMainForm(aParam).CliConnID_Edit.Text := IntToStr(aConnIndex);

end;

procedure ChkOpenEvent(aConnIndex: U16; aParam: Pointer); stdcall;
begin
  WriteLn(aConnIndex, ' üũ����');

end;

procedure AccDenineEvent(aConnIndex: U16; aReason: U16; aCode: U32; aPeer: PAnsiChar; aParam: Pointer); stdcall;
begin
  WriteLn(aConnIndex, ' �＼�� ����: ', aReason, ', ', aCode, '. from ', aPeer);

end;

{ TMainForm }

constructor TMainForm.Create(aOwner: TComponent);
begin
  inherited;

  // ���� ����
  CreateServer('Test', 10, 10101, $FEFF8744, PacketEvent, AcceptEvent, ChkOpenEvent, AccDenineEvent, Self);
  SetCurServer('Test');

  AllocConsole();
end;

destructor TMainForm.Destroy;
begin
  // ���� ����
  DeleteServer('Test');

  inherited;
end;

// ������ ƽ�� ����.
procedure TMainForm.TickTimerTimer(Sender: TObject);
begin
  TickAllServer(GetTickCount);
end;

procedure TMainForm.Start_ButtonClick(Sender: TObject);
begin
  if not IsStarted then
    StartServer
  else
    StopServer
end;

// PID - $03 ���� �׽�Ʈ.
procedure TMainForm.Packet01_ButtonClick(Sender: TObject);
var
  DataLen: WORD;
  Data: Integer;
begin
  Data := 12345;
  DataLen := SizeOf(Integer);

  WriteLn(IsEnableToSend(StrToIntDef(CliConnID_Edit.Text, 0), SizeOf(Word)));
  ToCli_SendPacketWArr(StrToIntDef(CliConnID_Edit.Text, 0), $03, 1, [DataLen], [@Data]);
end;

procedure TMainForm.Packet02_ButtonClick(Sender: TObject);
var
  a: WORD;
begin
  a := 33;
  ToCli_BeginPacket(StrToIntDef(CliConnID_Edit.Text, 0), $03);
  ToCli_SendBufferWArr(StrToIntDef(CliConnID_Edit.Text, 0), 1, [SizeOf(WORD)], [@a]);
  ToCli_EndPacket(StrToIntDef(CliConnID_Edit.Text, 0));
end;

procedure TMainForm.AccessDenine_ButtonClick(Sender: TObject);
begin
  ToCli_AccessDenine(StrToIntDef(CliConnID_Edit.Text, 0), 32323, 52525);
end;

end.
