{-----------------------------------------------------------------------------
 Unit Name: MainUnit
 Author:    오랑캐꽃
 Date:      2022-11-18
 Purpose:
    서버 소켓 DLL 테스트.

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
    Button1: TButton;
    procedure TickTimerTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
  WriteLn(aConnIndex, ' 패킷입력 : ', aPID, '. Len: ', aDataLen);
end;

procedure AcceptEvent(aConnIndex: U16; aPeer: PAnsiChar; aParam: Pointer); stdcall;
begin
  WriteLn(aConnIndex, ' 억셉트 from ', aPeer);

end;

procedure ChkOpenEvent(aConnIndex: U16; aParam: Pointer); stdcall;
begin
  WriteLn(aConnIndex, ' 체크오픈');

end;

procedure AccDenineEvent(aConnIndex: U16; aReason: U16; aCode: U32; aPeer: PAnsiChar; aParam: Pointer); stdcall;
begin
  WriteLn(aConnIndex, ' 억세스 디나인: ', aReason, ', ', aCode, '. from ', aPeer);

end;

{ TMainForm }

constructor TMainForm.Create(aOwner: TComponent);
begin
  inherited;

  // 서버 생성
  CreateServer('Test', 10, 10101, $FEFF8744, PacketEvent, AcceptEvent, ChkOpenEvent, AccDenineEvent, Self);
  SetCurServer('Test');

  AllocConsole();
end;

destructor TMainForm.Destroy;
begin
  // 서버 제거
  DeleteServer('Test');

  inherited;
end;

// 서버에 틱을 공급.
procedure TMainForm.TickTimerTimer(Sender: TObject);
begin
  TickAllServer(GetTickCount);
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  if not IsStarted then
    StartServer
  else
    StopServer
end;

end.
