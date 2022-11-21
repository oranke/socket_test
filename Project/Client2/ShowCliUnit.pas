unit ShowCliUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  zhTypes,
  SSockDll,
  ChatPacket;


type
  TShowCliForm = class(TForm)
    StatusLabel: TLabel;
    History_Memo: TMemo;
    NotiText_Edit: TEdit;
    NotiSend_Button: TButton;
    procedure NotiSend_ButtonClick(Sender: TObject);
  private
    { Private declarations }
    fConnIndex: U16;
    fUserID, fPeer: String; 
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetInfo(aConnIndex: U16; const aPeer: String);
    procedure SetUser(const aUserID: String);
  end;

var
  ShowCliForm: TShowCliForm;

implementation

{$R *.dfm}

{ TShowCliForm }

constructor TShowCliForm.Create(aOwner: TComponent);
begin
  inherited;

end;

destructor TShowCliForm.Destroy;
begin

  inherited;
end;

procedure TShowCliForm.SetInfo(aConnIndex: U16; const aPeer: String);
begin
  fConnIndex:= aConnIndex;
  fPeer:= aPeer;

  StatusLabel.Caption :=
    Format('ConnIndex: %d, UserID: %s, Peer: %s',
      [fConnIndex, fUserID, fPeer]
    );
end;

procedure TShowCliForm.SetUser(const aUserID: String);
begin
  fUserID := aUserID;
  
  StatusLabel.Caption :=
    Format('ConnIndex: %d, UserID: %s, Peer: %s',
      [fConnIndex, fUserID, fPeer]
    );
end;

procedure TShowCliForm.NotiSend_ButtonClick(Sender: TObject);
var
  NotiMsg: AnsiString;
  NotiLen: U8;
  SenderLen: U8;

  ChatType: TChatType;
begin
  NotiMsg := NotiText_Edit.Text;
  NotiLen := U8(Length(NotiMsg));

  SenderLen := 0;

  ChatType := ctNotify;

  ToCli_SendPacketWArr(fConnIndex, REQ_CHAT, 4,
  // ChatType           Sender(0)               Msg
    [SizeOf(TChatType), SizeOf(U8), SizeOf(U8), NotiLen],
    [@ChatType, @SenderLen, @NotiLen, PAnsiChar(NotiMsg)]
  );

end;

end.
