object MainForm: TMainForm
  Left = 626
  Top = 142
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #53364#46972#51060#50616#53944
  ClientHeight = 386
  ClientWidth = 522
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 33
    Height = 16
    Caption = #50500#51060#46356
  end
  object Connect_Button: TButton
    Left = 16
    Top = 51
    Width = 97
    Height = 25
    Caption = #53364#46972' '#51217#49549'/'#51333#47308
    TabOrder = 0
    OnClick = Connect_ButtonClick
  end
  object SvrIP_Edit: TEdit
    Left = 129
    Top = 51
    Width = 72
    Height = 24
    TabOrder = 1
    Text = '127.0.0.1'
  end
  object SvrPort_Edit: TEdit
    Left = 207
    Top = 51
    Width = 72
    Height = 24
    TabOrder = 2
    Text = '10102'
  end
  object ID_Edit: TEdit
    Left = 128
    Top = 20
    Width = 153
    Height = 24
    TabOrder = 3
    Text = 'user_'
  end
  object History_Memo: TMemo
    Left = 16
    Top = 88
    Width = 489
    Height = 233
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object ChatText_Edit: TEdit
    Left = 16
    Top = 336
    Width = 409
    Height = 24
    TabOrder = 5
  end
  object ChatSend_Button: TButton
    Left = 432
    Top = 336
    Width = 75
    Height = 25
    Caption = #51204#49569
    TabOrder = 6
    OnClick = ChatSend_ButtonClick
  end
  object TickTimer: TTimer
    Interval = 1
    OnTimer = TickTimerTimer
    Left = 344
    Top = 24
  end
  object XPManifest1: TXPManifest
    Left = 400
    Top = 24
  end
end
