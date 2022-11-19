object MainForm: TMainForm
  Left = 206
  Top = 135
  Width = 409
  Height = 327
  Caption = #53364#46972#51060#50616#53944
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 16
  object Connect_Button: TButton
    Left = 32
    Top = 59
    Width = 97
    Height = 25
    Caption = #53364#46972' '#51217#49549'/'#51333#47308
    TabOrder = 0
    OnClick = Connect_ButtonClick
  end
  object RecreateCli_Button: TButton
    Left = 32
    Top = 25
    Width = 97
    Height = 25
    Caption = #53364#46972' '#51116#49373#49457
    TabOrder = 1
    OnClick = RecreateCli_ButtonClick
  end
  object ConnKey_Edit: TEdit
    Left = 145
    Top = 27
    Width = 150
    Height = 24
    TabOrder = 2
    Text = '$3AA04B0F'
  end
  object SvrIP_Edit: TEdit
    Left = 145
    Top = 61
    Width = 72
    Height = 24
    TabOrder = 3
    Text = '127.0.0.1'
  end
  object SvrPort_Edit: TEdit
    Left = 223
    Top = 61
    Width = 72
    Height = 24
    TabOrder = 4
    Text = '10101'
  end
  object Packet1_Button: TButton
    Left = 32
    Top = 104
    Width = 97
    Height = 25
    Caption = #49436#48260#50640' '#54056#53431
    TabOrder = 5
    OnClick = Packet1_ButtonClick
  end
  object Packet2_Button: TButton
    Left = 32
    Top = 135
    Width = 97
    Height = 25
    Caption = #49436#48260#50640' '#54056#53431' 2'
    TabOrder = 6
    OnClick = Packet2_ButtonClick
  end
  object Packet3_Button: TButton
    Left = 32
    Top = 166
    Width = 97
    Height = 25
    Caption = #49436#48260#50640' '#54056#53431' 3'
    TabOrder = 7
    OnClick = Packet3_ButtonClick
  end
  object Packet4_Button: TButton
    Left = 32
    Top = 197
    Width = 97
    Height = 25
    Caption = #49436#48260#50640' '#54056#53431' 4'
    TabOrder = 8
    OnClick = Packet4_ButtonClick
  end
  object TickTimer: TTimer
    Interval = 1
    OnTimer = TickTimerTimer
    Left = 160
    Top = 104
  end
  object XPManifest1: TXPManifest
    Left = 272
    Top = 112
  end
end
