object MainForm: TMainForm
  Left = 206
  Top = 135
  Width = 478
  Height = 430
  Caption = #49436#48260' '#53580#49828#53944
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
    Left = 32
    Top = 104
    Width = 75
    Height = 16
    Caption = #53364#46972' '#52964#45349#49496' ID'
  end
  object Start_Button: TButton
    Left = 32
    Top = 32
    Width = 137
    Height = 33
    Caption = #49436#48260' '#49884#51089' / '#51333#47308
    TabOrder = 0
    OnClick = Start_ButtonClick
  end
  object CliConnID_Edit: TEdit
    Left = 32
    Top = 128
    Width = 121
    Height = 24
    TabOrder = 1
  end
  object Packet01_Button: TButton
    Left = 32
    Top = 160
    Width = 137
    Height = 33
    Caption = #53364#46972#50640' '#54056#53431
    TabOrder = 2
    OnClick = Packet01_ButtonClick
  end
  object Packet02_Button: TButton
    Left = 32
    Top = 200
    Width = 137
    Height = 33
    Caption = #53364#46972#50640' '#54056#53431' 2'
    TabOrder = 3
    OnClick = Packet02_ButtonClick
  end
  object AccessDenine_Button: TButton
    Left = 32
    Top = 240
    Width = 137
    Height = 33
    Caption = #53364#46972#50640' AccessDenine'
    TabOrder = 4
    OnClick = AccessDenine_ButtonClick
  end
  object TickTimer: TTimer
    Interval = 1
    OnTimer = TickTimerTimer
    Left = 32
    Top = 328
  end
  object XPManifest1: TXPManifest
    Left = 120
    Top = 328
  end
end
