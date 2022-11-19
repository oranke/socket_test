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
  object Button1: TButton
    Left = 32
    Top = 32
    Width = 137
    Height = 33
    Caption = #49436#48260' '#49884#51089' / '#51333#47308
    TabOrder = 0
    OnClick = Button1Click
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
