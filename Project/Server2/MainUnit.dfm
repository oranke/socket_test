object MainForm: TMainForm
  Left = 206
  Top = 135
  Width = 495
  Height = 459
  BorderIcons = [biSystemMenu]
  Caption = #49436#48260' '#53580#49828#53944' - '#52292#54021#49436#48260
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 16
  object Label2: TLabel
    Left = 16
    Top = 88
    Width = 48
    Height = 16
    Caption = #51217#49549' '#47785#47197
  end
  object Start_Button: TButton
    Left = 16
    Top = 24
    Width = 137
    Height = 33
    Caption = #49436#48260' '#49884#51089' / '#51333#47308
    TabOrder = 0
    OnClick = Start_ButtonClick
  end
  object ConnListBox: TListBox
    Left = 16
    Top = 112
    Width = 449
    Height = 249
    Style = lbVirtualOwnerDraw
    ItemHeight = 25
    TabOrder = 1
    OnDblClick = ConnListBoxDblClick
    OnDrawItem = ConnListBoxDrawItem
  end
  object NotiText_Edit: TEdit
    Left = 16
    Top = 384
    Width = 313
    Height = 24
    TabOrder = 2
  end
  object NotiSend_Button: TButton
    Left = 339
    Top = 384
    Width = 129
    Height = 25
    Caption = #51204#52404' '#44277#51648' '#51204#49569
    TabOrder = 3
    OnClick = NotiSend_ButtonClick
  end
  object TickTimer: TTimer
    Interval = 1
    OnTimer = TickTimerTimer
    Left = 232
    Top = 16
  end
  object XPManifest1: TXPManifest
    Left = 288
    Top = 16
  end
end
