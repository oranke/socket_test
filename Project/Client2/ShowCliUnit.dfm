object ShowCliForm: TShowCliForm
  Left = 296
  Top = 288
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #53364#46972' '#49345#53468#52285
  ClientHeight = 419
  ClientWidth = 582
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 16
  object StatusLabel: TLabel
    Left = 16
    Top = 16
    Width = 66
    Height = 16
    Caption = 'StatusLabel'
  end
  object History_Memo: TMemo
    Left = 16
    Top = 48
    Width = 545
    Height = 321
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object NotiText_Edit: TEdit
    Left = 16
    Top = 384
    Width = 409
    Height = 24
    TabOrder = 1
  end
  object NotiSend_Button: TButton
    Left = 432
    Top = 384
    Width = 129
    Height = 25
    Caption = #44060#48324' '#44277#51648' '#51204#49569
    TabOrder = 2
    OnClick = NotiSend_ButtonClick
  end
end
