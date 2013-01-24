object FormGuiasAbertas: TFormGuiasAbertas
  Left = 375
  Top = 197
  Caption = 'Formul'#225'rio abertos'
  ClientHeight = 406
  ClientWidth = 515
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 515
    Height = 406
    HorzScrollBar.Increment = 46
    HorzScrollBar.Tracking = True
    VertScrollBar.Increment = 36
    VertScrollBar.Tracking = True
    Align = alClient
    TabOrder = 0
  end
  object Timer1: TTimer
    Interval = 150
    OnTimer = Timer1Timer
    Left = 120
    Top = 96
  end
end
