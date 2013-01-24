object FormConfirmaFechar: TFormConfirmaFechar
  Left = 356
  Top = 274
  Caption = 'Confirma fechamento'
  ClientHeight = 208
  ClientWidth = 324
  Color = 16384
  Constraints.MinHeight = 246
  Constraints.MinWidth = 340
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    324
    208)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 308
    Height = 71
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Use o m'#233'todo OnCloseQuery da classe TForm para determinar se uma' +
      ' aba pode ou n'#227'o ser fechada. Veja uma exemplo no c'#243'digo fonte d' +
      'este formul'#225'rio.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 85
    Width = 332
    Height = 41
    Anchors = [akLeft, akRight]
    BevelInner = bvSpace
    BevelOuter = bvLowered
    Caption = 'Cofnirma fechamento'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -24
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 0
  end
end
