object FormConfiguracion: TFormConfiguracion
  Left = 387
  Top = 112
  BorderStyle = bsDialog
  Caption = 'Configuraci'#243'n del puerto'
  ClientHeight = 215
  ClientWidth = 189
  Color = clBtnFace
  Constraints.MaxHeight = 246
  Constraints.MaxWidth = 197
  Constraints.MinHeight = 242
  Constraints.MinWidth = 197
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    189
    215)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 3
    Top = 2
    Width = 184
    Height = 179
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Par'#225'metros'
    TabOrder = 0
    DesignSize = (
      184
      179)
    object Label1: TLabel
      Left = 50
      Top = 21
      Width = 34
      Height = 13
      Caption = 'Puerto:'
    end
    object Label2: TLabel
      Left = 34
      Top = 47
      Width = 50
      Height = 13
      Caption = 'Velocidad:'
    end
    object Label3: TLabel
      Left = 5
      Top = 73
      Width = 79
      Height = 13
      Caption = 'Cantidad de bits:'
    end
    object Label4: TLabel
      Left = 13
      Top = 99
      Width = 71
      Height = 13
      Caption = 'Bits de parada:'
    end
    object Label5: TLabel
      Left = 45
      Top = 125
      Width = 39
      Height = 13
      Caption = 'Paridad:'
    end
    object Label6: TLabel
      Left = 11
      Top = 152
      Width = 73
      Height = 13
      Caption = 'Control de flujo:'
    end
    object ComboBoxPuerto: TComboBox
      Left = 88
      Top = 17
      Width = 91
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
    end
    object ComboBoxVelocidad: TComboBox
      Left = 88
      Top = 43
      Width = 91
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
    end
    object ComboBoxCantidadDeBits: TComboBox
      Left = 88
      Top = 69
      Width = 91
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 2
    end
    object ComboBoxBitsDeParada: TComboBox
      Left = 88
      Top = 95
      Width = 91
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 3
    end
    object ComboBoxParidad: TComboBox
      Left = 88
      Top = 121
      Width = 91
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 4
    end
    object ComboBoxControlDeFlujo: TComboBox
      Left = 88
      Top = 148
      Width = 91
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 5
    end
  end
  object BitBtn1: TBitBtn
    Left = 4
    Top = 186
    Width = 90
    Height = 25
    Action = ActionCancelar
    Anchors = [akTop, akRight]
    Caption = 'Cancelar'
    TabOrder = 1
    TabStop = False
    Glyph.Data = {
      E6000000424DE6000000000000007600000028000000100000000E0000000100
      0400000000007000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00F1111FFFFFFF
      FFFFFF1111FFFFFFFFF1FFF1111FFFFFF11FFFFF1111FFF111FFFFFFF1111111
      FFFFFFFFFF11111FFFFFFFFFFF1111FFFFFFFFFF1111111FFFFFFF11111FF111
      FFFF11111FFFFF111FFF1111FFFFFFF11FFF111FFFFFFFFF11FF1FFFFFFFFFFF
      F11FFFFFFFFFFFFFFF11}
  end
  object BitBtn2: TBitBtn
    Left = 97
    Top = 186
    Width = 90
    Height = 25
    Action = ActionAceptar
    Anchors = [akTop, akRight]
    Caption = 'Aceptar'
    TabOrder = 2
    TabStop = False
    Glyph.Data = {
      E6000000424DE6000000000000007600000028000000100000000E0000000100
      0400000000007000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFF22FFFFFFFFFFFFF2222FFFFFFFFFFF222222FFFFFFFFF2222222FFF
      FFFFFF222F2222FFFFFFF222FFF2222FFFFFF22FFFFF2222FFFF22FFFFFFF222
      FFFF2FFFFFFFFF222FFFFFFFFFFFFFF22FFFFFFFFFFFFFFF22FFFFFFFFFFFFFF
      F22FFFFFFFFFFFFFFF22}
  end
  object ActionList1: TActionList
    Left = 8
    Top = 24
    object ActionAceptar: TAction
      Caption = 'Aceptar'
      Hint = 'Aceptar'
      ShortCut = 13
      OnExecute = ActionAceptarExecute
    end
    object ActionCancelar: TAction
      Caption = 'Cancelar'
      Hint = 'Cancelar'
      ShortCut = 27
      OnExecute = ActionCancelarExecute
    end
  end
end
