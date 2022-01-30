object FormReceptorICR75Configuracion: TFormReceptorICR75Configuracion
  Left = 251
  Top = 109
  Width = 460
  Height = 278
  Caption = 'ICOM IC-R75 Configuracion'
  Color = clGradientInactiveCaption
  Constraints.MinHeight = 278
  Constraints.MinWidth = 460
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    452
    247)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 3
    Top = 5
    Width = 430
    Height = 192
    Anchors = [akLeft, akTop, akRight, akBottom]
    Constraints.MinHeight = 192
    Constraints.MinWidth = 430
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 190
      Top = 1
      Width = 5
      Height = 190
      Cursor = crHSplit
      Beveled = True
      Color = clBtnFace
      ParentColor = False
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 189
      Height = 190
      Align = alLeft
      TabOrder = 0
      DesignSize = (
        189
        190)
      object Label10: TLabel
        Left = 27
        Top = 11
        Width = 63
        Height = 13
        Caption = 'Demodulador'
      end
      object Label14: TLabel
        Left = 69
        Top = 41
        Width = 22
        Height = 13
        Caption = 'AGC'
      end
      object Label6: TLabel
        Left = 21
        Top = 94
        Width = 70
        Height = 13
        Caption = 'Nivel del audio'
      end
      object Label1: TLabel
        Left = 8
        Top = 69
        Width = 83
        Height = 13
        Caption = 'Nivel del Squelch'
      end
      object LabelSquelch: TLabel
        Left = 157
        Top = 69
        Width = 18
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '000'
      end
      object LabelNivelDelAudio: TLabel
        Left = 157
        Top = 94
        Width = 18
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '000'
      end
      object ComboBoxDemodulador: TComboBox
        Left = 97
        Top = 7
        Width = 86
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
        Text = 'LSB'
        OnChange = ComboBoxDemoduladorChange
        Items.Strings = (
          'LSB'
          'USB'
          'AM'
          'AM Ancho'
          'CW'
          'CW Ancho'
          'FM'
          'FM Ancho')
      end
      object ComboBoxAGC: TComboBox
        Left = 97
        Top = 37
        Width = 86
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 1
        TabOrder = 1
        Text = 'R'#225'pido'
        OnChange = ComboBoxAGCChange
        Items.Strings = (
          'Ninguno'
          'R'#225'pido'
          'Medio'
          'Lento')
      end
      object TrackBarSquelch: TTrackBar
        Left = 92
        Top = 64
        Width = 61
        Height = 24
        Hint = 'Vl'#250'men de la bocina del receptor.'
        Anchors = [akLeft, akTop, akRight]
        Max = 255
        Orientation = trHorizontal
        ParentShowHint = False
        Frequency = 1
        Position = 10
        SelEnd = 0
        SelStart = 0
        ShowHint = True
        TabOrder = 2
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = TrackBarSquelchChange
      end
      object TrackBarNivelDelAudio: TTrackBar
        Left = 92
        Top = 89
        Width = 61
        Height = 24
        Hint = 'Vl'#250'men de la bocina del receptor.'
        Anchors = [akLeft, akTop, akRight]
        Max = 255
        Orientation = trHorizontal
        ParentShowHint = False
        Frequency = 1
        Position = 10
        SelEnd = 0
        SelStart = 0
        ShowHint = True
        TabOrder = 3
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = TrackBarNivelDelAudioChange
      end
    end
    object Panel3: TPanel
      Left = 195
      Top = 1
      Width = 234
      Height = 190
      Align = alClient
      TabOrder = 1
      DesignSize = (
        234
        190)
      object LabelIluminacionLCD: TLabel
        Left = 202
        Top = 166
        Width = 18
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '000'
      end
      object LabelTwinPbtExterior: TLabel
        Left = 202
        Top = 141
        Width = 18
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '000'
      end
      object LabelTwinPbtInterior: TLabel
        Left = 202
        Top = 116
        Width = 18
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '000'
      end
      object LabelNivelDeRF: TLabel
        Left = 202
        Top = 91
        Width = 18
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '000'
      end
      object Label8: TLabel
        Left = 18
        Top = 166
        Width = 94
        Height = 13
        Caption = 'Iluminaci'#243'n del LCD'
      end
      object Label2: TLabel
        Left = 20
        Top = 141
        Width = 91
        Height = 13
        Caption = 'TWIN PBT Exterior'
      end
      object Label18: TLabel
        Left = 23
        Top = 116
        Width = 88
        Height = 13
        Caption = 'TWIN PBT Interior'
      end
      object Label5: TLabel
        Left = 53
        Top = 91
        Width = 56
        Height = 13
        Caption = 'Nivel de RF'
      end
      object Label13: TLabel
        Left = 8
        Top = 64
        Width = 104
        Height = 13
        Caption = 'Preamplificador de RF'
      end
      object Label12: TLabel
        Left = 76
        Top = 11
        Width = 34
        Height = 13
        Caption = 'Antena'
      end
      object ComboBoxAntena: TComboBox
        Left = 118
        Top = 7
        Width = 110
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 0
        Text = 'Conector 50 Ohm'
        OnChange = ComboBoxAntenaChange
        Items.Strings = (
          'Conector 50 Ohm'
          'Conector 500 Ohm')
      end
      object CheckBoxAtenuador: TCheckBox
        Left = 61
        Top = 32
        Width = 70
        Height = 25
        Alignment = taLeftJustify
        Caption = 'Atenuador'
        TabOrder = 1
        OnClick = CheckBoxAtenuadorClick
      end
      object ComboBoxPreamplificador: TComboBox
        Left = 118
        Top = 59
        Width = 110
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 2
        Text = 'Ninguno'
        OnChange = ComboBoxPreamplificadorChange
        Items.Strings = (
          'Ninguno'
          'Preamplificador 1'
          'Preamplificador 2')
      end
      object TrackBarNivelDeRF: TTrackBar
        Left = 111
        Top = 86
        Width = 85
        Height = 24
        Hint = 'Vl'#250'men de la bocina del receptor.'
        Anchors = [akLeft, akTop, akRight]
        Max = 255
        Orientation = trHorizontal
        ParentShowHint = False
        Frequency = 1
        Position = 10
        SelEnd = 0
        SelStart = 0
        ShowHint = True
        TabOrder = 3
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = TrackBarNivelDeRFChange
      end
      object TrackBarTwinPbtInterior: TTrackBar
        Left = 111
        Top = 111
        Width = 85
        Height = 24
        Hint = 'Vl'#250'men de la bocina del receptor.'
        Anchors = [akLeft, akTop, akRight]
        Max = 255
        Orientation = trHorizontal
        ParentShowHint = False
        Frequency = 1
        Position = 10
        SelEnd = 0
        SelStart = 0
        ShowHint = True
        TabOrder = 4
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = TrackBarTwinPbtInteriorChange
      end
      object TrackBarTwinPbtExterior: TTrackBar
        Left = 111
        Top = 136
        Width = 85
        Height = 24
        Hint = 'Vl'#250'men de la bocina del receptor.'
        Anchors = [akLeft, akTop, akRight]
        Max = 255
        Orientation = trHorizontal
        ParentShowHint = False
        Frequency = 1
        Position = 10
        SelEnd = 0
        SelStart = 0
        ShowHint = True
        TabOrder = 5
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = TrackBarTwinPbtExteriorChange
      end
      object TrackBarIluminacionLCD: TTrackBar
        Left = 111
        Top = 161
        Width = 85
        Height = 24
        Hint = 'Vl'#250'men de la bocina del receptor.'
        Anchors = [akLeft, akTop, akRight]
        Max = 255
        Orientation = trHorizontal
        ParentShowHint = False
        Frequency = 1
        Position = 10
        SelEnd = 0
        SelStart = 0
        ShowHint = True
        TabOrder = 6
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = TrackBarIluminacionLCDChange
      end
    end
  end
  object BitBtn3: TBitBtn
    Left = 11
    Top = 205
    Width = 100
    Height = 26
    Action = ActionAyuda
    Anchors = [akRight, akBottom]
    Caption = 'Ayuda'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    Glyph.Data = {
      E6000000424DE6000000000000007600000028000000100000000E0000000100
      0400000000007000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFF333FF
      FFFFFFFFFFF333FFFFFFFFFFFFFFFFFFFFFFFFFFFFF333FFFFFFFFFFFFF333FF
      FFFFFFFFFFF33333FFFFFFFFFFF333333FFFFFFFFFFFFFF333FFFFFFFFFFFFFF
      333FFFF333FFFFFF333FFFF333FFFFFF333FFFFF333FFFF333FFFFFFF3333333
      3FFFFFFFFF333333FFFF}
  end
  object BitBtn4: TBitBtn
    Left = 118
    Top = 205
    Width = 100
    Height = 26
    Action = ActionRestablecer
    Anchors = [akRight, akBottom]
    Caption = 'Restablecer'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Glyph.Data = {
      E6000000424DE6000000000000007600000028000000100000000E0000000100
      0400000000007000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFFF1111FFFFFFFFFFF11FF11FFFFFFFFF11FFFF11F1FFFFFFFFFFFFF1
      11FFFFFFFFFFFFF111FFFFFFFFFFFF1111FFFF1111FFFFFFFFFFFF111FFFFFFF
      FFFFFF111FFFFFFFFFFFFF1F11FFFF11FFFFFFFFF11FF11FFFFFFFFFFF1111FF
      FFFFFFFFFFFFFFFFFFFF}
  end
  object BitBtn2: TBitBtn
    Left = 225
    Top = 205
    Width = 100
    Height = 26
    Action = ActionInformacion
    Anchors = [akRight, akBottom]
    Caption = 'Informaci'#243'n'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    Glyph.Data = {
      E6000000424DE6000000000000007600000028000000100000000E0000000100
      0400000000007000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFF111111F
      FFFFFFF11BBBBBB11FFFFF1BBB1111BBB1FFF1BBBB1111BBBB1FF1BBBB1111BB
      BB1F1BBBBB1111BBBBB11BBBBB1111BBBBB11BBBBB1111BBBBB11BBBBBBBBBBB
      BBB1F1BBBBB11BBBBB1FF1BBBB1111BBBB1FFF1BBBB11BBBB1FFFFF11BBBBBB1
      1FFFFFFFF111111FFFFF}
  end
  object BitBtn1: TBitBtn
    Left = 332
    Top = 205
    Width = 100
    Height = 26
    Action = ActionAceptar
    Anchors = [akRight, akBottom]
    Caption = 'Aceptar'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
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
    Left = 14
    Top = 35
    object ActionCerrar: TAction
      Caption = 'Cerrar'
      Hint = 'Cerrar di'#225'logo de configuraci'#243'n.'
      ShortCut = 27
      OnExecute = ActionCerrarExecute
    end
    object ActionAceptar: TAction
      Caption = 'Aceptar'
      ShortCut = 13
      OnExecute = ActionAceptarExecute
    end
    object ActionInformacion: TAction
      Caption = 'Informaci'#243'n'
      Hint = 'Informaci'#243'n'
      ShortCut = 16457
      OnExecute = ActionInformacionExecute
    end
    object ActionRestablecer: TAction
      Caption = 'Restablecer'
      ShortCut = 16466
      OnExecute = ActionRestablecerExecute
    end
    object ActionAyuda: TAction
      Caption = 'Ayuda'
      Hint = 'Ayuda'
      ShortCut = 112
      OnExecute = ActionAyudaExecute
    end
  end
end
