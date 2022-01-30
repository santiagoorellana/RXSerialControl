object FormReceptorICR8500Configuracion: TFormReceptorICR8500Configuracion
  Left = 548
  Top = 138
  Width = 451
  Height = 206
  Caption = 'ICOM IC-R8500 Configuracion'
  Color = clGradientInactiveCaption
  Constraints.MinHeight = 206
  Constraints.MinWidth = 451
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    443
    175)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 3
    Top = 5
    Width = 425
    Height = 120
    Anchors = [akLeft, akTop, akRight, akBottom]
    Constraints.MinHeight = 120
    Constraints.MinWidth = 425
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 207
      Top = 1
      Width = 5
      Height = 118
      Cursor = crHSplit
      Beveled = True
      Color = clBtnFace
      ParentColor = False
    end
    object PanelA: TPanel
      Left = 1
      Top = 1
      Width = 206
      Height = 118
      Align = alLeft
      TabOrder = 0
      DesignSize = (
        206
        118)
      object Label10: TLabel
        Left = 27
        Top = 11
        Width = 63
        Height = 13
        Caption = 'Demodulador'
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
        Left = 174
        Top = 69
        Width = 18
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '000'
      end
      object LabelNivelDeAudio: TLabel
        Left = 174
        Top = 94
        Width = 18
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '000'
      end
      object ComboBoxDemodulador: TComboBox
        Left = 97
        Top = 7
        Width = 103
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
          'AM Estrecho'
          'AM Ancho'
          'CW'
          'FM'
          'FM Estrecho')
      end
      object TrackBarSquelch: TTrackBar
        Left = 95
        Top = 64
        Width = 75
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
        TabOrder = 1
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = TrackBarSquelchChange
      end
      object TrackBarNivelDelAudio: TTrackBar
        Left = 95
        Top = 89
        Width = 75
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
        OnChange = TrackBarNivelDelAudioChange
      end
      object CheckBoxAGC: TCheckBox
        Left = 66
        Top = 39
        Width = 44
        Height = 17
        Alignment = taLeftJustify
        Caption = 'AGC'
        TabOrder = 3
        OnClick = CheckBoxAGCClick
      end
    end
    object PanelB: TPanel
      Left = 212
      Top = 1
      Width = 212
      Height = 118
      Align = alClient
      TabOrder = 1
      DesignSize = (
        212
        118)
      object LabelAPF: TLabel
        Left = 180
        Top = 64
        Width = 18
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '000'
      end
      object LabelFI: TLabel
        Left = 180
        Top = 39
        Width = 18
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '000'
      end
      object Label18: TLabel
        Left = 56
        Top = 40
        Width = 9
        Height = 13
        Caption = 'FI'
      end
      object Label3: TLabel
        Left = 19
        Top = 12
        Width = 49
        Height = 13
        Caption = 'Atenuador'
      end
      object TrackBarFI: TTrackBar
        Left = 69
        Top = 34
        Width = 106
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
        TabOrder = 0
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = TrackBarFIChange
      end
      object TrackBarAPF: TTrackBar
        Left = 93
        Top = 59
        Width = 82
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
        TabOrder = 1
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = TrackBarAPFChange
      end
      object ComboBoxAtenuador: TComboBox
        Left = 76
        Top = 8
        Width = 129
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 1
        TabOrder = 2
        Text = '10 dB'
        OnChange = ComboBoxAtenuadorChange
        Items.Strings = (
          'Ninguno'
          '10 dB'
          '20 dB'
          '30 dB')
      end
      object CheckBoxAPF: TCheckBox
        Left = 44
        Top = 63
        Width = 44
        Height = 17
        Alignment = taLeftJustify
        Caption = 'APF'
        TabOrder = 3
        OnClick = CheckBoxAPFClick
      end
      object CheckBoxNoiseBlanker: TCheckBox
        Left = 6
        Top = 88
        Width = 82
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Limpiar (NB)'
        TabOrder = 4
        OnClick = CheckBoxNoiseBlankerClick
      end
    end
  end
  object BitBtn3: TBitBtn
    Left = 5
    Top = 133
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
    Left = 112
    Top = 133
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
    Left = 219
    Top = 133
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
    Left = 326
    Top = 133
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
