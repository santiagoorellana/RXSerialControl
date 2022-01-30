object FormReceptorAR5000AConfiguracion: TFormReceptorAR5000AConfiguracion
  Left = 671
  Top = 115
  Width = 453
  Height = 233
  Caption = 'AOR AR5000A Configuraci'#243'n'
  Color = clGradientInactiveCaption
  Constraints.MinHeight = 233
  Constraints.MinWidth = 453
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    445
    202)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 3
    Top = 5
    Width = 425
    Height = 145
    Anchors = [akLeft, akTop, akRight, akBottom]
    Constraints.MinHeight = 145
    Constraints.MinWidth = 425
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 220
      Top = 1
      Width = 5
      Height = 143
      Cursor = crHSplit
      Beveled = True
      Color = clBtnFace
      ParentColor = False
    end
    object PanelA: TPanel
      Left = 1
      Top = 1
      Width = 219
      Height = 143
      Align = alLeft
      TabOrder = 0
      DesignSize = (
        219
        143)
      object Label2: TLabel
        Left = 27
        Top = 11
        Width = 63
        Height = 13
        Caption = 'Demodulador'
      end
      object Label3: TLabel
        Left = 11
        Top = 38
        Width = 79
        Height = 13
        Caption = 'Ancho de banda'
      end
      object Label1: TLabel
        Left = 67
        Top = 66
        Width = 22
        Height = 13
        Caption = 'AGC'
      end
      object Label8: TLabel
        Left = 50
        Top = 92
        Width = 39
        Height = 13
        Caption = 'Squelch'
      end
      object Label6: TLabel
        Left = 20
        Top = 118
        Width = 70
        Height = 13
        Caption = 'Nivel del audio'
      end
      object LabelSquelch: TLabel
        Left = 182
        Top = 92
        Width = 18
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '000'
      end
      object LabelNivelDelAudio: TLabel
        Left = 182
        Top = 118
        Width = 18
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '000'
      end
      object ComboBoxDemodulador: TComboBox
        Left = 99
        Top = 7
        Width = 113
        Height = 21
        Hint = 'Tipo de demodulador.'
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Text = 'AM'
        OnChange = ComboBoxDemoduladorChange
        Items.Strings = (
          'FM'
          'AM'
          'LSB'
          'USB'
          'CW')
      end
      object ComboBoxAnchoDeBanda: TComboBox
        Left = 99
        Top = 34
        Width = 113
        Height = 21
        Hint = 'Ancho de banda.'
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Text = '3.0 Khz'
        OnChange = ComboBoxAnchoDeBandaChange
        Items.Strings = (
          '0.5 Khz'
          '3.0 Khz'
          '6.0 Khz'
          '15 Khz'
          '30 Khz'
          '110 Khz'
          '220 Khz')
      end
      object ComboBoxAGC: TComboBox
        Left = 99
        Top = 61
        Width = 113
        Height = 21
        Hint = 'Control Autom'#225'tico de Ganancia.'
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        Text = 'Medio'
        OnChange = ComboBoxAGCChange
        Items.Strings = (
          'R'#225'pido'
          'Medio'
          'Lento'
          'Desactivado')
      end
      object TrackBarNivelDelAudio: TTrackBar
        Left = 92
        Top = 113
        Width = 86
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
      object TrackBarSquelch: TTrackBar
        Left = 92
        Top = 87
        Width = 86
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
        OnChange = TrackBarSquelchChange
      end
    end
    object PanelB: TPanel
      Left = 225
      Top = 1
      Width = 199
      Height = 143
      Align = alClient
      TabOrder = 1
      DesignSize = (
        199
        143)
      object Label5: TLabel
        Left = 50
        Top = 12
        Width = 34
        Height = 13
        Caption = 'Antena'
      end
      object Label7: TLabel
        Left = 28
        Top = 118
        Width = 56
        Height = 13
        Caption = 'Salida de FI'
      end
      object Label10: TLabel
        Left = 7
        Top = 94
        Width = 78
        Height = 13
        Caption = 'Filtro Pasa Bajos'
      end
      object Label9: TLabel
        Left = 10
        Top = 66
        Width = 75
        Height = 13
        Caption = 'Filtro Pasa Altos'
      end
      object Label4: TLabel
        Left = 35
        Top = 38
        Width = 49
        Height = 13
        Caption = 'Atenuador'
      end
      object ComboBoxAntena: TComboBox
        Left = 93
        Top = 7
        Width = 102
        Height = 21
        Hint = 'Para amplificar la RF que entra por la antena.'
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Text = 'Conector #1'
        OnChange = ComboBoxAntenaChange
        Items.Strings = (
          'Autom'#225'tico'
          'Conector #1'
          'Conector #2')
      end
      object ComboBoxAtenuador: TComboBox
        Left = 93
        Top = 34
        Width = 102
        Height = 21
        Hint = 'Para atenuar la entrada de RF.'
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Text = '10 dB'
        OnChange = ComboBoxAtenuadorChange
        Items.Strings = (
          '0 dB'
          '10 dB'
          '20 dB'
          'Autom'#225'tico')
      end
      object ComboBoxFiltroPasaAltos: TComboBox
        Left = 93
        Top = 61
        Width = 102
        Height = 21
        Hint = 'Filtro Pasa Altos para el audio de salida.'
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        Text = '200 Hz'
        OnClick = ComboBoxFiltroPasaAltosChange
        Items.Strings = (
          '50  Hz'
          '200 Hz'
          '300 Hz'
          '400 Hz')
      end
      object ComboBoxFiltroPasaBajos: TComboBox
        Left = 93
        Top = 87
        Width = 102
        Height = 21
        Hint = 'Filtro Pasa Bajos para el audio de salida.'
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        Text = '4 KHz'
        OnClick = ComboBoxFiltroPasaBajosChange
        Items.Strings = (
          '3 KHz'
          '4 KHz'
          '6 KHz'
          '12 KHz')
      end
      object ComboBoxSalidaDeFI: TComboBox
        Left = 93
        Top = 114
        Width = 102
        Height = 21
        Hint = 'Para amplificar la RF que entra por la antena.'
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        Text = 'Conector #1'
        OnChange = ComboBoxSalidaDeFIChange
        Items.Strings = (
          'Desactivado'
          'Conector #1'
          'Conector #2')
      end
    end
  end
  object BitBtn3: TBitBtn
    Left = 4
    Top = 159
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
    Left = 111
    Top = 159
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
    Left = 218
    Top = 159
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
    Left = 325
    Top = 159
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
    Top = 67
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
