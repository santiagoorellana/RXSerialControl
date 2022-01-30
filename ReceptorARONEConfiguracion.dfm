object FormReceptorARONEConfiguracion: TFormReceptorARONEConfiguracion
  Left = 547
  Top = 145
  Width = 503
  Height = 317
  Caption = 'AOR AR-ONE Configuraci'#243'n'
  Color = clGradientInactiveCaption
  Constraints.MinHeight = 317
  Constraints.MinWidth = 503
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    495
    286)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 3
    Top = 5
    Width = 477
    Height = 230
    Anchors = [akLeft, akTop, akRight, akBottom]
    Constraints.MinHeight = 230
    Constraints.MinWidth = 470
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 211
      Top = 1
      Width = 5
      Height = 228
      Cursor = crHSplit
      Beveled = True
      Color = clBtnFace
      ParentColor = False
    end
    object PanelA: TPanel
      Left = 1
      Top = 1
      Width = 210
      Height = 228
      Align = alLeft
      Constraints.MinWidth = 210
      TabOrder = 0
      DesignSize = (
        210
        228)
      object Label2: TLabel
        Left = 24
        Top = 11
        Width = 63
        Height = 13
        Caption = 'Demodulador'
      end
      object Label3: TLabel
        Left = 8
        Top = 38
        Width = 79
        Height = 13
        Caption = 'Ancho de banda'
      end
      object Label1: TLabel
        Left = 64
        Top = 67
        Width = 22
        Height = 13
        Caption = 'AGC'
      end
      object Label11: TLabel
        Left = 47
        Top = 95
        Width = 39
        Height = 13
        Caption = 'Squelch'
      end
      object LabelNivelDelAudio: TLabel
        Left = 175
        Top = 122
        Width = 18
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '000'
      end
      object LabelSquelch: TLabel
        Left = 174
        Top = 94
        Width = 18
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '000'
      end
      object ComboBoxDemodulador: TComboBox
        Left = 91
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
          'CW'
          'USB'
          'LSB'
          'WFM'
          'NFM')
      end
      object ComboBoxAnchoDeBanda: TComboBox
        Left = 91
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
          '8.5 Khz'
          '16 Khz'
          '30 Khz'
          '100 Khz'
          '200 Khz'
          '300 Khz'
          '5 Mhz')
      end
      object ComboBoxAGC: TComboBox
        Left = 91
        Top = 62
        Width = 113
        Height = 21
        Hint = 'Control Autom'#225'tico de Ganancia.'
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        Text = 'R'#225'pido'
        OnChange = ComboBoxAGCChange
        Items.Strings = (
          'Desactivado'
          'R'#225'pido'
          'Lento'
          'Medio')
      end
      object TrackBarSquelch: TTrackBar
        Left = 89
        Top = 89
        Width = 80
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Max = 255
        Orientation = trHorizontal
        Frequency = 1
        Position = 10
        SelEnd = 0
        SelStart = 0
        TabOrder = 3
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = TrackBarSquelchChange
      end
      object CheckBoxBocinaActivada: TCheckBox
        Left = 17
        Top = 117
        Width = 91
        Height = 25
        Hint = 'Activa la bocina del receptor.'
        Alignment = taLeftJustify
        Caption = 'Activar bocina'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnClick = CheckBoxBocinaActivadaClick
      end
      object TrackBarNivelDelAudio: TTrackBar
        Left = 113
        Top = 118
        Width = 56
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
        OnChange = TrackBarNivelDelAudioChange
      end
    end
    object PanelB: TPanel
      Left = 216
      Top = 1
      Width = 260
      Height = 228
      Align = alClient
      Constraints.MinWidth = 260
      TabOrder = 1
      DesignSize = (
        260
        228)
      object Label4: TLabel
        Left = 67
        Top = 12
        Width = 49
        Height = 13
        Caption = 'Atenuador'
      end
      object Label8: TLabel
        Left = 42
        Top = 119
        Width = 73
        Height = 13
        Caption = 'Ganancia de FI'
      end
      object Label7: TLabel
        Left = 7
        Top = 94
        Width = 108
        Height = 13
        Caption = 'Ganancia de 10,7 Mhz'
      end
      object Label6: TLabel
        Left = 38
        Top = 68
        Width = 78
        Height = 13
        Caption = 'Ganancia de RF'
      end
      object Label5: TLabel
        Left = 28
        Top = 40
        Width = 89
        Height = 13
        Caption = 'Amplificador de RF'
      end
      object LabelGananciaDeFIValor: TLabel
        Left = 222
        Top = 118
        Width = 18
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '000'
      end
      object LabelGananciaDe10700KhzValor: TLabel
        Left = 222
        Top = 93
        Width = 18
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '000'
      end
      object LabelGananciaDeRFValor: TLabel
        Left = 222
        Top = 67
        Width = 18
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '000'
      end
      object LabelContrasteDelLCD: TLabel
        Left = 222
        Top = 201
        Width = 18
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '000'
      end
      object Label10: TLabel
        Left = 38
        Top = 176
        Width = 78
        Height = 13
        Caption = 'Filtro Pasa Bajos'
      end
      object Label9: TLabel
        Left = 40
        Top = 147
        Width = 75
        Height = 13
        Caption = 'Filtro Pasa Altos'
      end
      object ComboBoxAtenuador: TComboBox
        Left = 128
        Top = 7
        Width = 125
        Height = 21
        Hint = 'Para atenuar la entrada de RF.'
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Text = '10 dB'
        OnChange = ComboBoxAtenuadorChange
        Items.Strings = (
          '0 dB'
          '10 dB'
          '20 dB'
          'Autom'#225'tico')
      end
      object ComboBoxAmplificadorRF: TComboBox
        Left = 128
        Top = 36
        Width = 125
        Height = 21
        Hint = 'Para amplificar la RF que entra por la antena.'
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Text = 'Conectado'
        OnChange = ComboBoxAmplificadorRFChange
        Items.Strings = (
          'Desconectado'
          'Conectado'
          'Autom'#225'tico')
      end
      object TrackBarGananciaRF: TTrackBar
        Left = 121
        Top = 62
        Width = 96
        Height = 24
        Hint = 'Ganancia del Amplificador de RF.'
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
        OnChange = TrackBarGananciaRFChange
      end
      object TrackBarGanancia10700Khz: TTrackBar
        Left = 121
        Top = 88
        Width = 96
        Height = 24
        Hint = 'Ganancia del Amplificador de FI de 10.7 Mhz.'
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
        OnChange = TrackBarGanancia10700KhzChange
      end
      object TrackBarGananciaFI: TTrackBar
        Left = 121
        Top = 113
        Width = 96
        Height = 24
        Hint = 'Ganancia del Amplificador de FI.'
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
        OnChange = TrackBarGananciaFIChange
      end
      object ComboBoxFiltroPasaAltos: TComboBox
        Left = 129
        Top = 142
        Width = 125
        Height = 21
        Hint = 'Filtro Pasa Altos para el audio de salida.'
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        Text = '200 Hz'
        OnChange = ComboBoxFiltroPasaAltosChange
        Items.Strings = (
          '50  Hz'
          '200 Hz'
          '300 Hz'
          '400 Hz'
          'Autom'#225'tico')
      end
      object ComboBoxFiltroPasaBajos: TComboBox
        Left = 129
        Top = 171
        Width = 125
        Height = 21
        Hint = 'Filtro Pasa Bajos para el audio de salida.'
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        Text = '4 KHz'
        OnChange = ComboBoxFiltroPasaBajosChange
        Items.Strings = (
          '3 KHz'
          '4 KHz'
          '6 KHz'
          '12 KHz'
          'Autom'#225'tico')
      end
      object TrackBarContrasteDelLCD: TTrackBar
        Left = 146
        Top = 196
        Width = 72
        Height = 24
        Hint = 'Contraste del la pantalla del receptor.'
        Anchors = [akLeft, akTop, akRight]
        Max = 31
        Orientation = trHorizontal
        ParentShowHint = False
        Frequency = 1
        Position = 10
        SelEnd = 0
        SelStart = 0
        ShowHint = True
        TabOrder = 7
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = TrackBarContrasteDelLCDChange
      end
      object CheckBoxLamparaDelLCD: TCheckBox
        Left = 28
        Top = 196
        Width = 113
        Height = 26
        Hint = 'Ilumina la pantalla del receptor.'
        Alignment = taLeftJustify
        Caption = 'Contraste del LCD'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
        OnClick = CheckBoxLamparaDelLCDClick
      end
    end
  end
  object BitBtn3: TBitBtn
    Left = 54
    Top = 244
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
    Left = 161
    Top = 244
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
    Left = 268
    Top = 244
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
    Left = 375
    Top = 244
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
    Left = 22
    Top = 195
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
