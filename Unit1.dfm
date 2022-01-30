object Form1: TForm1
  Left = 188
  Top = 134
  Width = 473
  Height = 218
  Caption = 'RX Serial Control'
  Color = clBtnShadow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 7
    Top = 28
    Width = 314
    Height = 149
    Stretch = True
  end
  object Button1: TButton
    Left = 328
    Top = 3
    Width = 116
    Height = 22
    Caption = 'Activar receptor'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 328
    Top = 156
    Width = 118
    Height = 21
    Caption = 'Apagar receptor'
    TabOrder = 1
    OnClick = Button3Click
  end
  object Button5: TButton
    Left = 328
    Top = 29
    Width = 117
    Height = 22
    Caption = 'Inicializar receptor'
    TabOrder = 2
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 328
    Top = 55
    Width = 117
    Height = 22
    Caption = 'Configurar receptor'
    TabOrder = 3
    OnClick = Button6Click
  end
  object ComboBoxRX: TComboBox
    Left = 199
    Top = 3
    Width = 121
    Height = 21
    ItemHeight = 13
    TabOrder = 4
    Text = 'AOR AR-ONE'
    OnChange = ComboBoxRXChange
    Items.Strings = (
      'AOR AR-ONE'
      'AOR AR5000A'
      'ICOM IC-R75'
      'ICOM IC-R8500')
  end
  object ComboBoxSerial: TComboBox
    Left = 5
    Top = 3
    Width = 81
    Height = 21
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 5
    Text = 'COM 1'
    Items.Strings = (
      'COM 1'
      'COM 2'
      'COM 3'
      'COM 4'
      'COM 5'
      'COM 6'
      'COM 7')
  end
  object Button2: TButton
    Left = 328
    Top = 129
    Width = 118
    Height = 21
    Caption = 'Escanear frecuencias'
    TabOrder = 6
    OnClick = Button2Click
  end
  object Button7: TButton
    Left = 90
    Top = 3
    Width = 105
    Height = 22
    Caption = 'Configurar puerto'
    TabOrder = 7
    OnClick = Button7Click
  end
  object GroupBox1: TGroupBox
    Left = 328
    Top = 80
    Width = 117
    Height = 44
    Caption = 'Frecuencia MHz:'
    TabOrder = 8
    DesignSize = (
      117
      44)
    object Edit1: TEdit
      Left = 4
      Top = 16
      Width = 70
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object Button4: TButton
      Left = 79
      Top = 17
      Width = 31
      Height = 19
      Anchors = [akTop, akRight]
      Caption = 'OK'
      TabOrder = 1
      OnClick = Button4Click
    end
  end
end
