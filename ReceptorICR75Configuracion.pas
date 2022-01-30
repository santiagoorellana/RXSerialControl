///////////////////////////////////////////////////////////////////////////////
// Autor: Santiago A. Orellana Pérez.
// Creado: 12/06/2013
// Escrito para: Delphi 6 y 7
// Utilización: Para configurar un receptor ICOM modelo IC-R75.
///////////////////////////////////////////////////////////////////////////////

unit ReceptorICR75Configuracion;

interface

//-----------------------------------------------------------------------------
// Dependencias de otros paquetes.
//-----------------------------------------------------------------------------
uses
   Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
   Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, ReceptorICR75, ActnList,
   ShellApi;

//-----------------------------------------------------------------------------
const CCarpetaDePDF = 'rx_manuals';
const CFicheroPDF   = 'ICOM_IC-R75.pdf';

//-----------------------------------------------------------------------------
// Objeto que implementa el diálogo de configuración.
//-----------------------------------------------------------------------------
type
  TFormReceptorICR75Configuracion = class(TForm)
    Panel1: TPanel;
    ActionList1: TActionList;
    ActionCerrar: TAction;
    ActionAceptar: TAction;
    ActionInformacion: TAction;
    ActionRestablecer: TAction;
    ActionAyuda: TAction;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn1: TBitBtn;
    Panel2: TPanel;
    Label10: TLabel;
    ComboBoxDemodulador: TComboBox;
    Label14: TLabel;
    ComboBoxAGC: TComboBox;
    TrackBarSquelch: TTrackBar;
    TrackBarNivelDelAudio: TTrackBar;
    Label6: TLabel;
    Label1: TLabel;
    LabelSquelch: TLabel;
    LabelNivelDelAudio: TLabel;
    Panel3: TPanel;
    ComboBoxAntena: TComboBox;
    CheckBoxAtenuador: TCheckBox;
    ComboBoxPreamplificador: TComboBox;
    TrackBarNivelDeRF: TTrackBar;
    TrackBarTwinPbtInterior: TTrackBar;
    TrackBarTwinPbtExterior: TTrackBar;
    TrackBarIluminacionLCD: TTrackBar;
    LabelIluminacionLCD: TLabel;
    LabelTwinPbtExterior: TLabel;
    LabelTwinPbtInterior: TLabel;
    LabelNivelDeRF: TLabel;
    Label8: TLabel;
    Label2: TLabel;
    Label18: TLabel;
    Label5: TLabel;
    Label13: TLabel;
    Label12: TLabel;
    Splitter1: TSplitter;
    procedure ComboBoxDemoduladorChange(Sender: TObject);
    procedure ComboBoxAntenaChange(Sender: TObject);
    procedure ActionCerrarExecute(Sender: TObject);
    procedure ComboBoxAGCChange(Sender: TObject);
    procedure TrackBarNivelDelAudioChange(Sender: TObject);
    procedure ActionAceptarExecute(Sender: TObject);
    procedure ActionInformacionExecute(Sender: TObject);
    procedure ActionRestablecerExecute(Sender: TObject);
    procedure ActionAyudaExecute(Sender: TObject);
    procedure TrackBarSquelchChange(Sender: TObject);
    procedure TrackBarTwinPbtInteriorChange(Sender: TObject);
    procedure TrackBarTwinPbtExteriorChange(Sender: TObject);
    procedure TrackBarIluminacionLCDChange(Sender: TObject);
    procedure TrackBarNivelDeRFChange(Sender: TObject);
    procedure CheckBoxAtenuadorClick(Sender: TObject);
    procedure ComboBoxPreamplificadorChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Receptor: TReceptorICR75;
  public
    constructor Crear(Objeto: TReceptorICR75);
    constructor MostrarConfiguracionActual(Receptor: TReceptorICR75);
  end;


implementation

{$R *.dfm}

//------------------------------------------------------------------------------
// Activa el Double buffered.
//------------------------------------------------------------------------------
procedure TFormReceptorICR75Configuracion.FormCreate(Sender: TObject);
begin
DoubleBuffered := True;
Panel1.DoubleBuffered := True;
Panel2.DoubleBuffered := True;
Panel3.DoubleBuffered := True;
end;

//-----------------------------------------------------------------------------
// Crea el formulario.
//-----------------------------------------------------------------------------
constructor TFormReceptorICR75Configuracion.Crear(Objeto: TReceptorICR75);
begin
Create(nil);                             //Llamma al constructor original del objeto.
Receptor := Objeto;                      //Copia la referencia al receptor.
MostrarConfiguracionActual(Receptor);    //Pone en  los controles los valores actuales.
end;

//-----------------------------------------------------------------------------
// Crea el formulario.
//-----------------------------------------------------------------------------
constructor TFormReceptorICR75Configuracion.MostrarConfiguracionActual(Receptor: TReceptorICR75);
begin
//Pone en  los controles los valores actuales.
case Receptor.Modo of
     mLSB:  ComboBoxDemodulador.ItemIndex := 0;    //LSB
     mUSB:  ComboBoxDemodulador.ItemIndex := 1;    //USB
     mAM:   ComboBoxDemodulador.ItemIndex := 2;    //AM
     mAM_W: ComboBoxDemodulador.ItemIndex := 3;    //AM Ancho
     mCW:   ComboBoxDemodulador.ItemIndex := 4;    //CW
     mCW_W: ComboBoxDemodulador.ItemIndex := 5;    //CW Ancho
     mFM:   ComboBoxDemodulador.ItemIndex := 6;    //FM
     mFM_W: ComboBoxDemodulador.ItemIndex := 7;    //FM Ancho
     else   ComboBoxDemodulador.ItemIndex := 0;
     end;
ComboBoxAGC.ItemIndex := Receptor.ControlAutomaticoDeGanancia;
TrackBarSquelch.Position := Receptor.NivelDelSquelch;
TrackBarNivelDelAudio.Position := Receptor.NivelDelAudio;
ComboBoxAntena.ItemIndex := Receptor.Antena;
CheckBoxAtenuador.Checked := Receptor.Atenuador = $20;
ComboBoxPreamplificador.ItemIndex := Receptor.AmplificadorRF;
TrackBarNivelDeRF.Position := Receptor.NivelDelRF;
TrackBarTwinPbtInterior.Position := Receptor.TwinPbtInterior;
TrackBarTwinPbtExterior.Position := Receptor.TwinPbtExterior;
TrackBarIluminacionLCD.Position := Receptor.IluminacionDelLCD;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el tipo de demodulador.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR75Configuracion.ComboBoxDemoduladorChange(Sender: TObject);
begin
case ComboBoxDemodulador.Itemindex of
     0: Receptor.Modo := mLSB;
     1: Receptor.Modo := mUSB;
     2: Receptor.Modo := mAM;
     3: Receptor.Modo := mAM_W;
     4: Receptor.Modo := mCW;
     5: Receptor.Modo := mCW_W;
     6: Receptor.Modo := mFM;
     7: Receptor.Modo := mFM_W;
     else Receptor.Modo := mLSB;
     end;
MostrarConfiguracionActual(Receptor);
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el atenuador.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR75Configuracion.CheckBoxAtenuadorClick(Sender: TObject);
begin
if CheckBoxAtenuador.Checked then
   Receptor.Atenuador := $20
else   
   Receptor.Atenuador := $00;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie la antena.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR75Configuracion.ComboBoxAntenaChange(Sender: TObject);
begin
Receptor.Antena := ComboBoxAntena.Itemindex;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el AGC.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR75Configuracion.ComboBoxAGCChange(Sender: TObject);
begin
Receptor.ControlAutomaticoDeGanancia := ComboBoxAGC.Itemindex;
end;
          
//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie
//-----------------------------------------------------------------------------
procedure TFormReceptorICR75Configuracion.TrackBarNivelDelAudioChange(Sender: TObject);
begin
with TrackBarNivelDelAudio do
     begin
     LabelNivelDelAudio.Caption := IntToStr(Round(Position / Max * 100)) + '%';;
     Receptor.NivelDelAudio := Position;
     end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el Twin Pbt Interior.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR75Configuracion.TrackBarTwinPbtInteriorChange(Sender: TObject);
begin
with TrackBarTwinPbtInterior do
     begin
     LabelTwinPbtInterior.Caption := IntToStr(Round(Position / Max * 100)) + '%';;
     Receptor.TwinPbtInterior := Position;
     end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el Twin Pbt Exterior.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR75Configuracion.TrackBarTwinPbtExteriorChange(Sender: TObject);
begin
with TrackBarTwinPbtExterior do
     begin
     LabelTwinPbtExterior.Caption := IntToStr(Round(Position / Max * 100)) + '%';;
     Receptor.TwinPbtExterior := Position;
     end;
end;

//-----------------------------------------------------------------------------
// Cierra el diálogo de configuración al presionar Escape.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR75Configuracion.ActionCerrarExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Cierra el diálogo de configuración al presionar Enter.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR75Configuracion.ActionAceptarExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Muestra la ayuda.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR75Configuracion.ActionInformacionExecute(Sender: TObject);
var Fichero: String;
begin
Fichero := ExtractFilePath(Application.ExeName) + '\' + CCarpetaDePDF + '\' + CFicheroPDF;
if FileExists(Fichero) then
   ShellExecute(Handle, nil, PChar(Fichero), '', '', SW_SHOWNORMAL);
end;

//-----------------------------------------------------------------------------
// Restablece la configuración por defecto del receptor.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR75Configuracion.ActionRestablecerExecute(Sender: TObject);
begin
Receptor.Inicializar;
MostrarConfiguracionActual(Receptor);
end;

//-----------------------------------------------------------------------------
// Envia comando para cambiar el Squelch.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR75Configuracion.TrackBarSquelchChange(Sender: TObject);
begin
with TrackBarSquelch do
     begin
     LabelSquelch.Caption := IntToStr(Round(Position / Max * 100)) + '%';;
     Receptor.NivelDelSquelch := Position;
     end;
end;

//-----------------------------------------------------------------------------
// Envia comando para cambiar la iluminación del LCD.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR75Configuracion.TrackBarIluminacionLCDChange(Sender: TObject);
begin
with TrackBarIluminacionLCD do
     begin
     LabelIluminacionLCD.Caption := IntToStr(Round(Position / Max * 100)) + '%';;
     Receptor.IluminacionDelLCD := Position;
     end;
end;

//-----------------------------------------------------------------------------
// Envia comando para cambiar el nivel de RF.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR75Configuracion.TrackBarNivelDeRFChange(Sender: TObject);
begin
with TrackBarNivelDeRF do
     begin
     LabelNivelDeRF.Caption := IntToStr(Round(Position / Max * 100)) + '%';;
     Receptor.NivelDelRF := Position;
     end;
end;

//------------------------------------------------------------------------------
// Envía comandos para establecer el preamplificador.
//------------------------------------------------------------------------------
procedure TFormReceptorICR75Configuracion.ComboBoxPreamplificadorChange(Sender: TObject);
begin
Receptor.AmplificadorRF := ComboBoxPreamplificador.ItemIndex;
end;

//------------------------------------------------------------------------------
// Muestra la ayuda de la ventana.
//------------------------------------------------------------------------------
procedure TFormReceptorICR75Configuracion.ActionAyudaExecute(Sender: TObject);
var msg: String;
begin
msg := 'Configuración del receptor ICOM IC-R75' + #13#13;
msg := msg + 'Aquí se debe colocar el texto de ayuda básica...' + #13;
MessageBox(0, PChar(msg), 'AYUDA', MB_OK);
end;

end.
