///////////////////////////////////////////////////////////////////////////////
// Autor: Santiago A. Orellana Pérez.
// Creado: 20/06/2013
// Escrito para: Delphi 6 y 7
// Utilización: Para configurar un receptor ICOM modelo IC-R8500.
///////////////////////////////////////////////////////////////////////////////

unit ReceptorICR8500Configuracion;

interface

//-----------------------------------------------------------------------------
// Dependencias de otros paquetes.
//-----------------------------------------------------------------------------
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, Buttons, ComCtrls, ExtCtrls, ShellApi, ReceptorICR8500;

//-----------------------------------------------------------------------------
const CCarpetaDePDF = 'rx_manuals';
const CFicheroPDF   = 'ICOM_IC-R8500.pdf';

//-----------------------------------------------------------------------------
// Objeto que implementa el diálogo de configuración.
//-----------------------------------------------------------------------------
type
  TFormReceptorICR8500Configuracion = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    PanelA: TPanel;
    Label10: TLabel;
    Label6: TLabel;
    Label1: TLabel;
    LabelSquelch: TLabel;
    LabelNivelDeAudio: TLabel;
    ComboBoxDemodulador: TComboBox;
    TrackBarSquelch: TTrackBar;
    TrackBarNivelDelAudio: TTrackBar;
    PanelB: TPanel;
    LabelAPF: TLabel;
    LabelFI: TLabel;
    Label18: TLabel;
    TrackBarFI: TTrackBar;
    TrackBarAPF: TTrackBar;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn1: TBitBtn;
    ActionList1: TActionList;
    ActionCerrar: TAction;
    ActionAceptar: TAction;
    ActionInformacion: TAction;
    ActionRestablecer: TAction;
    ActionAyuda: TAction;
    CheckBoxAGC: TCheckBox;
    Label3: TLabel;
    ComboBoxAtenuador: TComboBox;
    CheckBoxAPF: TCheckBox;
    CheckBoxNoiseBlanker: TCheckBox;
    procedure ComboBoxDemoduladorChange(Sender: TObject);
    procedure ActionCerrarExecute(Sender: TObject);
    procedure TrackBarNivelDelAudioChange(Sender: TObject);
    procedure ActionAceptarExecute(Sender: TObject);
    procedure ActionInformacionExecute(Sender: TObject);
    procedure ActionRestablecerExecute(Sender: TObject);
    procedure ActionAyudaExecute(Sender: TObject);
    procedure TrackBarSquelchChange(Sender: TObject);
    procedure TrackBarFIChange(Sender: TObject);
    procedure TrackBarAPFChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxAGCClick(Sender: TObject);
    procedure ComboBoxAtenuadorChange(Sender: TObject);
    procedure CheckBoxAPFClick(Sender: TObject);
    procedure CheckBoxNoiseBlankerClick(Sender: TObject);
  private
    Receptor: TReceptorICR8500;
  public
    constructor Crear(Objeto: TReceptorICR8500);
    constructor MostrarConfiguracionActual(Receptor: TReceptorICR8500);
  end;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
// Activa el Double buffered.
//------------------------------------------------------------------------------
procedure TFormReceptorICR8500Configuracion.FormCreate(Sender: TObject);
begin
DoubleBuffered := True;
Panel1.DoubleBuffered := True;
PanelA.DoubleBuffered := True;
PanelB.DoubleBuffered := True;
end;

//-----------------------------------------------------------------------------
// Crea el formulario.
//-----------------------------------------------------------------------------
constructor TFormReceptorICR8500Configuracion.Crear(Objeto: TReceptorICR8500);
begin
Create(nil);                             //Llamma al constructor original del objeto.
Receptor := Objeto;                      //Copia la referencia al receptor.
MostrarConfiguracionActual(Receptor);    //Pone en  los controles los valores actuales.
end;

//-----------------------------------------------------------------------------
// Crea el formulario.
//-----------------------------------------------------------------------------
constructor TFormReceptorICR8500Configuracion.MostrarConfiguracionActual(Receptor: TReceptorICR8500);
begin
//Pone en  los controles los valores actuales.
case Receptor.Modo of
     mLSB:  ComboBoxDemodulador.ItemIndex := 0;    //LSB
     mUSB:  ComboBoxDemodulador.ItemIndex := 1;    //USB
     mAM:   ComboBoxDemodulador.ItemIndex := 2;    //AM
     mAM_N: ComboBoxDemodulador.ItemIndex := 3;    //AM Estrecho
     mAM_W: ComboBoxDemodulador.ItemIndex := 4;    //AM Ancho
     mCW:   ComboBoxDemodulador.ItemIndex := 5;    //CW
     mFM:   ComboBoxDemodulador.ItemIndex := 6;    //FM
     mFM_N: ComboBoxDemodulador.ItemIndex := 7;    //FM Estrecho
     else   ComboBoxDemodulador.ItemIndex := 0;
     end;
TrackBarSquelch.Position := Receptor.NivelDelSquelch;
TrackBarNivelDelAudio.Position := Receptor.NivelDelAudio;
TrackBarFI.Position := Receptor.FI;
TrackBarAPF.Position := Receptor.APF;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el tipo de demodulador.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR8500Configuracion.ComboBoxDemoduladorChange(Sender: TObject);
begin
case ComboBoxDemodulador.Itemindex of
     0: Receptor.Modo := mLSB;
     1: Receptor.Modo := mUSB;
     2: Receptor.Modo := mAM;
     3: Receptor.Modo := mAM_N;
     4: Receptor.Modo := mAM_W;
     5: Receptor.Modo := mCW;
     6: Receptor.Modo := mFM;
     7: Receptor.Modo := mFM_N;
     else Receptor.Modo := mLSB;
     end;
MostrarConfiguracionActual(Receptor);
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie
//-----------------------------------------------------------------------------
procedure TFormReceptorICR8500Configuracion.TrackBarNivelDelAudioChange(Sender: TObject);
begin
with TrackBarNivelDelAudio do
     begin
     LabelNivelDeAudio.Caption := IntToStr(Round(Position / Max * 100)) + '%';;
     Receptor.NivelDelAudio := Position;
     end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el Twin Pbt Interior.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR8500Configuracion.TrackBarFIChange(Sender: TObject);
begin
with TrackBarFI do
     begin
     LabelFI.Caption := IntToStr(Round(Position / Max * 100)) + '%';;
     Receptor.FI := Position;
     end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el Twin Pbt Exterior.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR8500Configuracion.TrackBarAPFChange(Sender: TObject);
begin
with TrackBarAPF do
     begin
     LabelAPF.Caption := IntToStr(Round(Position / Max * 100)) + '%';;
     Receptor.APF := Position;
     end;
end;

//-----------------------------------------------------------------------------
// Cierra el diálogo de configuración al presionar Escape.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR8500Configuracion.ActionCerrarExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Cierra el diálogo de configuración al presionar Enter.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR8500Configuracion.ActionAceptarExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Muestra la ayuda.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR8500Configuracion.ActionInformacionExecute(Sender: TObject);
var Fichero: String;
begin
Fichero := ExtractFilePath(Application.ExeName) + '\' + CCarpetaDePDF + '\' + CFicheroPDF;
if FileExists(Fichero) then
   ShellExecute(Handle, nil, PChar(Fichero), '', '', SW_SHOWNORMAL);
end;

//-----------------------------------------------------------------------------
// Restablece la configuración por defecto del receptor.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR8500Configuracion.ActionRestablecerExecute(Sender: TObject);
begin
Receptor.Inicializar;
MostrarConfiguracionActual(Receptor);
end;

//-----------------------------------------------------------------------------
// Envia comando para cambiar el Squelch.
//-----------------------------------------------------------------------------
procedure TFormReceptorICR8500Configuracion.TrackBarSquelchChange(Sender: TObject);
begin
with TrackBarSquelch do
     begin
     LabelSquelch.Caption := IntToStr(Round(Position / Max * 100)) + '%';;
     Receptor.NivelDelSquelch := Position;
     end;
end;

//------------------------------------------------------------------------------
// Muestra la ayuda de la ventana.
//------------------------------------------------------------------------------
procedure TFormReceptorICR8500Configuracion.ActionAyudaExecute(Sender: TObject);
var msg: String;
begin
msg := 'Configuración del receptor ICOM IC-R75' + #13#13;
msg := msg + 'Aquí se debe colocar el texto de ayuda básica...' + #13;
MessageBox(0, PChar(msg), 'AYUDA', MB_OK);
end;

//------------------------------------------------------------------------------
// Envia comandos al receptor para cambiar el AGC.
//------------------------------------------------------------------------------
procedure TFormReceptorICR8500Configuracion.CheckBoxAGCClick(Sender: TObject);
begin
if CheckBoxAGC.Checked then
   Receptor.ControlAutomaticoDeGanancia := $11
else
   Receptor.ControlAutomaticoDeGanancia := $10;
end;

//------------------------------------------------------------------------------
// Envia comandos al receptor para cambiar el Atenuador.
//------------------------------------------------------------------------------
procedure TFormReceptorICR8500Configuracion.ComboBoxAtenuadorChange(Sender: TObject);
begin
Receptor.Atenuador := ComboBoxAtenuador.ItemIndex shl 4;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
procedure TFormReceptorICR8500Configuracion.CheckBoxAPFClick(Sender: TObject);
begin
if CheckBoxAPF.Checked then
   Receptor.ActivoAPF := $31
else
   Receptor.ActivoAPF := $30;
TrackBarAPF.Visible := CheckBoxAPF.Checked;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
procedure TFormReceptorICR8500Configuracion.CheckBoxNoiseBlankerClick(Sender: TObject);
begin
if CheckBoxNoiseBlanker.Checked then
   Receptor.NoiseBlanker := $21
else
   Receptor.NoiseBlanker := $20;
end;

end.
