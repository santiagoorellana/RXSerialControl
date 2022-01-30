///////////////////////////////////////////////////////////////////////////////
// Autor: Santiago A. Orellana Pérez.
// Creado: 30/05/2013
// Escrito para: Delphi 6 y 7
// Utilización: Para configurar un receptor AOR modelo AR5000A.
///////////////////////////////////////////////////////////////////////////////

unit ReceptorAR5000AConfiguracion;

interface

//-----------------------------------------------------------------------------
// Dependencias de otros paquetes.
//-----------------------------------------------------------------------------
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ReceptorAR5000A, ActnList, Buttons, ExtCtrls,
  Spin, ShellApi;

//-----------------------------------------------------------------------------
const CCarpetaDePDF = 'rx_manuals';
const CFicheroPDF   = 'AOR_AR5000A.pdf';

//-----------------------------------------------------------------------------
// Objeto que implementa el diálogo de configuración.
//-----------------------------------------------------------------------------
type
  TFormReceptorAR5000AConfiguracion = class(TForm)
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
    PanelA: TPanel;
    Label2: TLabel;
    ComboBoxDemodulador: TComboBox;
    Label3: TLabel;
    ComboBoxAnchoDeBanda: TComboBox;
    Label1: TLabel;
    ComboBoxAGC: TComboBox;
    Label8: TLabel;
    Label6: TLabel;
    TrackBarNivelDelAudio: TTrackBar;
    TrackBarSquelch: TTrackBar;
    LabelSquelch: TLabel;
    LabelNivelDelAudio: TLabel;
    PanelB: TPanel;
    Label5: TLabel;
    ComboBoxAntena: TComboBox;
    ComboBoxAtenuador: TComboBox;
    ComboBoxFiltroPasaAltos: TComboBox;
    ComboBoxFiltroPasaBajos: TComboBox;
    ComboBoxSalidaDeFI: TComboBox;
    Label7: TLabel;
    Label10: TLabel;
    Label9: TLabel;
    Label4: TLabel;
    Splitter1: TSplitter;
    procedure ComboBoxDemoduladorChange(Sender: TObject);
    procedure ComboBoxAnchoDeBandaChange(Sender: TObject);
    procedure ComboBoxAtenuadorChange(Sender: TObject);
    procedure ComboBoxAntenaChange(Sender: TObject);
    procedure ComboBoxFiltroPasaAltosChange(Sender: TObject);
    procedure ComboBoxFiltroPasaBajosChange(Sender: TObject);
    procedure ActionCerrarExecute(Sender: TObject);
    procedure ComboBoxAGCChange(Sender: TObject);
    procedure TrackBarNivelDelAudioChange(Sender: TObject);
    procedure ActionAceptarExecute(Sender: TObject);
    procedure ActionInformacionExecute(Sender: TObject);
    procedure ActionRestablecerExecute(Sender: TObject);
    procedure ComboBoxSalidaDeFIChange(Sender: TObject);
    procedure ActionAyudaExecute(Sender: TObject);
    procedure TrackBarSquelchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Receptor: TReceptorAR5000A;
  public
    constructor Crear(Objeto: TReceptorAR5000A);
    constructor MostrarConfiguracionActual(Receptor: TReceptorAR5000A);
  end;


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

implementation

uses Unit1;


{$R *.dfm}

//------------------------------------------------------------------------------
// Activa el Double buffered.
//------------------------------------------------------------------------------
procedure TFormReceptorAR5000AConfiguracion.FormCreate(Sender: TObject);
begin
DoubleBuffered := True;
Panel1.DoubleBuffered := True;
PanelA.DoubleBuffered := True;
PanelB.DoubleBuffered := True;
end;

//-----------------------------------------------------------------------------
// Crea el formulario.
//-----------------------------------------------------------------------------
constructor TFormReceptorAR5000AConfiguracion.Crear(Objeto: TReceptorAR5000A);
begin
Create(nil);                             //Llamma al constructor original del objeto.
Receptor := Objeto;                      //Copia la referencia al receptor.
MostrarConfiguracionActual(Receptor);    //Pone en  los controles los valores actuales.
end;

//-----------------------------------------------------------------------------
// Crea el formulario.
//-----------------------------------------------------------------------------
constructor TFormReceptorAR5000AConfiguracion.MostrarConfiguracionActual(Receptor: TReceptorAR5000A);
begin
//Pone en  los controles los valores actuales.
ComboBoxDemodulador.ItemIndex := Receptor.Demodulador;
ComboBoxAnchoDeBanda.ItemIndex := Receptor.AnchoDebanda;
with ComboBoxAtenuador do
     if Receptor.Atenuador <> 255 then
        ItemIndex := Receptor.Atenuador
     else
        ItemIndex := Items.Count - 1;
with ComboBoxAGC do
     if Receptor.ControlAutomaticoDeGanancia <> 255 then
        ItemIndex := Receptor.ControlAutomaticoDeGanancia
     else
        ItemIndex := Items.Count - 1;
ComboBoxAntena.ItemIndex := Receptor.Antena;
TrackBarNivelDelAudio.Position := Receptor.NivelDelAudio;
TrackBarSquelch.Position := Receptor.Squelch;
ComboBoxFiltroPasaAltos.ItemIndex := Receptor.FiltroPasaAltos;
ComboBoxFiltroPasaBajos.ItemIndex := Receptor.FiltroPasaBajos;
ComboBoxSalidaDeFI.ItemIndex := Receptor.SalidaDeFI;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el tipo de demodulador.
//-----------------------------------------------------------------------------
procedure TFormReceptorAR5000AConfiguracion.ComboBoxDemoduladorChange(Sender: TObject);
begin
Receptor.Demodulador := ComboBoxDemodulador.Itemindex;
MostrarConfiguracionActual(Receptor); 
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el ancho de banda.
//-----------------------------------------------------------------------------
procedure TFormReceptorAR5000AConfiguracion.ComboBoxAnchoDeBandaChange(Sender: TObject);
begin
Receptor.AnchoDebanda := ComboBoxAnchoDeBanda.Itemindex;
MostrarConfiguracionActual(Receptor); 
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el atenuador.
//-----------------------------------------------------------------------------
procedure TFormReceptorAR5000AConfiguracion.ComboBoxAtenuadorChange(Sender: TObject);
begin
Receptor.Atenuador := ComboBoxAtenuador.Itemindex;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el amplificador de RF.
//-----------------------------------------------------------------------------
procedure TFormReceptorAR5000AConfiguracion.ComboBoxAntenaChange(Sender: TObject);
begin
Receptor.Antena := ComboBoxAntena.Itemindex;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el AGC.
//-----------------------------------------------------------------------------
procedure TFormReceptorAR5000AConfiguracion.ComboBoxAGCChange(Sender: TObject);
begin
Receptor.ControlAutomaticoDeGanancia := ComboBoxAGC.Itemindex;
end;
          
//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie
//-----------------------------------------------------------------------------
procedure TFormReceptorAR5000AConfiguracion.TrackBarNivelDelAudioChange(Sender: TObject);
begin
with TrackBarNivelDelAudio do
     begin
     LabelNivelDelAudio.Caption := IntToStr(Round(Position / Max * 100)) + '%';;
     Receptor.NivelDelAudio := Position;
     end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el filtro pasa bajos.
//-----------------------------------------------------------------------------
procedure TFormReceptorAR5000AConfiguracion.ComboBoxFiltroPasaAltosChange(Sender: TObject);
begin
Receptor.FiltroPasaAltos := ComboBoxFiltroPasaAltos.ItemIndex
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el filtro pasa bajos.
//-----------------------------------------------------------------------------
procedure TFormReceptorAR5000AConfiguracion.ComboBoxFiltroPasaBajosChange(Sender: TObject);
begin
Receptor.FiltroPasaBajos := ComboBoxFiltroPasaBajos.ItemIndex
end;

//-----------------------------------------------------------------------------
// Cierra el diálogo de configuración al presionar Escape.
//-----------------------------------------------------------------------------
procedure TFormReceptorAR5000AConfiguracion.ActionCerrarExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Cierra el diálogo de configuración al presionar Enter.
//-----------------------------------------------------------------------------
procedure TFormReceptorAR5000AConfiguracion.ActionAceptarExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Muestra la ayuda.
//-----------------------------------------------------------------------------
procedure TFormReceptorAR5000AConfiguracion.ActionInformacionExecute(Sender: TObject);
var Fichero: String;
begin
Fichero := ExtractFilePath(Application.ExeName) + '\' + CCarpetaDePDF + '\' + CFicheroPDF;
if FileExists(Fichero) then
   ShellExecute(Form1.Handle, nil, PChar(Fichero), '', '', SW_SHOWNORMAL);
end;

//-----------------------------------------------------------------------------
// Restablece la configuración por defecto del receptor.
//-----------------------------------------------------------------------------
procedure TFormReceptorAR5000AConfiguracion.ActionRestablecerExecute(Sender: TObject);
begin
Receptor.Inicializar;
MostrarConfiguracionActual(Receptor);
end;

//-----------------------------------------------------------------------------
// Establece la salida de FI.
//-----------------------------------------------------------------------------
procedure TFormReceptorAR5000AConfiguracion.ComboBoxSalidaDeFIChange(Sender: TObject);
begin
Receptor.SalidaDeFI := ComboBoxSalidaDeFI.ItemIndex;
end;

//------------------------------------------------------------------------------
// Muestra la ayuda de la ventana.
//------------------------------------------------------------------------------
procedure TFormReceptorAR5000AConfiguracion.ActionAyudaExecute(Sender: TObject);
var msg: String;
begin
msg := 'Configuración del receptor AOR AR5000A' + #13#13;
msg := msg + 'Aquí se debe colocar el texto de ayuda básica...' + #13;
MessageBox(0, PChar(msg), 'AYUDA', MB_OK);
end;

//------------------------------------------------------------------------------
// Establece el nivel del Squelch.
//------------------------------------------------------------------------------
procedure TFormReceptorAR5000AConfiguracion.TrackBarSquelchChange(Sender: TObject);
begin
with TrackBarSquelch do
     begin
     LabelSquelch.Caption := IntToStr(Round(Position / Max * 100)) + '%';;
     Receptor.Squelch := 255 - Position;
     end;
end;


end.
