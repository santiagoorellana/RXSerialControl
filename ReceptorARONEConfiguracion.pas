///////////////////////////////////////////////////////////////////////////////
// Autor: Santiago A. Orellana Pérez.
// Creado: 30/05/2013
// Escrito para: Delphi 6 y 7
// Utilización: Para configurar un receptor AOR modelo AR-ONE.
///////////////////////////////////////////////////////////////////////////////

unit ReceptorARONEConfiguracion;

interface

//-----------------------------------------------------------------------------
// Dependencias de otros paquetes.
//-----------------------------------------------------------------------------
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ReceptorARONE, ActnList, Buttons, ExtCtrls,
  Spin, ShellApi;

//-----------------------------------------------------------------------------
const CCarpetaDePDF = 'rx_manuals';
const CFicheroPDF   = 'AOR_AR-ONE.pdf';

//-----------------------------------------------------------------------------
// Objeto que implementa el diálogo de configuración.
//-----------------------------------------------------------------------------
type
  TFormReceptorARONEConfiguracion = class(TForm)
    ActionList1: TActionList;
    ActionCerrar: TAction;
    Panel1: TPanel;
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
    TrackBarSquelch: TTrackBar;
    Label11: TLabel;
    CheckBoxBocinaActivada: TCheckBox;
    TrackBarNivelDelAudio: TTrackBar;
    LabelNivelDelAudio: TLabel;
    LabelSquelch: TLabel;
    Splitter1: TSplitter;
    PanelB: TPanel;
    Label4: TLabel;
    ComboBoxAtenuador: TComboBox;
    ComboBoxAmplificadorRF: TComboBox;
    TrackBarGananciaRF: TTrackBar;
    TrackBarGanancia10700Khz: TTrackBar;
    TrackBarGananciaFI: TTrackBar;
    Label8: TLabel;
    Label7: TLabel;
    Label6: TLabel;
    Label5: TLabel;
    LabelGananciaDeFIValor: TLabel;
    LabelGananciaDe10700KhzValor: TLabel;
    LabelGananciaDeRFValor: TLabel;
    ComboBoxFiltroPasaAltos: TComboBox;
    ComboBoxFiltroPasaBajos: TComboBox;
    TrackBarContrasteDelLCD: TTrackBar;
    LabelContrasteDelLCD: TLabel;
    CheckBoxLamparaDelLCD: TCheckBox;
    Label10: TLabel;
    Label9: TLabel;
    procedure ComboBoxDemoduladorChange(Sender: TObject);
    procedure ComboBoxAnchoDeBandaChange(Sender: TObject);
    procedure ComboBoxAtenuadorChange(Sender: TObject);
    procedure ComboBoxAmplificadorRFChange(Sender: TObject);
    procedure TrackBarGananciaRFChange(Sender: TObject);
    procedure TrackBarGanancia10700KhzChange(Sender: TObject);
    procedure TrackBarGananciaFIChange(Sender: TObject);
    procedure ComboBoxFiltroPasaAltosChange(Sender: TObject);
    procedure ComboBoxFiltroPasaBajosChange(Sender: TObject);
    procedure ActionCerrarExecute(Sender: TObject);
    procedure CheckBoxBocinaActivadaClick(Sender: TObject);
    procedure TrackBarContrasteDelLCDChange(Sender: TObject);
    procedure ComboBoxAGCChange(Sender: TObject);
    procedure TrackBarNivelDelAudioChange(Sender: TObject);
    procedure CheckBoxLamparaDelLCDClick(Sender: TObject);
    procedure ActionAceptarExecute(Sender: TObject);
    procedure ActionInformacionExecute(Sender: TObject);
    procedure ActionRestablecerExecute(Sender: TObject);
    procedure ActionAyudaExecute(Sender: TObject);
    procedure TrackBarSquelchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Receptor: TReceptorARONE;
  public
    constructor Crear(Objeto: TReceptorARONE);
    constructor MostrarConfiguracionActual(Receptor: TReceptorARONE);
  end;

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

implementation

uses Unit1;


{$R *.dfm}

//------------------------------------------------------------------------------
// Activa el Double buffered.
//------------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.FormCreate(Sender: TObject);
begin
DoubleBuffered := True;
Panel1.DoubleBuffered := True;
PanelA.DoubleBuffered := True;
PanelB.DoubleBuffered := True;
end;

//-----------------------------------------------------------------------------
// Crea el formulario.
//-----------------------------------------------------------------------------
constructor TFormReceptorARONEConfiguracion.Crear(Objeto: TReceptorARONE);
begin
Create(nil);                             //Llamma al constructor original del objeto.
Receptor := Objeto;                      //Copia la referencia al receptor.
MostrarConfiguracionActual(Receptor);    //Pone en  los controles los valores actuales.
end;

//-----------------------------------------------------------------------------
// Crea el formulario.
//-----------------------------------------------------------------------------
constructor TFormReceptorARONEConfiguracion.MostrarConfiguracionActual(Receptor: TReceptorARONE);
begin
//Pone en  los controles los valores actuales.
ComboBoxDemodulador.ItemIndex := Receptor.Demodulador;
ComboBoxAnchoDeBanda.ItemIndex := Receptor.AnchoDebanda;
ComboBoxAtenuador.ItemIndex := Receptor.Atenuador;
ComboBoxAmplificadorRF.ItemIndex := Receptor.AmplificadorDeRF;
ComboBoxAGC.ItemIndex := Receptor.ControlAutomaticoDeGanancia;
TrackBarGananciaRF.Position := Receptor.GananciaDeRF;
TrackBarGanancia10700Khz.Position := Receptor.GananciaDe10700Khz;
TrackBarGananciaFI.Position := Receptor.GananciaDeFI;
TrackBarContrasteDelLCD.Position := Receptor.ContrasteDelLCD;
TrackBarNivelDelAudio.Position := Receptor.NivelDelAudio;
TrackBarSquelch.Position := Receptor.Squelch;
TrackBarSquelchChange(Self);
with ComboBoxFiltroPasaAltos do
     if Receptor.FiltroPasaAltos <> 255 then
        ItemIndex := Receptor.FiltroPasaAltos
     else
        ItemIndex := ComboBoxFiltroPasaAltos.Items.Count - 1;
with ComboBoxFiltroPasaBajos do
     if Receptor.FiltroPasaBajos <> 255 then
        ItemIndex := Receptor.FiltroPasaBajos
     else
        ItemIndex := ComboBoxFiltroPasaBajos.Items.Count - 1;
with CheckBoxBocinaActivada do
     if Receptor.BocinaActivada = 0 then Checked := False else Checked := True;
CheckBoxBocinaActivadaClick(Self);
with CheckBoxLamparaDelLCD do
     if Receptor.LamparaDelLCD = 0 then Checked := False else Checked := True;
CheckBoxLamparaDelLCDClick(Self);
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el tipo de demodulador.
//-----------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.ComboBoxDemoduladorChange(Sender: TObject);
begin
Receptor.Demodulador := ComboBoxDemodulador.Itemindex;
MostrarConfiguracionActual(Receptor); 
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el ancho de banda.
//-----------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.ComboBoxAnchoDeBandaChange(Sender: TObject);
begin
Receptor.AnchoDebanda := ComboBoxAnchoDeBanda.Itemindex;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el atenuador.
//-----------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.ComboBoxAtenuadorChange(Sender: TObject);
begin
Receptor.Atenuador := ComboBoxAtenuador.Itemindex;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el amplificador de RF.
//-----------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.ComboBoxAmplificadorRFChange(Sender: TObject);
begin
Receptor.AmplificadorDeRF := ComboBoxAmplificadorRF.Itemindex;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el AGC.
//-----------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.ComboBoxAGCChange(Sender: TObject);
begin
Receptor.ControlAutomaticoDeGanancia := ComboBoxAmplificadorRF.Itemindex;
end;
          
//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie la ganancia de RF.
//-----------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.TrackBarGananciaRFChange(Sender: TObject);
begin
with TrackBarGananciaRF do
     begin
     Receptor.GananciaDeRF := Position;
     LabelGananciaDeRFValor.Caption := IntToStr(Round(Position / Max * 100)) + '%';;
     end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie la ganancia de 10.7 Mhs.
//-----------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.TrackBarGanancia10700KhzChange(Sender: TObject);
begin
with TrackBarGanancia10700Khz do
     begin
     Receptor.GananciaDe10700Khz := Position;
     LabelGananciaDe10700KhzValor.Caption := IntToStr(Round(Position / Max * 100)) + '%';;
     end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie la ganancia de FI.
//-----------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.TrackBarGananciaFIChange(Sender: TObject);
begin
with TrackBarGananciaFI do
     begin
     Receptor.GananciaDeFI := Position;
     LabelGananciaDeFIValor.Caption := IntToStr(Round(Position / Max * 100)) + '%';;
     end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie
//-----------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.TrackBarNivelDelAudioChange(Sender: TObject);
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
procedure TFormReceptorARONEConfiguracion.ComboBoxFiltroPasaAltosChange(Sender: TObject);
begin
if ComboBoxFiltroPasaAltos.ItemIndex <> ComboBoxFiltroPasaAltos.Items.Count - 1 then
   Receptor.FiltroPasaAltos := ComboBoxFiltroPasaAltos.ItemIndex
else
   Receptor.FiltroPasaAltos := 255;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el filtro pasa bajos.
//-----------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.ComboBoxFiltroPasaBajosChange(Sender: TObject);
begin
if ComboBoxFiltroPasaBajos.ItemIndex <> ComboBoxFiltroPasaBajos.Items.Count - 1 then
   Receptor.FiltroPasaBajos := ComboBoxFiltroPasaBajos.ItemIndex
else
   Receptor.FiltroPasaBajos := 255;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para activar o desactivar la bocina.
//-----------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.CheckBoxBocinaActivadaClick(Sender: TObject);
begin
if CheckBoxBocinaActivada.Checked then
   Receptor.BocinaActivada := 1
else
   Receptor.BocinaActivada := 0;
TrackBarNivelDelAudio.Enabled := CheckBoxBocinaActivada.Checked;
TrackBarNivelDelAudio.Visible := CheckBoxBocinaActivada.Checked;
LabelNivelDelAudio.Visible := CheckBoxBocinaActivada.Checked;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para cambiar el contraste del LCD.
//-----------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.TrackBarContrasteDelLCDChange(Sender: TObject);
begin
with TrackBarContrasteDelLCD do
     begin
     LabelContrasteDelLCD.Caption := IntToStr(Round(Position / Max * 100)) + '%';;
     Receptor.ContrasteDelLCD := Position;
     end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para activar o desactivar la lámpara del LCD.
//-----------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.CheckBoxLamparaDelLCDClick(Sender: TObject);
begin
if CheckBoxLamparaDelLCD.Checked then
   Receptor.LamparaDelLCD := 1
else
   Receptor.LamparaDelLCD := 0;
TrackBarContrasteDelLCD.Enabled := CheckBoxLamparaDelLCD.Checked;
TrackBarContrasteDelLCD.Visible := CheckBoxLamparaDelLCD.Checked;
LabelContrasteDelLCD.Visible := CheckBoxLamparaDelLCD.Checked;
end;

//-----------------------------------------------------------------------------
// Cierra el diálogo de configuración al presionar Escape.
//-----------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.ActionCerrarExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Cierra el diálogo de configuración al presionar Enter.
//-----------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.ActionAceptarExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Muestra la ayuda.
//-----------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.ActionInformacionExecute(Sender: TObject);
var Fichero: String;
begin
Fichero := ExtractFilePath(Application.ExeName) + '\' + CCarpetaDePDF + '\' + CFicheroPDF;
if FileExists(Fichero) then
   ShellExecute(Form1.Handle, nil, PChar(Fichero), '', '', SW_SHOWNORMAL);
end;

//-----------------------------------------------------------------------------
// Restablece la configuración por defecto del receptor.
//-----------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.ActionRestablecerExecute(Sender: TObject);
begin
Receptor.Inicializar;
MostrarConfiguracionActual(Receptor);
end;

//------------------------------------------------------------------------------
// Muestra la ayuda de la ventana.
//------------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.ActionAyudaExecute(Sender: TObject);
var msg: String;
begin
msg := 'Configuración del receptor AOR AR5000A' + #13#13;
msg := msg + 'Aquí se debe colocar el texto de ayuda básica...' + #13;
MessageBox(0, PChar(msg), 'AYUDA', MB_OK);
end;

//------------------------------------------------------------------------------
// Establece el nivel del Squelch.
//------------------------------------------------------------------------------
procedure TFormReceptorARONEConfiguracion.TrackBarSquelchChange(Sender: TObject);
begin
with TrackBarSquelch do
     begin
     LabelSquelch.Caption := IntToStr(Round(Position / Max * 100)) + '%';;
     Receptor.Squelch := Position;
     end;
end;


end.
