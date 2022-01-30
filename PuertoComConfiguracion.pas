
///////////////////////////////////////////////////////////////////////////////
// Nombre: PuertoComConfiguracion
// Autor: Santiago A. Orellana Pérez.
// Creado: 15/05/2013
// Escrito para: Delphi 6 y 7
// Funcionamiento: Exporta una clase que implementa un formulario mediante
//                 el cual el usuario puede configurar manualmente los
//                 parámetros de funcionamiento del puerto COM.
///////////////////////////////////////////////////////////////////////////////

unit PuertoComConfiguracion;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, PuertoComControl, ActnList, Buttons;

type
  TFormConfiguracion = class(TForm)
    GroupBox1: TGroupBox;
    ComboBoxPuerto: TComboBox;
    ComboBoxVelocidad: TComboBox;
    ComboBoxCantidadDeBits: TComboBox;
    ComboBoxBitsDeParada: TComboBox;
    ComboBoxParidad: TComboBox;
    ComboBoxControlDeFlujo: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ActionList1: TActionList;
    ActionAceptar: TAction;
    ActionCancelar: TAction;
    procedure ActionAceptarExecute(Sender: TObject);
    procedure ActionCancelarExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CopiaDelObjeto: TPuertoCOM;
    constructor Crear(Objeto: TPuertoCOM); 
  public
    { Public declarations }
  end;

procedure ComfigurarPuertoCOM(Objeto: TPuertoCOM);


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
  
implementation

{$R *.dfm}


//-----------------------------------------------------------------------------
// Crea una instancia del formulario para configurar el puerto.
//-----------------------------------------------------------------------------
procedure ComfigurarPuertoCOM(Objeto: TPuertoCOM);
begin
with TFormConfiguracion.Crear(Objeto) do ShowModal;
end;

//-----------------------------------------------------------------------------
// Al crear el formulario muestra el estado actual del puerto COM.
//-----------------------------------------------------------------------------
constructor TFormConfiguracion.Crear(Objeto: TPuertoCOM);
var br: TBaudRate;
    db: TDataBits;
    sb: TStopBits;
    pr: TParityBits;
    fc: TFlowControl;
    pc: Integer;
begin
Create(nil);
if Assigned(Objeto) then           //Si el objeto existe:
   begin
   //Copia una referencia al objeto.
   CopiaDelObjeto := Objeto;

   //Si existen puertos COM disponibles, llena el ComboBox con
   //los nombres y continúa con el resto de la función.
   if PuertosComExistentes(ComboBoxPuerto.Items) > 0 then
      begin
      //Selecciona el puerto actualmente en uso.
      for pc := 1 to ComboBoxPuerto.Items.Count do
          if ComboBoxPuerto.Items[pc - 1] = CopiaDelObjeto.Puerto then
             ComboBoxPuerto.ItemIndex := pc - 1;

      //Llena el ComboBox con las valocidades disponibles
      //y selecciona el valor actualmente en uso.
      ComboBoxVelocidad.ItemIndex := Ord(br9600);
      br := br110;
      while br <= High(TBaudRate) do
            begin
            ComboBoxVelocidad.Items.Add(BaudRateToStr(br));
            if CopiaDelObjeto.Velocidad = br then
               ComboBoxVelocidad.ItemIndex := Ord(br) - 1;
            br := Succ(br);
            end;

      //Llena el ComboBox con los números de bits disponibles
      //y selecciona el valor actualmente en uso.
      ComboBoxCantidadDeBits.ItemIndex := Ord(dbOcho);
      db := Low(TDataBits);
      while db <= High(TDataBits) do
            begin
            ComboBoxCantidadDeBits.Items.Add(DataBitsToStr(db));
            if CopiaDelObjeto.CantidadDeBits = db then
               ComboBoxCantidadDeBits.ItemIndex := Ord(db);
            db := Succ(db);
            end;

      //Llena el ComboBox con los números de bits de parada
      //disponibles y selecciona el valor actualmente en uso.
      ComboBoxBitsDeParada.ItemIndex := Ord(sbDos);
      sb := Low(TStopBits);
      while sb <= High(TStopBits) do
            begin
            ComboBoxBitsDeParada.Items.Add(StopBitsToStr(sb));
            if CopiaDelObjeto.BitsDeParada = sb then
               ComboBoxBitsDeParada.ItemIndex := Ord(sb);
            sb := Succ(sb);
            end;

      //Llena el ComboBox con los tipos de paridad disponibles
      //y selecciona el valor actualmente en uso.
      ComboBoxParidad.ItemIndex := Ord(prNinguno);
      pr := Low(TParityBits);
      while pr <= High(TParityBits) do
            begin
            ComboBoxParidad.Items.Add(ParityToStr(pr));
            if CopiaDelObjeto.Paridad.Bits = pr then
               ComboBoxParidad.ItemIndex := Ord(pr);
            pr := Succ(pr);
            end;

      //Llena el ComboBox con los tipos de control de flujo 
      //disponibles y selecciona el valor actualmente en uso.
      ComboBoxControlDeFlujo.ItemIndex := Ord(fcNinguno);
      fc := Low(TFlowControl);
      while fc <= Pred(High(TFlowControl)) do
            begin
            ComboBoxControlDeFlujo.Items.Add(FlowControlToStr(fc));
            if CopiaDelObjeto.ControlDeFlujo.FlowControl = fc then
               ComboBoxControlDeFlujo.ItemIndex := Ord(fc);
            fc := Succ(fc);
            end;
      end;
   end;
end;

//-----------------------------------------------------------------------------
// Cierra el formulario y guarda la configuración establecida por el usuario.
//-----------------------------------------------------------------------------
procedure TFormConfiguracion.ActionAceptarExecute(Sender: TObject);
begin
//Guarda la congiruración establecida por el usuario.
with CopiaDelObjeto do
     begin
     with ComboBoxPuerto do Puerto := Items[ItemIndex];
     with ComboBoxVelocidad do Velocidad := StrToBaudRate(Items[ItemIndex]);
     with ComboBoxCantidadDeBits do CantidadDeBits := StrToDataBits(Items[ItemIndex]);
     with ComboBoxBitsDeParada do BitsDeParada := StrToStopBits(Items[ItemIndex]);
     with ComboBoxParidad do Paridad.Bits := StrToParity(Items[ItemIndex]);
     with ComboBoxControlDeFlujo do ControlDeFlujo.FlowControl := StrToFlowControl(Items[ItemIndex]);
     end;
Close;
end;

//-----------------------------------------------------------------------------
// Cierra el formulario sin guardar la configuración.
//-----------------------------------------------------------------------------
procedure TFormConfiguracion.ActionCancelarExecute(Sender: TObject);
begin
Close;
end;

//-----------------------------------------------------------------------------
// Pone el foco sobre el primer ComboBox.
//-----------------------------------------------------------------------------
procedure TFormConfiguracion.FormShow(Sender: TObject);
begin
ComboBoxPuerto.SetFocus;
end;

end.
