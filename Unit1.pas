///////////////////////////////////////////////////////////////////////////////
// Autor: Santiago A. Orellana Pérez.
// Creado: 12/06/2013
// Escrito para: Delphi 6 y 7
// Utilización: Es un formulario que permite controlar las funciones básicas de
// un receptor mediante puerto serie. Permite seleccionar el tipo de receptor.
///////////////////////////////////////////////////////////////////////////////

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, PuertoComControl, ReceptorBasico, ReceptorARONE,
  ReceptorAR5000A, ReceptorICR75, ReceptorICR8500, ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button3: TButton;
    Button5: TButton;
    Button6: TButton;
    ComboBoxRX: TComboBox;
    ComboBoxSerial: TComboBox;
    Button2: TButton;
    Button7: TButton;
    Image1: TImage;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure ComboBoxRXChange(Sender: TObject);
  private
    Receptor: IReceptor;
  public
    { Public declarations }
  end;

var Form1: TForm1;


implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
if Receptor.Activar then
   Receptor.Inicializar
else
   ShowMessage('No se pudo activar el receptor');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
if Assigned(Receptor) then Receptor.Eliminar;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
if Assigned(Receptor) then Receptor.RecibirLaFrecuencia(StrToFloat(Edit1.Text));
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
if Assigned(Receptor) then Receptor.Inicializar;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
   Receptor.ConfiguracionAvanzada;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
ComboBoxRX.ItemIndex := 0;
ComboBoxRXChange(Sender);
ComboBoxSerial.ItemIndex := 0;
end;

// Esta función hay que mejorarla para que el escaneo de frecuencias
// se haga de asíncrona y que se pueda detener, pausar, etc.
// Además, es importante que se pueda establecer listas de frecuencias para escanear.
procedure TForm1.Button2Click(Sender: TObject);
const Max = 30.0;
var Frec: Double;
    IntervaloMinimo: Integer;
begin
Frec := 1;
IntervaloMinimo := Receptor.TiempoMinimoDeCambioDeFrecuencia;
if Assigned(Receptor) then
   while Frec < Max do
         begin
         Receptor.RecibirLaFrecuencia(Frec);
         Application.ProcessMessages;
         Frec := Frec + 0.01;
         Sleep(IntervaloMinimo);
         end;

end;

procedure TForm1.Button7Click(Sender: TObject);
begin
if Assigned(Receptor) then Receptor.ConfigurarPuerto;
end;

procedure TForm1.ComboBoxRXChange(Sender: TObject);
begin
if ComboBoxRX.ItemIndex >= 0 then
   try
      if Assigned(Receptor) then Receptor.Eliminar;
      Image1.Picture.LoadFromFile('./images/' + ComboBoxRX.Text + '.bmp');
      case ComboBoxRX.ItemIndex of
           0: Receptor := TReceptorARONE.Create(ComboBoxSerial.ItemIndex);
           1: Receptor := TReceptorAR5000A.Create(ComboBoxSerial.ItemIndex);
           2: Receptor := TReceptorICR75.Create(ComboBoxSerial.ItemIndex);
           3: Receptor := TReceptorICR8500.Create(ComboBoxSerial.ItemIndex);
           else Exit;
           end;
   except
      //Nada
   end;
end;

end.
