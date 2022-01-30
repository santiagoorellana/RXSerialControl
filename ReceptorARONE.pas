///////////////////////////////////////////////////////////////////////////////
// Autor: Santiago A. Orellana Pérez.
// Creado: 30/05/2013
// Escrito para: Delphi 6 y 7
// Utilización: Para controlar un receptor AOR modelo AR-ONE.
///////////////////////////////////////////////////////////////////////////////

unit ReceptorARONE;

interface

uses ReceptorBasico, PuertoComControl, SysUtils, Classes, Windows;


//-----------------------------------------------------------------------------
const CFinDeCadena                      = #13#10;      //Caracteres que marcan el final de una linea.
const CTiempoDeComprobacionDeConexcion  = 3000;        //3 segundos. (Tiempo que se espera la respuesta de un comando)
const CDemoraDeActivacion               = 10000;       //10 segundos (Lo que demora el receptor activarse)
const CDemoraDeCambioDeFrecuencia       = 500;         //0.5 segundos (Lo que demora el receptor encambiar de frecuencia).
const CPrecisionDeFrecuencia            = 6;           //Cantidad de dígitos después de la coma, en la frecuencia.
const CTiempoMinimoDeCambioDeFrecuencia = 10;
const CTiempoDeEsperaParaComandos       = 100;          //Este es un tiempo prudencial que se espera luego de enviar un comando.

const CFrecuenciaMinimaMhz              = 0.01;        //10 Khz es la Mínima frecuencia de trabajo del receptor.
const CFrecuenciaMaximaMhz              = 999.999999;  //999.999999 Mhz es la Máxima frecuencia de trabajo del receptor.


//-----------------------------------------------------------------------------
// Clase que representa un receptor tipo AR-ONE.
// Esta clase implementa una interface del tipo "IReceptor" que contiene
// las funcionalidades básicas que deberán poseer todos los receptores
//-----------------------------------------------------------------------------
type
   TReceptorARONE = class (TInterfacedObject, IReceptor)
   private
      FPuerto: TPuertoCOM;
      FActivo: Boolean;

      FControlAutomaticoDeGanancia: Byte;  //Control automático de ganancia.
      FAmplificadorDeRF: Byte;             //Amplificador de RF.
      FGananciaDe10700Khz: Byte;           //Ganancia de 10,7 Mhz.
      FGananciaDeRF: Byte;                 //Ganancia de RF.
      FGananciaDeFI: Byte;                 //Ganancia de IF.
      FNivelDelAudio: Byte;                //Nivel del audio.
      FAtenuador: Byte;                    //Activación del atenuador.
      FModo: Byte;                         //Modo de demodulación.
      FFiltroPasaAltos: Byte;              //Filtro pasa altos.
      FFiltroPasaBajos: Byte;              //Filtro pasa bajos.
      FNivelDeSennalAutomatico: Byte;      //Auto signal level.
      FLamparaDeLCD: Byte;                 //Activa o desactiva la lámpara del LCD.
      FContrasteDeLCD: Byte;               //Contraste del LCD.
      FActivarSpeaker :Byte;               //Speakers externa activa.
      FSpeakerExterna: Byte;               //Activación del Speaker externo.
      FAnchoDeBanda: Byte;                 //Ancho de banda.
      FFrecuencia: Double;                 //Frecuencia de trabajo.
      FDeEmphasis: Byte;                   //De-Emphasis.
      FNivelDelSquelch: Byte;              //Nivel del Squelch.

      procedure AbortarTodo;
      procedure EstablecerFrecuencia(Valor: Double);
      procedure EstablecerDemodulador(Valor: Byte);
      procedure EstablecerAnchoDeBanda(Valor: Byte);
      procedure EstablecerAtenuador(Valor: Byte);
      procedure EstablecerAmplificadorRF(Valor: Byte);
      procedure EstablecerAGC(Valor: Byte);
      procedure EstablecerGananciaDeRF(Valor: Byte);
      procedure EstablecerGananciaDe10700Khz(Valor: Byte);
      procedure EstablecerGananciaDeFI(Valor: Byte);
      procedure EstablecerFiltroPasaAltos(Valor: Byte);
      procedure EstablecerFiltroPasaBajos(Valor: Byte);
      procedure EstablecerActivacionDeBocina(Valor: Byte);
      procedure EstablecerLamparaDePantalla(Valor: Byte);
      procedure EstablecerContrasteDePantalla(Valor: Byte);
      procedure EstablecerNivelDelAudio(Valor: Byte);
      procedure EstablecerDeEmphasis(Valor: Byte);
      procedure EstablecerNivelDeSquelch(Valor: Byte);

      function ComprobarAccion: Boolean;
   public
      property Frecuencia: Double read FFrecuencia write EstablecerFrecuencia;
      property Demodulador: Byte read FModo write EstablecerDemodulador;
      property AnchoDebanda: Byte read FAnchoDebanda write EstablecerAnchoDeBanda;
      property Atenuador: Byte read FAtenuador write EstablecerAtenuador;
      property AmplificadorDeRF: Byte read FAmplificadorDeRF write EstablecerAmplificadorRF;
      property ControlAutomaticoDeGanancia: Byte read FControlAutomaticoDeGanancia write EstablecerAGC;
      property GananciaDeRF: Byte read FGananciaDeRF write EstablecerGananciaDeRF;
      property GananciaDe10700Khz: Byte read FGananciaDe10700Khz write EstablecerGananciaDe10700Khz;
      property GananciaDeFI: Byte read FGananciaDeFI write EstablecerGananciaDeFI;
      property FiltroPasaAltos: Byte read FFiltroPasaAltos write EstablecerFiltroPasaAltos;
      property FiltroPasaBajos: Byte read FFiltroPasaBajos write EstablecerFiltroPasaBajos;
      property BocinaActivada: Byte read FSpeakerExterna write EstablecerActivacionDeBocina;
      property LamparaDelLCD: Byte read FLamparaDeLCD write EstablecerLamparaDePantalla;
      property ContrasteDelLCD: Byte read FContrasteDeLCD write EstablecerContrasteDePantalla;
      property NivelDelAudio: Byte read FNivelDelAudio write EstablecerNivelDelAudio;
      property DeEmphasis: Byte read FDeEmphasis write EstablecerDeEmphasis;
      property Squelch: Byte read FNivelDelSquelch write EstablecerNivelDeSquelch;

      //Métodos propios del tipo de receptor.
      constructor Create(PuetoCom: Integer);
      procedure ConfiguracionOriginalDelPuerto;
      procedure ConfiguracionOriginalDelReceptor;
      procedure Enviar(Comando: String);
      function Recibir: String;
      function EnviarYComprobarRespuesta(Comando: String; Espera: Integer): Boolean;
      function DeFrecuenciaACadena(FrecuenciaMhz: Double): String;


      //Métodos de la interface básica del receptor.
      procedure ConfigurarPuerto;
      function Activar: Boolean;
      function Inicializar: Boolean;
      function FrecuenciaMinima: Double;                          //Devuelve la mínima frecuencia de trabajo.
      function FrecuenciaMaxima: Double;                          //Devuelve la frecuencia máxima de trabajo.
      function FrecuenciaValida(FrecuenciaMhz: Double): Boolean;  //Devuelve TRUE si es posible establecer la frecuencia.
      function TiempoMinimoDeCambioDeFrecuencia: Integer;
      function RecibirLaFrecuencia(Mhz: Double): Boolean;
      procedure ConfiguracionAvanzada;
      procedure Apagar;
      procedure Eliminar;
   end;


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

implementation

uses StrUtils, ReceptorARONEConfiguracion, Forms;

//-----------------------------------------------------------------------------
// Este es el constructor de la clase.
//-----------------------------------------------------------------------------
constructor TReceptorARONE.Create(PuetoCom: Integer);
begin
try
   FPuerto := TPuertoCOM.Crear;                        //Crea el objeto para acceder al puerto serie.
   FPuerto.Puerto := 'COM' + IntToStr(PuetoCom + 1);   //Indica el puerto COM a utilizar.
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Este es el destructor de la clase.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.Eliminar;
begin
try
   FPuerto.EnviarCadena('QP');   //Apaga el receptor.
   FPuerto.Cerrar;               //Detiene todas las operaciones y libera el puerto.
   FPuerto.Free;                 //Destruye el objeto de interacción con el puerto.
except
   //nada
end;
inherited Destroy;            //Llama al destructor de la clase paterna.
end;

//-----------------------------------------------------------------------------
// Abre el diálogo de configuración del puerto serie.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.ConfigurarPuerto;
begin
try
   FPuerto.MostrarDialogoDeConfiguracion;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Intenta activar el receptor.
// La salida de la función solo se produce cuando la operación
// concluya, y los resultados serán devueltos a la salida.
//
// Salida:
// Si el receptor se activa, devuelve TRUE. Si el receptor está desconectado
// o no se puede activar por alguna otra razón, devuelve FALSE.
//-----------------------------------------------------------------------------
function TReceptorARONE.Activar: Boolean;
var Inicio: Cardinal;
begin
try
   Result := False;                                 //Si no se puede activar, devuelve FALSE.
   FActivo := False;                                //Por el momento el receptor no está activo.
   FPuerto.Abrir;                                   //Abre el puerto serie.
   if FPuerto.Abierto then                          //Si el puerto está conectado:
      begin
      ConfiguracionOriginalDelPuerto;               //Establece los parámetros de funcionamiento del puerto.

      //Si el receptor no esta activado, lo activa.
      AbortarTodo;                                  //Limpia los buffers y detiene las operaciones anteriores.
      Inicio := GetTickCount;                       //Obtiene el tiempo de inicio.
      while True do                                 //Comienza a mandarle mensajes al receptor recursivamente.
            begin
            FPuerto.EnviarCadena('RF');             //El mensaje dice: ¿Dime en que frecuencia estas?.
            if FPuerto.CaracteresRecibidos > 0 then //Si el receptor responde, se asume que este esta activado
               begin                                //o que se activó con la cadena de texto que se le mandó.
               Result := True;                      //Devuelve TRUE porque el receptor esta activado y conectado.
               FActivo := True;                     //Indica que el receptor está activo.
               Break;                               //Sale del ciclo de comprobación y de la función.
               end;
            if Abs(GetTickCount - Inicio) > CDemoraDeActivacion then Break;
            end;
      AbortarTodo;                                  //Limpia los buffers y detiene las operaciones anteriores.
      end;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Establece la configuración inicial de los parámetros del receptor.
//-----------------------------------------------------------------------------
function TReceptorARONE.Inicializar: Boolean;
begin
//Realiza una espera para darle tiempo al receptor.
Sleep(100);

//Detiene todas las operaciones pendientes.
AbortarTodo;

//Establece una configuración por defecto para el receptor.
ConfiguracionOriginalDelReceptor;

//Envía los comandos que configuran al receptor.
//No se debe variar el orden en que se introducen, ya
//que pueden existir dependencias entre estos.
Enviar('ID00');                                         //Le asigna el ID por defecto al receptor.
  //Enviar('^A0');                                         //Toma el control del receptor desactivando el panel frontal.
Enviar('GA0');                                          //Memoria = Desactivado.
Enviar('LA1');                                          //Auto backlit = Activado.
Enviar('LD0');                                          //Backlit dimmer = Normal.
Enviar('VA');                                           //Modo VFO = Activado.
Enviar('SV');                                           //VFO A = Activado.
Enviar('AU0');                                          //Modo automático = Desactivado
Enviar('AC' + IntToStr(FControlAutomaticoDeGanancia));  //Control automático de ganancia = Desactivado.
Enviar('AM' + IntToStr(FAmplificadorDeRF));             //Amplificador de RF.
Enviar('BF0');                                          //Frecuencia de batido = Desactivado.
Enviar('MG' + IntToStr(FGananciaDe10700Khz));           //Ganancia de 10,7 Mhz.
Enviar('RG' + IntToStr(FGananciaDeRF));                 //Ganancia de RF.
Enviar('IG' + IntToStr(FGananciaDeFI));                 //Ganancia de IF.
Enviar('AT' + IntToStr(FAtenuador));                    //Atenuador.
Enviar('MD' + IntToStr(FModo));                         //Establece el modo.
Enviar('EN5');                                          //Desactivado.

if FFiltroPasaAltos <> 255 then
   Enviar('HP' + IntToStr(FFiltroPasaAltos))            //Selección del filtro pasa altos.
else
   Enviar('HPF');                                       //Filtro pasa altos automático.

if FFiltroPasaBajos <> 255 then
   Enviar('LP' + IntToStr(FFiltroPasaBajos))            //Selección del filtro pasa bajos.
else
   Enviar('LPF');                                       //Filtro pasa bajos automático.

Enviar('LC0');                                          //Auto signal level.
Enviar('SQ0');                                          //Tipo de Squelch.
Enviar('RQ' + IntToStr(FNivelDelSquelch));              //Establece el nivel del Squelch.
Enviar('SQ0');                                          //Squelch por umbral de ruido.
Enviar('LA' + IntToStr(FLamparaDeLCD));                 //Activa o desactiva la lámpara del LCD.
Enviar('LV' + IntToStr(FContrasteDeLCD));               //Contraste del LCD.
Enviar('BL5');                                          //Nivel del Beep del speaker interno.
Enviar('SO' + IntToStr(FActivarSpeaker));               //Speaker activa.
Enviar('PO' + IntToStr(FSpeakerExterna));               //Speaker externa.
Enviar('AG' + IntToStr(FNivelDelAudio));                //Nivel de audio.
Enviar('OF00');                                         //Modo duplex = Desactivado.
Enviar('BW' + IntToStr(FAnchoDeBanda));                 //Selecciona un ancho de banda.
Enviar(DeFrecuenciaACadena(FFrecuencia));               //Establece una frecuencia inicial.

Sleep(100);                                             //Realiza una pausa necesaria.
Result := ComprobarAccion;                              //Devuelve TRUE si el receptor ha recibido comandos.
AbortarTodo;
end;

//-----------------------------------------------------------------------------
// Devuelve la frecuencia mínima que puede sintonizar el receptor.
//-----------------------------------------------------------------------------
function TReceptorARONE.FrecuenciaMinima: Double;
begin
Result := CFrecuenciaMinimaMhz;
end;

//-----------------------------------------------------------------------------
// Devuelve la frecuencia máxima que puede sintonizar el receptor.
//-----------------------------------------------------------------------------
function TReceptorARONE.FrecuenciaMaxima: Double;
begin
Result := CFrecuenciaMaximaMhz;
end;

//-----------------------------------------------------------------------------
// Devuelve TRUE si el receptor puede sintonizar la frecuencia.
//-----------------------------------------------------------------------------
function TReceptorARONE.FrecuenciaValida(FrecuenciaMhz: Double): Boolean;
begin
Result := (FrecuenciaMhz >= CFrecuenciaMinimaMhz) and
          (FrecuenciaMhz <= CFrecuenciaMaximaMhz);
end;

//-----------------------------------------------------------------------------
// Devuelve la cantidad de milisegundos que se debe esperar entre
// el establecimiento de una frecuencia y la otra.
// Este valor se emplea como intervalo mínimo de espera
// para cuando se está explorando a grandes velocidades.
//-----------------------------------------------------------------------------
function TReceptorARONE.TiempoMinimoDeCambioDeFrecuencia: Integer;
begin
Result := CTiempoMinimoDeCambioDeFrecuencia;
end;

//-----------------------------------------------------------------------------
// Esta función hace que el receptor sintonice la frecuencia indicada.
// La salida de la función solo se produce cuando la operación
// concluya, y el resultado será devuelto a la salida.
//
// Entrada:
// FrecuenciaMhz = Frecuencia en Mega Herts.
//
// Salida:
// Devuelve TRUE si el receptor ha respondido.
//-----------------------------------------------------------------------------
function TReceptorARONE.RecibirLaFrecuencia(Mhz: Double): Boolean;
var Valor: String;
begin
AbortarTodo;                                                               //Elimina los restos de comunicaciones anteriores.
Valor := DeFrecuenciaACadena(Mhz);                                         //Crea el comando para el cambio de frecuencia.
Result := EnviarYComprobarRespuesta(Valor, CDemoraDeCambioDeFrecuencia);   //Envía el comando al receptor y si este responde:
if Result then FFrecuencia := Mhz;                                         //Devuelve TRUE y asume que se ha cambiado la frecuencia.
end;

//-----------------------------------------------------------------------------
// Abre un diálogo que permite al usuario configurar el receptor.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.ConfiguracionAvanzada;
begin
//if FActivo then
   with TFormReceptorARONEConfiguracion.Crear(Self) do ShowModal;
end;

//-----------------------------------------------------------------------------
// Esta función apaga el receptor.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.Apagar;
begin
try
   FPuerto.EnviarCadena('QP');     //Envía el comando de apagar el receptor.
except
   //Nada
end;
end;

///////////////////////////////////////////////////////////////////////////////
///////////////    Estas son las funciones internas     ///////////////////////
///////////////////////////////////////////////////////////////////////////////

//-----------------------------------------------------------------------------
// Establece los parámetros de funcionamiento del puerto serie COM
// necesarios para interactuar con este tipo de receptor.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.ConfiguracionOriginalDelPuerto;
begin
try
   FPuerto.Paridad.Bits := prNinguno;                //Establece los bits de paridad.
   FPuerto.Velocidad := br9600;                      //Establece la velocidad de la comunicación.
   FPuerto.BitsDeParada := sbDos;                    //Cantidad de bits de parada.
   FPuerto.ControlDeFlujo.FlowControl := fcNinguno;  //Control de flujo que se utiliza.
   FPuerto.CantidadDeBits := dbOcho;                 //Se utilizan ocho.
   FPuerto.FinDeCadena := CFinDeCadena;              //Estos son los caracteres que marcan el fin de una linea.
   FPuerto.LimpiarBuffer(True, True);                //Limpia los buffers.
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Establece la configuración por defecto del receptor.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.ConfiguracionOriginalDelReceptor;
begin
FControlAutomaticoDeGanancia := 0;     //Control automático de ganancia = Desactivado.
FAmplificadorDeRF := 1;                //Amplificador de RF = Activado.
FNivelDelAudio := 50;                  //Nivel de audio = 50.
FNivelDelSquelch := 0;                 //Establece el nivel del Squelch.
FGananciaDe10700Khz := 200;            //Ganancia de 10,7 Mhz = 200.
FGananciaDeRF := 200;                  //Ganancia de RF = 200.
FGananciaDeFI := 200;                  //Ganancia de IF = 200.
FAtenuador := 0;                       //Atenuador = Desactivado.
FModo := 4;                            //Establece el modo LSB.
FFiltroPasaAltos := 0;                 //Filtro pasa altos = 50 Hz.
FFiltroPasaBajos := 3;                 //Filtro pasa bajos = 12 Khz.
FNivelDeSennalAutomatico := 0;         //Auto signal level = Desactivado.
FLamparaDeLCD := 1;                    //Por defecto se activa la lámpara del LCD.
FContrasteDeLCD := 12;                 //Contraste del LCD = 12.
FActivarSpeaker := 0;                  //Speaker activada.
FSpeakerExterna := 0;                  //Speakers seleccionadas.
FAnchoDeBanda := 7;                    //Selecciona un ancho de banda de 200 Khz.
FFrecuencia := 14.0;                   //Establece una frecuencia inicial.
end;

//-----------------------------------------------------------------------------
// Detiene las operaciones que este realizando el puerto.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.AbortarTodo;
begin
try
   FPuerto.AbortarTodasLasOperacionesAsincronicas;   //Detiene las oparaciones asincrónicas.
   FPuerto.LimpiarBuffer(True, True);                //Borra los buffers de entrada y salida.
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia una cadena que contiene uno o varios comandos para el receptor.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.Enviar(Comando: String);
begin
try
   if FPuerto.Abierto then                           //Si el puerto esta abierto:
      begin
      AbortarTodo;                                   //Aborta todas las operaciones.
      FPuerto.EnviarCadena(Comando);                 //Envía el comando.
      end;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Recibe una cadena desde el receptor.
//-----------------------------------------------------------------------------
function TReceptorARONE.Recibir: String;
begin
try
   if FPuerto.Abierto then                                          //Si el puerto esta abierto:
      FPuerto.RecibirCadena(Result, FPuerto.CaracteresRecibidos);   //lee una cadena desde el puerto.
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Esta función envía una cadena al receptor y espera durante un tiempo
// la respuesta del receptor. Se asume que el receptor conectado es un AR-ONE.
//
// Entrada:
// Comando = Cadena de texto que contiene el comando que se desea enviar.
// Espera  = Tiempo de espera de la respuesta en milisegundos.
//
// Salida:
// Devuelve TRUE si el receptor responde antes de que se termine el
// tiempo de espera. De lo contrario, devuelve FALSE.
//-----------------------------------------------------------------------------
function TReceptorARONE.EnviarYComprobarRespuesta(Comando: String; Espera: Integer): Boolean;
var Inicio: Cardinal;                                       //Guarda el tiempo de inicio.
begin
Result := False;
try
   if FPuerto.Abierto then                                     //Si el puerto esta abierto:
      begin
      AbortarTodo;                                             //Termina todas las operaciones y limpia los buffers.
      FPuerto.EnviarCadena(Comando);                           //Envía el comando.
      Inicio := GetTickCount;                                  //Obtiene el tiempo de inicio.
      while true do                                            //Espera durante un tiempo la respuesta del receptor.
            begin
            if FPuerto.CaracteresRecibidos > 0 then            //Si el receptor responde:
               begin
               Result := True;                                 //Devuelve TRUE a la salida.
               Break;                                          //Sale del ciclo de espera.
               end;
            if Abs(GetTickCount - Inicio) > Espera then break; //Si se ha agotado el tiempo de espera, sale de la espera.
            end;
      AbortarTodo;                                             //Termina todas las operaciones y limpia los buffers.
      end;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Devuelve la frecuencia como una cadena de texto. La cadena se devuelve
// lista para ser enviada al receptor para establecer dicha frecuencia.
//-----------------------------------------------------------------------------
function TReceptorARONE.DeFrecuenciaACadena(FrecuenciaMhz: Double): String;
begin
Result := 'RF' + FloatToStrF(FrecuenciaMhz, ffNumber, 15, CPrecisionDeFrecuencia);  //Crea la frecuencia.
Result := AnsiReplaceStr(Result, ',', '.');                                         //Reemplaza la coma por un punto.
end;

//-----------------------------------------------------------------------------
// Comprueba si el receptor ha respondido.
// Esta función debe ser llamada exclusivamente después
// de mandar un comando al receptor, para verificar que
// este halla recibido el comando.
//-----------------------------------------------------------------------------
function TReceptorARONE.ComprobarAccion: Boolean;
var Inicio: Cardinal;
    msg: String;
begin
try
   Result := False;
   Inicio := GetTickCount;                            //Obtiene el tiempo de inicio.
   while True do                                      //Comienza a mandarle mensajes al receptor recursivamente.
         begin
         if FPuerto.CaracteresRecibidos > 0 then      //Si el receptor responde, se asume que este esta activado
            begin                                     //o que se activó con la cadena de texto que se le mandó.
            Result := True;                           //Devuelve TRUE porque el receptor esta activado y conectado.
            Break;                                    //Sale del ciclo de comprobación y de la función.
            end;
         if Abs(GetTickCount - Inicio) > CTiempoDeComprobacionDeConexcion then Break;
         end;
   if not Result then
      begin
      msg := 'El receptor no responde.' + #13;
      msg := msg + 'Compruebe la conección del puerto COM.';
      MessageBox(0, PChar(msg), 'Atención', MB_ICONEXCLAMATION);
      end;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Establece la frecuencia de recepción.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.EstablecerFrecuencia(Valor: Double);
begin
RecibirLaFrecuencia(Valor);
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el tipo de demodulador.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.EstablecerDemodulador(Valor: Byte);
begin
try
   AbortarTodo;
   FPuerto.EnviarCadena('MD' + IntToStr(Valor));    //Cambia el tipo de demodulador.
   if ComprobarAccion then                          //Si el receptor responde:
      begin
      FModo := Valor;                               //Registra en una variable el tipo
      AnchoDebanda := FAnchoDeBanda;                //de demodulador activo.
      FiltroPasaAltos := FFiltroPasaAltos;          //Devido a que el cambio de demodulador
      FiltroPasaBajos := FFiltroPasaBajos;          //modifica al ancho de banda y los filtros de
      end;                                          //audio, hay que establecerlos nuevamente.
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el ancho de banda.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.EstablecerAnchoDeBanda(Valor: Byte);
begin
try
   AbortarTodo;
   FPuerto.EnviarCadena('BW' + IntToStr(Valor));
   if ComprobarAccion then FAnchoDeBanda := Valor;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el atenuador.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.EstablecerAtenuador(Valor: Byte);
begin
try
   AbortarTodo;
   FPuerto.EnviarCadena('AT' + IntToStr(Valor));
   if ComprobarAccion then FAtenuador := Valor;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el amplificador de RF.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.EstablecerAmplificadorRF(Valor: Byte);
begin
try
   AbortarTodo;
   FPuerto.EnviarCadena('AM' + IntToStr(Valor));
   if ComprobarAccion then FAmplificadorDeRF := Valor;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el AGC.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.EstablecerAGC(Valor: Byte);
begin
try
   AbortarTodo;
   FPuerto.EnviarCadena('AC' + IntToStr(Valor));
   if ComprobarAccion then FControlAutomaticoDeGanancia := Valor;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el Ganancia de RF.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.EstablecerGananciaDeRF(Valor: Byte);
begin
try
   AbortarTodo;
   FPuerto.EnviarCadena('RG' + IntToStr(Valor));
   if ComprobarAccion then FGananciaDeRF := Valor;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el Ganancia de 10.7 Mhz.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.EstablecerGananciaDe10700Khz(Valor: Byte);
begin
try
   AbortarTodo;
   FPuerto.EnviarCadena('MG' + IntToStr(Valor));
   if ComprobarAccion then FGananciaDe10700Khz := Valor;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el Ganancia de FI.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.EstablecerGananciaDeFI(Valor: Byte);
begin
try
   AbortarTodo;
   FPuerto.EnviarCadena('IG' + IntToStr(Valor));
   if ComprobarAccion then FGananciaDeFI := Valor;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el Filtro Pasa Altos.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.EstablecerFiltroPasaAltos(Valor: Byte);
begin
try
   AbortarTodo;
   if Valor <> 255 then
      FPuerto.EnviarCadena('HP' + IntToStr(Valor))
   else
      FPuerto.EnviarCadena('HPF');
   if ComprobarAccion then FFiltroPasaAltos := Valor;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el Filtro Pasa Bajos.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.EstablecerFiltroPasaBajos(Valor: Byte);
begin
try
   AbortarTodo;
   if Valor <> 255 then
      FPuerto.EnviarCadena('LP' + IntToStr(Valor))
   else
      FPuerto.EnviarCadena('LPF');
   if ComprobarAccion then FFiltroPasaBajos := Valor;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que active o desactive la bocina externa.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.EstablecerActivacionDeBocina(Valor: Byte);
begin
try
   AbortarTodo;
   FPuerto.EnviarCadena('SO' + IntToStr(Valor));
   if ComprobarAccion then FSpeakerExterna := Valor;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que active o desactive la lámpara.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.EstablecerLamparaDePantalla(Valor: Byte);
begin
try
   AbortarTodo;
   FPuerto.EnviarCadena('LA' + IntToStr(Valor));
   if ComprobarAccion then FLamparaDeLCD := Valor;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que active o desactive la bocina externa.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.EstablecerContrasteDePantalla(Valor: Byte);
begin
try
   AbortarTodo;
   FPuerto.EnviarCadena('LV' + IntToStr(Valor));
   if ComprobarAccion then FContrasteDeLCD := Valor;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que establecer la antena.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.EstablecerDeEmphasis(Valor: Byte);
begin
try
   AbortarTodo;
   FPuerto.EnviarCadena('EN' + IntToStr(Valor));
   if ComprobarAccion then FDeEmphasis := Valor;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que camboie el nivel del audio.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.EstablecerNivelDelAudio(Valor: Byte);
begin
try
   AbortarTodo;                                        //Detiene las operaciones anteriores.
   FPuerto.EnviarCadena('AG' + IntToStr(Valor));       //Manda el valor del Volumen.
   if ComprobarAccion then FNivelDelAudio := Valor;    //Cambia el nivel del Volumen.
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambiar el nivel del Squelch.
//-----------------------------------------------------------------------------
procedure TReceptorARONE.EstablecerNivelDeSquelch(Valor: Byte);
begin
try
   AbortarTodo;                                        //Detiene las operaciones anteriores.
   FPuerto.EnviarCadena('SQ0');                        //Utiliza el Squelch de ruido.
   FPuerto.EnviarCadena('RQ' + IntToStr(Valor));       //Manda el valor del Squelch.
   if ComprobarAccion then FNivelDelSquelch := Valor;  //Cambia el nivel del Squelch.
except
   //Nada
end;
end;

end.
