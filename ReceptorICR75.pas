///////////////////////////////////////////////////////////////////////////////
// Autor: Santiago A. Orellana Pérez.
// Creado: 12/06/2013
// Escrito para: Delphi 6 y 7
// Utilización: Para controlar un receptor ICOM modelo IC-R75.
///////////////////////////////////////////////////////////////////////////////

unit ReceptorICR75;

interface

uses ReceptorBasico, PuertoComControl, SysUtils, Classes, Windows;

//-----------------------------------------------------------------------------
const CTiempoDeRespuesta                = 3000;              //3 segundos. (Tiempo que se espera la respuesta de un comando)
const CTiempoDeActivacion               = 10000;             //10 segundos (Lo que demora el receptor activarse)
const CTiempoDeActivacionObligado       = 3000;              //3 segundos de espera obligatoria.
const CPrecisionDeFrecuencia            = 6;                 //Cantidad de dígitos después de la coma, en la frecuencia.
const CTiempoMinimoDeCambioDeFrecuencia = 10;

const CFrecuenciaMinimaMhz              = 0.03;              //30 Khz es la Mínima frecuencia de trabajo del receptor.
const CFrecuenciaMaximaMhz              = 60;                //60 Mhz es la Máxima frecuencia de trabajo del receptor.

const CPreambulo                        = String(#254#254);  //0xFEFE es el preámbulo del comando.
const CControlador                      = Chr(224);          //0xE0 es la dirección por defecto del controlador.
const CReceptor                         = Chr($5A);          //0x5A es la dirección por defecto del receptor.
const CFinal                            = Chr(253);          //0xFD marca el final del comando.

//Modos de demodulación.
const mLSB  = $0001;
const mUSB  = $0101;
const mAM   = $0202;
const mAM_W = $0200;
const mCW   = $0302;
const mCW_W = $0301;
const mFM   = $0502;
const mFM_W = $0501;

//-----------------------------------------------------------------------------
// Clase que representa un receptor tipo IC-R75.
// Esta clase implementa una interface del tipo "IReceptor" que contiene
// las funcionalidades básicas que deberán poseer todos los receptores
//-----------------------------------------------------------------------------
type
   TReceptorICR75 = class (TInterfacedObject, IReceptor)
   private
      FPuerto: TPuertoCOM;
      FActivo: Boolean;

      FControlAutomaticoDeGanancia: Byte;  //Control automático de ganancia.
      FNivelDelAudio: Byte;                //Nivel del audio.
      FNivelDelSquelch: Byte;              //Nivel del Squelch.
      FNivelDeRF: Byte;                    //Nivel de RF.
      FAntena: Byte;                       //Antena.
      FAmplificadorRF: Byte;               //Amplificador de RF.
      FAtenuador: Byte;                    //Activación del atenuador.
      FModo: Word;                         //Modo de demodulación y ancho de banda.
      FTwinPbtInterior: Byte;              //TWIN PBT Interior
      FTwinPbtExterior: Byte;              //TWIN PBT Exterior
      FFrecuencia: Double;                 //Frecuencia de trabajo.
      FDeEmphasis: Byte;                   //De-Emphasis.
      FIluminacionDelLCD: Byte;            //Nivel iluminación del LCD.

      procedure AbortarTodo;
      procedure EstablecerAGC(Valor: Byte);
      procedure EstablecerNivelDelAudio(Valor: Byte);
      procedure EstablecerNivelDelSquelch(Valor: Byte);
      procedure EstablecerNivelDelRF(Valor: Byte);
      procedure EstablecerAntena(Valor: Byte);
      procedure EstablecerAmplificadorRF(Valor: Byte);
      procedure EstablecerAtenuador(Valor: Byte);
      procedure EstablecerDemodulador(Valor: Word);
      procedure EstablecerTwinPbtInterior(Valor: Byte);
      procedure EstablecerTwinPbtExterior(Valor: Byte);
      procedure EstablecerFrecuencia(Valor: Double);
      procedure EstablecerIluminacionDelLCD(Valor: Byte);

      function ComprobarAccion: Boolean;

      function IntegerToBCD(Value: Int64): Int64;
      procedure Enviar(Comando: String); overload;
      procedure Enviar(Comando: Byte; Subcomando: Byte); overload;
      procedure EnviarB(Comando: Byte; Subcomando: Byte; ValorBCD: Byte);
      procedure EnviarW(Comando: Byte; Subcomando: Byte; ValorBCD: Word);
      procedure EnviarBB(Comando: Byte; Subcomando: Byte; No: Byte; ValorBCD: Byte);
      procedure EnviarBW(Comando: Byte; Subcomando: Byte; No: Byte; ValorBCD: Word);
      function Recibir: String;
   public
      property ControlAutomaticoDeGanancia: Byte read FControlAutomaticoDeGanancia write EstablecerAGC;
      property NivelDelAudio: Byte read FNivelDelAudio write EstablecerNivelDelAudio;
      property NivelDelSquelch: Byte read FNivelDelSquelch write EstablecerNivelDelSquelch;
      property NivelDelRF: Byte read FNivelDeRF write EstablecerNivelDelRF;
      property Antena: Byte read FAntena write EstablecerAntena;
      property AmplificadorRF: Byte read FAmplificadorRF write EstablecerAmplificadorRF;
      property Atenuador: Byte read FAtenuador write EstablecerAtenuador;
      property Modo: Word read FModo write EstablecerDemodulador;
      property TwinPbtInterior: Byte read FTwinPbtInterior write EstablecerTwinPbtInterior;
      property TwinPbtExterior: Byte read FTwinPbtExterior write EstablecerTwinPbtExterior;
      property Frecuencia: Double read FFrecuencia write EstablecerFrecuencia;
      property IluminacionDelLCD: Byte read FIluminacionDelLCD write EstablecerIluminacionDelLCD;

      //Métodos propios del tipo de receptor.
      constructor Create(PuetoCom: Integer);
      procedure ConfiguracionOriginalDelPuerto;
      procedure ConfiguracionOriginalDelReceptor;

      //Métodos de la interface básica del receptor.
      procedure ConfigurarPuerto;
      function Activar: Boolean;
      function Inicializar: Boolean;
      function FrecuenciaMinima: Double;                          //Devuelve la mínima frecuencia de trabajo.
      function FrecuenciaMaxima: Double;                          //Devuelve la frecuencia máxima de trabajo.
      function FrecuenciaValida(FrecuenciaMhz: Double): Boolean;  //Devuelve TRUE si es posible establecer la frecuencia.
      function TiempoMinimoDeCambioDeFrecuencia: Integer;         //Es el tiempo de espera en milisegundos, para cambiar d euna frecuencia a otra.
      function RecibirLaFrecuencia(Mhz: Double): Boolean;
      procedure ConfiguracionAvanzada;
      procedure Apagar;
      procedure Eliminar;
   end;


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

implementation

uses StrUtils, Forms, ReceptorICR75Configuracion;

//-----------------------------------------------------------------------------
// Convierte de Integer a BCD.
//-----------------------------------------------------------------------------
function TReceptorICR75.IntegerToBCD(Value: Int64): Int64;
var Divisor: Integer;
begin
Result := 0;
Divisor := 1000000000;
while Divisor > 0 do
      begin
      Result := (Result shl 4) or Value div Divisor mod 10;
      Divisor := Divisor div 10;
      end;
end;

//-----------------------------------------------------------------------------
// Envía un comando simple al receptor.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.Enviar(Comando: Byte; Subcomando: Byte);
begin
Enviar(CPreambulo + CReceptor + CControlador + Chr(Comando) + Chr(Subcomando) + CFinal);
end;

//-----------------------------------------------------------------------------
// Envía al receptor un comando con un dato tipo Byte.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.EnviarB(Comando: Byte; Subcomando: Byte; ValorBCD: Byte);
var Bajo: Byte;
begin
Bajo := IntegerToBCD(ValorBCD) and $FF;
Enviar(CPreambulo + CReceptor + CControlador + Chr(Comando) + Chr(Subcomando) + Chr(Bajo) + CFinal);
end;

//-----------------------------------------------------------------------------
// Envía al receptor un comando con un dato tipo Word.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.EnviarW(Comando: Byte; Subcomando: Byte; ValorBCD: Word);
var Alto, Bajo: Byte;
    BCD: Word;
begin
BCD := IntegerToBCD(ValorBCD);
Alto := BCD shr 8;
Bajo := BCD and $FF;
Enviar(CPreambulo + CReceptor + CControlador + Chr(Comando) + Chr(Subcomando) + Chr(Alto) + Chr(Bajo) + CFinal);
end;

//-----------------------------------------------------------------------------
// Envía al receptor un comando con dos datos tipo Byte.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.EnviarBB(Comando: Byte; Subcomando: Byte; No: Byte; ValorBCD: Byte);
var Bajo: Byte;
begin
Bajo := IntegerToBCD(ValorBCD) and $FF;
Enviar(CPreambulo + CReceptor + CControlador + Chr(Comando) + Chr(Subcomando) + Chr(No) + Chr(Bajo) + CFinal);
end;

//-----------------------------------------------------------------------------
// Envía al receptor un comando con dos datos tipo Byte y Word.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.EnviarBW(Comando: Byte; Subcomando: Byte; No: Byte; ValorBCD: Word);
var Alto, Bajo: Byte;
    BCD: Word;
begin
BCD := IntegerToBCD(ValorBCD);
Alto := BCD shr 8;
Bajo := BCD and $FF;
Enviar(CPreambulo + CReceptor + CControlador + Chr(Comando) + Chr(Subcomando) + Chr(No) + Chr(Alto) + Chr(Bajo) + CFinal);
end;


//-----------------------------------------------------------------------------
// Este es el constructor de la clase.
//-----------------------------------------------------------------------------
constructor TReceptorICR75.Create(PuetoCom: Integer);
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
procedure TReceptorICR75.Eliminar;
begin
try
   Enviar($18, $00);             //Desactiva el receptor.
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
procedure TReceptorICR75.ConfigurarPuerto;
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
function TReceptorICR75.Activar: Boolean;
var Inicio: Cardinal;
begin
Result := False;                                 //Si no se puede activar, devuelve FALSE.
try
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
            Enviar($18, $01);                       //Activa el receptor.
            if FPuerto.CaracteresRecibidos > 0 then //Si el receptor responde, se asume que este esta activado
               begin                                //o que se activó con la cadena de texto que se le mandó.
               Result := True;                      //Devuelve TRUE porque el receptor esta activado y conectado.
               FActivo := True;                     //Indica que el receptor está activo.
               Break;                               //Sale del ciclo de comprobación y de la función.
               end;
            if Abs(GetTickCount - Inicio) > CTiempoDeActivacion then Break;
            end;
      Sleep(CTiempoDeActivacionObligado);
      AbortarTodo;                                  //Limpia los buffers y detiene las operaciones anteriores.
      end;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Establece la configuración inicial de los parámetros del receptor.
//-----------------------------------------------------------------------------
function TReceptorICR75.Inicializar: Boolean;
begin
Sleep(50);                                        //Realiza una espera para darle tiempo al receptor.
AbortarTodo;                                      //Detiene todas las operaciones pendientes.
ConfiguracionOriginalDelReceptor;                 //Establece una configuración por defecto para el receptor.

//Envía los comandos que configuran al receptor.
//No se debe variar el orden en que se introducen, ya
//que pueden existir dependencias entre estos.

Enviar($0E, $00);                                 //Detiene el escaneo.
Enviar($10, $00);                                 //Establece el paso de sintonía.
EnviarB($16, $22, 0);                             //Desactiva el NoiseBlanker.
EnviarB($16, $40, 0);                             //Desactiva el NoiseReduction.
EnviarB($16, $41, 0);                             //Desactiva el AutoNotch.
EnviarBB($1A, $02, $01, $02);                     //Activa la posibilidad de controlar la ganancia de RF y Squelch.
EnviarB($16, $12, FControlAutomaticoDeGanancia);  //Control Automático de Ganancia.
EnviarW($14, $01, FNivelDelAudio);                //Nivel de Audio Frecuencia. (Volumen)
EnviarW($14, $03, FNivelDelSquelch);              //Nivel del Squelch.
EnviarW($14, $02, FNivelDeRF);                    //Establece el nivel de RF.
EnviarW($14, $07, FTwinPbtInterior);              //Estado del control TWIN PBT Interior
EnviarW($14, $08, FTwinPbtExterior);              //Estado del control TWIN PBT Exterior.
Enviar($12, FAntena);                             //Selecciona la antena.
EnviarB($16, $02, FAmplificadorRF);               //Selecciona el amplificador de RF.
Enviar($11, FAtenuador);                          //Atenuador de entrada de antena.
EnviarB($01, FModo shr 8, FModo and $0F);         //Establecer ancho de banda y demodulador.
RecibirLaFrecuencia(FFrecuencia);                 //Establece una frecuencia original.

Sleep(50);                                        //Realiza una pausa necesaria.
Result := ComprobarAccion;                        //Devuelve TRUE si el receptor ha recibido comandos.
AbortarTodo;
end;

//-----------------------------------------------------------------------------
// Devuelve la frecuencia mínima que puede sintonizar el receptor.
//-----------------------------------------------------------------------------
function TReceptorICR75.FrecuenciaMinima: Double;
begin
Result := CFrecuenciaMinimaMhz;
end;

//-----------------------------------------------------------------------------
// Devuelve la frecuencia máxima que puede sintonizar el receptor.
//-----------------------------------------------------------------------------
function TReceptorICR75.FrecuenciaMaxima: Double;
begin
Result := CFrecuenciaMaximaMhz;
end;

//-----------------------------------------------------------------------------
// Devuelve TRUE si el receptor puede sintonizar la frecuencia.
//-----------------------------------------------------------------------------
function TReceptorICR75.FrecuenciaValida(FrecuenciaMhz: Double): Boolean;
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
function TReceptorICR75.TiempoMinimoDeCambioDeFrecuencia: Integer;
begin
Result := CTiempoMinimoDeCambioDeFrecuencia;
end;

//-----------------------------------------------------------------------------
// Devuelve la cantidad de milisegundos que se debe esperar entre
// el establecimiento de una frecuencia y la otra.
// Este valor se emplea como intervalo mínimo de espera
// para cuando se está explorando a grandes velocidades.
//-----------------------------------------------------------------------------
function TiempoMinimoDeCambioDeFrecuencia: Integer;
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
function TReceptorICR75.RecibirLaFrecuencia(Mhz: Double): Boolean;
var n, p: Integer;
    Frec: Int64;
    v: Double;
    DatoFrecuencia: String;
begin
Result := False;
AbortarTodo;                                                                          //Elimina los restos de comunicaciones anteriores.
Frec := IntegerToBCD(Round(Mhz * 1000000));                                           //Combierte de Mhz a Hz y luego lo convierte a código BCD.
DatoFrecuencia := '';                                                                 //Limpia la cadena que portará la frecuencia.
for n := 0 to 4 do DatoFrecuencia := DatoFrecuencia + Chr(Frec shr (8 * n) and $FF);  //Convierte el código BCD al formato del ICOM.
Enviar(CPreambulo + CReceptor + CControlador + Chr($00) + DatoFrecuencia + CFinal);   //Envía el comando con la frecuencia.
if ComprobarAccion then
   begin                                                                              //Si el receptor responde:
   Result := True;                                                                    //Devuelve TRUE.
   FFrecuencia := Mhz;                                                                //Registra el cambio de Frecuencia.
   end;
end;

//-----------------------------------------------------------------------------
// Abre un diálogo que permite al usuario configurar el receptor.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.ConfiguracionAvanzada;
begin
//if FActivo then
   with TFormReceptorICR75Configuracion.Crear(Self) do ShowModal;
end;

//-----------------------------------------------------------------------------
// Esta función apaga el receptor.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.Apagar;
begin
Enviar($18, $00);      //Envía el comando de apagar el receptor.
end;

///////////////////////////////////////////////////////////////////////////////
///////////////    Estas son las funciones internas     ///////////////////////
///////////////////////////////////////////////////////////////////////////////

//-----------------------------------------------------------------------------
// Establece los parámetros de funcionamiento del puerto serie COM
// necesarios para interactuar con este tipo de receptor.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.ConfiguracionOriginalDelPuerto;
begin
try
   FPuerto.Paridad.Bits := prNinguno;                //Establece los bits de paridad.
   FPuerto.Velocidad := br9600;                      //Establece la velocidad de la comunicación.
   FPuerto.BitsDeParada := sbDos;                    //Cantidad de bits de parada.
   FPuerto.ControlDeFlujo.FlowControl := fcNinguno;  //Control de flujo que se utiliza.
   FPuerto.CantidadDeBits := dbOcho;                 //Se utilizan ocho.
   FPuerto.FinDeCadena :='';                         //No se utilizan caracteres de fin de linea.
   FPuerto.LimpiarBuffer(True, True);                //Limpia los buffers.
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Establece la configuración por defecto del receptor.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.ConfiguracionOriginalDelReceptor;
begin
FControlAutomaticoDeGanancia := 0;   //Desactiva el control automático de ganancia.
FNivelDelAudio := 150;               //Nivel del audio.
FNivelDelSquelch := 0;               //Desactiva el Squelch.
FNivelDeRF := 200;                   //Nivel de RF.
FAntena := 0;                        //Antena de 50 Ohms.
FAmplificadorRF := 2;                //Selecciona el segundo amplificador de RF.
FAtenuador := 0;                     //Desactiva el atenuador.
FModo := 0;                          //Establece el modo LSB.
FTwinPbtInterior := 128;             //TWIN PBT Interior en el valor medio.
FTwinPbtExterior := 128;             //TWIN PBT Exterior en el valor medio.
FFrecuencia := 14.0;                 //Frecuencia de trabajo inicial.
FIluminacionDelLCD := 200;           //Nivel iluminación del LCD.
end;

//-----------------------------------------------------------------------------
// Detiene las operaciones que este realizando el puerto.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.AbortarTodo;
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
procedure TReceptorICR75.Enviar(Comando: String);
begin
try
   if FPuerto.Abierto then                           //Si el puerto esta abierto:
      begin
      AbortarTodo;                                   //Aborta todas las operaciones.
      FPuerto.EnviarCadena(Comando);                 //Enví ael comando.
      end;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Recibe una cadena desde el receptor.
//-----------------------------------------------------------------------------
function TReceptorICR75.Recibir: String;
begin
try
   if FPuerto.Abierto then                                          //Si el puerto esta abierto:
      FPuerto.RecibirCadena(Result, FPuerto.CaracteresRecibidos);   //lee una cadena desde el puerto.
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Comprueba si el receptor ha respondido.
// Esta función debe ser llamada exclusivamente después
// de mandar un comando al receptor, para verificar que
// este halla recibido el comando.
//-----------------------------------------------------------------------------
function TReceptorICR75.ComprobarAccion: Boolean;
var Inicio: Cardinal;
    msg: String;
begin
Result := False;
try
   Inicio := GetTickCount;                            //Obtiene el tiempo de inicio.
   while True do                                      //Comienza a mandarle mensajes al receptor recursivamente.
         begin
         if FPuerto.CaracteresRecibidos > 0 then      //Si el receptor responde, se asume que este esta activado
            begin                                     //o que se activó con la cadena de texto que se le mandó.
            Result := True;                           //Devuelve TRUE porque el receptor esta activado y conectado.
            Break;                                    //Sale del ciclo de comprobación y de la función.
            end;
         if Abs(GetTickCount - Inicio) > CTiempoDeRespuesta then Break;
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
procedure TReceptorICR75.EstablecerFrecuencia(Valor: Double);
begin
RecibirLaFrecuencia(Valor);
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el tipo de demodulador.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.EstablecerDemodulador(Valor: Word);
begin
AbortarTodo;                                //Elimina los restos de comunicaciones anteriores.
EnviarB($01, Valor shr 8, Valor and $0F);   //Envía el comando para establecer el modo.
if ComprobarAccion then FModo := Valor;     //Si el receptor responde: Registra el cambio de Demodulador.
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el atenuador.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.EstablecerAtenuador(Valor: Byte);
begin
AbortarTodo;
Enviar($11, Valor);
if ComprobarAccion then FAtenuador := Valor;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el AGC.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.EstablecerAGC(Valor: Byte);
begin
AbortarTodo;
EnviarB($16, $12, Valor);
if ComprobarAccion then FControlAutomaticoDeGanancia := Valor;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el Filtro Pasa Altos.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.EstablecerTwinPbtInterior(Valor: Byte);
begin
AbortarTodo;
EnviarW($14, $07, Valor);
if ComprobarAccion then FTwinPbtInterior := Valor;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el Filtro Pasa Bajos.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.EstablecerTwinPbtExterior(Valor: Byte);
begin
AbortarTodo;
EnviarW($14, $08, Valor);
if ComprobarAccion then FTwinPbtExterior := Valor;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que camboie el nivel del audio.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.EstablecerNivelDelAudio(Valor: Byte);
begin
AbortarTodo;
EnviarW($14, $01, Valor);
if ComprobarAccion then FNivelDelAudio := Valor;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que establecer la antena.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.EstablecerAntena(Valor: Byte);
begin
AbortarTodo;
Enviar($12, Valor);
if ComprobarAccion then FAntena := Valor;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que establecer el Squelch.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.EstablecerNivelDelSquelch(Valor: Byte);
begin
AbortarTodo;
EnviarW($14, $03, Valor);
if ComprobarAccion then FNivelDelSquelch := Valor;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que establecer el nivel de RF.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.EstablecerNivelDelRF(Valor: Byte);
begin
AbortarTodo;
EnviarW($14, $02, Valor);
if ComprobarAccion then FNivelDeRF := Valor;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que establecer el amplificador de RF.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.EstablecerAmplificadorRF(Valor: Byte);
begin
AbortarTodo;
EnviarB($16, $02, Valor);
if ComprobarAccion then FAmplificadorRF := Valor;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que establecer la iluminación del LCD.
//-----------------------------------------------------------------------------
procedure TReceptorICR75.EstablecerIluminacionDelLCD(Valor: Byte);
begin
AbortarTodo;
EnviarBW($1A, $02, $21, Valor);
if ComprobarAccion then FIluminacionDelLCD := Valor;
end;

end.
