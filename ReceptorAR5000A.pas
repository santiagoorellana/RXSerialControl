///////////////////////////////////////////////////////////////////////////////
// Autor: Santiago A. Orellana P�rez.
// Creado: 30/05/2013
// Escrito para: Delphi 6 y 7
// Utilizaci�n: Para controlar un receptor AOR modelo AR5000A.
///////////////////////////////////////////////////////////////////////////////

unit ReceptorAR5000A;

interface

uses ReceptorBasico, PuertoComControl, SysUtils, Classes, Windows;


//-----------------------------------------------------------------------------
const CFinDeCadena                      = #13#10;       //Caracteres que marcan el final de una linea.
const CTiempoDeRespuesta                = 3000;         //3 segundos. (Tiempo que se espera la respuesta de un comando)
const CTiempoDeActivacion               = 10000;        //10 segundos (Lo que demora el receptor activarse)
const CTiempoDeActivacionObligado       = 2000;         //3 segundos de espera obligatoria.
const CPrecisionDeFrecuencia            = 6;            //Cantidad de d�gitos despu�s de la coma, en la frecuencia.
const CTiempoMinimoDeCambioDeFrecuencia = 10;

const CFrecuenciaMinimaMhz              = 0.01;         //10 Khz es la M�nima frecuencia de trabajo del receptor.
const CFrecuenciaMaximaMhz              = 2999.999999;  //2999.999999 Mhz es la M�xima frecuencia de trabajo del receptor.

//-----------------------------------------------------------------------------
// Clase que representa un receptor tipo AR5000A.
// Esta clase implementa una interface del tipo "IReceptor" que contiene
// las funcionalidades b�sicas que deber�n poseer todos los receptores
//-----------------------------------------------------------------------------
type
   TReceptorAR5000A = class (TInterfacedObject, IReceptor)
   private
      FPuerto: TPuertoCOM;
      FActivo: Boolean;

      FControlAutomaticoDeGanancia: Byte;  //Control autom�tico de ganancia.
      FNivelDelAudio: Byte;                //Nivel del audio.
      FAntena: Byte;                       //Antena.
      FSalidaDeFI: Byte;                   //Conector de salida de la FI.
      FAtenuador: Byte;                    //Activaci�n del atenuador.
      FModo: Byte;                         //Modo de demodulaci�n.
      FFiltroPasaAltos: Byte;              //Filtro pasa altos.
      FFiltroPasaBajos: Byte;              //Filtro pasa bajos.
      FAnchoDeBanda: Byte;                 //Ancho de banda.
      FFrecuencia: Double;                 //Frecuencia de trabajo.
      FDeEmphasis: Byte;                   //De-Emphasis.
      FSquelch: Byte;                      //Nivel del Squelch.

      procedure AbortarTodo;
      procedure EstablecerFrecuencia(Valor: Double);
      procedure EstablecerDemodulador(Valor: Byte);
      procedure EstablecerAnchoDeBanda(Valor: Byte);
      procedure EstablecerAtenuador(Valor: Byte);
      procedure EstablecerAGC(Valor: Byte);
      procedure EstablecerFiltroPasaAltos(Valor: Byte);
      procedure EstablecerFiltroPasaBajos(Valor: Byte);
      procedure EstablecerNivelDelAudio(Valor: Byte);
      procedure EstablecerAntena(Valor: Byte);
      procedure EstablecerDeEmphasis(Valor: Byte);
      procedure EstablecerSalidaDeFI(Valor: Byte);
      procedure EstablecerSquelch(Valor: Byte);

      function ComprobarAccion: Boolean;
   public
      property Frecuencia: Double read FFrecuencia write EstablecerFrecuencia;
      property Demodulador: Byte read FModo write EstablecerDemodulador;
      property AnchoDebanda: Byte read FAnchoDebanda write EstablecerAnchoDeBanda;
      property Atenuador: Byte read FAtenuador write EstablecerAtenuador;
      property Antena: Byte read FAntena write EstablecerAntena;
      property ControlAutomaticoDeGanancia: Byte read FControlAutomaticoDeGanancia write EstablecerAGC;
      property FiltroPasaAltos: Byte read FFiltroPasaAltos write EstablecerFiltroPasaAltos;
      property FiltroPasaBajos: Byte read FFiltroPasaBajos write EstablecerFiltroPasaBajos;
      property NivelDelAudio: Byte read FNivelDelAudio write EstablecerNivelDelAudio;
      property DeEmphasis: Byte read FDeEmphasis write EstablecerDeEmphasis;
      property SalidaDeFI: Byte read FSalidaDeFI write EstablecerSalidaDeFI;
      property Squelch: Byte read FSquelch write EstablecerSquelch;

      //M�todos propios del tipo de receptor.
      constructor Create(PuetoCom: Integer);
      procedure ConfiguracionOriginalDelPuerto;
      procedure ConfiguracionOriginalDelReceptor;
      procedure Enviar(Comando: String);
      function Recibir: String;
      function EnviarYComprobarRespuesta(Comando: String; Espera: Integer): Boolean;
      function DeFrecuenciaACadena(FrecuenciaMhz: Double): String;


      //M�todos de la interface b�sica del receptor.
      procedure ConfigurarPuerto;
      function Activar: Boolean;
      function Inicializar: Boolean;
      function FrecuenciaMinima: Double;                          //Devuelve la m�nima frecuencia de trabajo.
      function FrecuenciaMaxima: Double;                          //Devuelve la frecuencia m�xima de trabajo.
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

uses StrUtils, Forms, ReceptorAR5000AConfiguracion;

//-----------------------------------------------------------------------------
// Este es el constructor de la clase.
//-----------------------------------------------------------------------------
constructor TReceptorAR5000A.Create(PuetoCom: Integer);
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
procedure TReceptorAR5000A.Eliminar;
begin
try
   FPuerto.EnviarCadena('QP');   //Apaga el receptor.
   FPuerto.Cerrar;               //Detiene todas las operaciones y libera el puerto.
   FPuerto.Free;                 //Destruye el objeto de interacci�n con el puerto.
except
   //nada
end;
inherited Destroy;            //Llama al destructor de la clase paterna.
end;

//-----------------------------------------------------------------------------
// Abre el di�logo de configuraci�n del puerto serie.
//-----------------------------------------------------------------------------
procedure TReceptorAR5000A.ConfigurarPuerto;
begin
try
   FPuerto.MostrarDialogoDeConfiguracion;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Intenta activar el receptor.
// La salida de la funci�n solo se produce cuando la operaci�n
// concluya, y los resultados ser�n devueltos a la salida.
//
// Salida:
// Si el receptor se activa, devuelve TRUE. Si el receptor est� desconectado
// o no se puede activar por alguna otra raz�n, devuelve FALSE.
//-----------------------------------------------------------------------------
function TReceptorAR5000A.Activar: Boolean;
var Inicio: Cardinal;
begin
Result := False;                                 //Si no se puede activar, devuelve FALSE.
FActivo := False;                                //Por el momento el receptor no est� activo.
try
   FPuerto.Abrir;                                   //Abre el puerto serie.
   if FPuerto.Abierto then                          //Si el puerto est� conectado:
      begin
      ConfiguracionOriginalDelPuerto;               //Establece los par�metros de funcionamiento del puerto.

      //Si el receptor no esta activado, lo activa.
      AbortarTodo;                                  //Limpia los buffers y detiene las operaciones anteriores.
      Inicio := GetTickCount;                       //Obtiene el tiempo de inicio.
      while True do                                 //Comienza a mandarle mensajes al receptor recursivamente.
            begin
            FPuerto.EnviarCadena('RF');             //El mensaje dice: �Dime en que frecuencia estas?.
            if FPuerto.CaracteresRecibidos > 0 then //Si el receptor responde, se asume que este esta activado
               begin                                //o que se activ� con la cadena de texto que se le mand�.
               Result := True;                      //Devuelve TRUE porque el receptor esta activado y conectado.
               FActivo := True;                     //Indica que el receptor est� activo.
               Break;                               //Sale del ciclo de comprobaci�n y de la funci�n.
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
// Establece la configuraci�n inicial de los par�metros del receptor.
//-----------------------------------------------------------------------------
function TReceptorAR5000A.Inicializar: Boolean;
begin
try
   //Realiza una espera para darle tiempo al receptor.
   Sleep(100);

   //Detiene todas las operaciones pendientes.
   AbortarTodo;

   //Establece una configuraci�n por defecto para el receptor.
   ConfiguracionOriginalDelReceptor;

   //Env�a los comandos que configuran al receptor.
   //No se debe variar el orden en que se introducen, ya
   //que pueden existir dependencias entre estos.
   FPuerto.EnviarCadena('AF' + IntToStr(FControlAutomaticoDeGanancia)); //Control autom�tico de ganancia = Desactivado.
   FPuerto.EnviarCadena('AI1');                                         //Se exportar� la frecuencia intermedia por el conector #1.
   FPuerto.EnviarCadena('AN1');                                         //Se seleccionar� la antena #1.
   FPuerto.EnviarCadena('ACF');                                         //No se utilizar� el control autom�tico de ganancia.
   FPuerto.EnviarCadena('AS0');                                         //Desactivado.
   if FFiltroPasaBajos <> 255 then
      FPuerto.EnviarCadena('AT' + IntToStr(FAtenuador))                 //Selecci�n del filtro pasa bajos.
   else
      FPuerto.EnviarCadena('ATF');                                      //Filtro pasa bajos autom�tico.
   FPuerto.EnviarCadena('BQ0');                                         //Desactivado.
   FPuerto.EnviarCadena('BW' + IntToStr(FAnchoDeBanda));                //Selecciona un ancho de banda.
   FPuerto.EnviarCadena('DA000');                                       //VFO Voice-level = Desactivado.
   FPuerto.EnviarCadena('DB000');                                       //VFO squelch-level = Desactivado.
   FPuerto.EnviarCadena('DS0');                                         //Cyberscan = Desactivado.
   FPuerto.EnviarCadena('EN' + IntToStr(FDeEmphasis));                  //Desactivado.
   FPuerto.EnviarCadena('GA0');                                         //Tag memory channels for select scan = Desactivado.
   FPuerto.EnviarCadena('HP' + IntToStr(FFiltroPasaAltos));             //Selecci�n del filtro pasa altos.
   FPuerto.EnviarCadena('LP' + IntToStr(FFiltroPasaBajos));             //Selecci�n del filtro pasa bajos.
   FPuerto.EnviarCadena('LC0');                                         //Respond with frequency and level when squelch opens = Desactivado.
   FPuerto.EnviarCadena('LS000');                                       //Tone eliminate frequency = Desactivado.
   FPuerto.EnviarCadena('MD' + IntToStr(FModo));                        //Establece el modo.
   FPuerto.EnviarCadena('ML0');                                         //Escaneo de banco = Desactivado.
   FPuerto.EnviarCadena('MP0');                                         //Memory channel as pass = Desactivado.
   FPuerto.EnviarCadena('MT1');                                         //Manual tune or auto-tune of frontend preselector = Desactivado.
   FPuerto.EnviarCadena('NB0');                                         //Ruido blanco = Desactivado.
   FPuerto.EnviarCadena('QM0');                                         //Decodificador DTMF = Desactivado.
   FPuerto.EnviarCadena('RQ255');                                       //Squelch = M�nimo.
   FPuerto.EnviarCadena('SA0');                                         //Voice level = Desactivado.
   FPuerto.EnviarCadena('SB0');                                         //Level scan = Desactivado.
   FPuerto.EnviarCadena('ST0.05');                                      //Tuning step size = 0.05 Khz.
   FPuerto.EnviarCadena('SJ0');                                         //Sub-step size
   FPuerto.EnviarCadena('VL' + IntToStr(FNivelDelAudio));               //Nivel de audio.
   FPuerto.EnviarCadena(DeFrecuenciaACadena(FFrecuencia));              //Establece una frecuencia inicial.

   Sleep(100);                                                           //Realiza una pausa necesaria.
   Result := ComprobarAccion;                                           //Devuelve TRUE si el receptor ha recibido comandos.
   AbortarTodo;
except
   Result := False;
end;
end;

//-----------------------------------------------------------------------------
// Devuelve la frecuencia m�nima que puede sintonizar el receptor.
//-----------------------------------------------------------------------------
function TReceptorAR5000A.FrecuenciaMinima: Double;
begin
Result := CFrecuenciaMinimaMhz;
end;

//-----------------------------------------------------------------------------
// Devuelve la frecuencia m�xima que puede sintonizar el receptor.
//-----------------------------------------------------------------------------
function TReceptorAR5000A.FrecuenciaMaxima: Double;
begin
Result := CFrecuenciaMaximaMhz;
end;

//-----------------------------------------------------------------------------
// Devuelve TRUE si el receptor puede sintonizar la frecuencia.
//-----------------------------------------------------------------------------
function TReceptorAR5000A.FrecuenciaValida(FrecuenciaMhz: Double): Boolean;
begin
Result := (FrecuenciaMhz >= CFrecuenciaMinimaMhz) and
          (FrecuenciaMhz <= CFrecuenciaMaximaMhz);
end;

//-----------------------------------------------------------------------------
// Devuelve la cantidad de milisegundos que se debe esperar entre
// el establecimiento de una frecuencia y la otra.
// Este valor se emplea como intervalo m�nimo de espera
// para cuando se est� explorando a grandes velocidades.
//-----------------------------------------------------------------------------
function TReceptorAR5000A.TiempoMinimoDeCambioDeFrecuencia: Integer;
begin
Result := CTiempoMinimoDeCambioDeFrecuencia;
end;

//-----------------------------------------------------------------------------
// Esta funci�n hace que el receptor sintonice la frecuencia indicada.
// La salida de la funci�n solo se produce cuando la operaci�n
// concluya, y el resultado ser� devuelto a la salida.
//
// Entrada:
// FrecuenciaMhz = Frecuencia en Mega Herts.
//
// Salida:
// Devuelve TRUE si el receptor ha respondido.
//-----------------------------------------------------------------------------
function TReceptorAR5000A.RecibirLaFrecuencia(Mhz: Double): Boolean;
var Valor: String;
begin
AbortarTodo;                                                      //Elimina los restos de comunicaciones anteriores.
Valor := DeFrecuenciaACadena(Mhz);                                //Crea el comando para el cambio de frecuencia.
Result := EnviarYComprobarRespuesta(Valor, CTiempoDeRespuesta);   //Env�a el comando al receptor y si este responde:
if Result then FFrecuencia := Mhz;                                //Devuelve TRUE y asume que se ha cambiado la frecuencia.
end;

//-----------------------------------------------------------------------------
// Abre un di�logo que permite al usuario configurar el receptor.
//-----------------------------------------------------------------------------
procedure TReceptorAR5000A.ConfiguracionAvanzada;
begin
//if FActivo then
   with TFormReceptorAR5000AConfiguracion.Crear(Self) do ShowModal;
end;

//-----------------------------------------------------------------------------
// Esta funci�n apaga el receptor.
//-----------------------------------------------------------------------------
procedure TReceptorAR5000A.Apagar;
begin
try
   FPuerto.EnviarCadena('QP');     //Env�a el comando de apagar el receptor.
except
   //Nada
end;
end;

///////////////////////////////////////////////////////////////////////////////
///////////////    Estas son las funciones internas     ///////////////////////
///////////////////////////////////////////////////////////////////////////////

//-----------------------------------------------------------------------------
// Establece los par�metros de funcionamiento del puerto serie COM
// necesarios para interactuar con este tipo de receptor.
//-----------------------------------------------------------------------------
procedure TReceptorAR5000A.ConfiguracionOriginalDelPuerto;
begin
try
   FPuerto.Paridad.Bits := prNinguno;                //Establece los bits de paridad.
   FPuerto.Velocidad := br19200;                     //Establece la velocidad de la comunicaci�n.
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
// Establece la configuraci�n por defecto del receptor.
//-----------------------------------------------------------------------------
procedure TReceptorAR5000A.ConfiguracionOriginalDelReceptor;
begin
FAntena := 1;                          //Selecciona la antena #1.
FSalidaDeFI := 1;                      //La FI saldr� por el primer conector. 
FControlAutomaticoDeGanancia := 255;   //Control autom�tico de ganancia = Desactivado.
FNivelDelAudio := 150;                 //Nivel de audio = 150.
FAtenuador := 0;                       //Atenuador = Desactivado.
FModo := 2;                            //Establece el modo LSB.
FFiltroPasaAltos := 0;                 //Filtro pasa altos = 50 Hz.
FFiltroPasaBajos := 3;                 //Filtro pasa bajos = 12 Khz.
FAnchoDeBanda := 6;                    //Selecciona un ancho de banda de 200 Khz.
FFrecuencia := 14.0;                   //Establece una frecuencia inicial.
FDeEmphasis := 4;
end;

//-----------------------------------------------------------------------------
// Detiene las operaciones que este realizando el puerto.
//-----------------------------------------------------------------------------
procedure TReceptorAR5000A.AbortarTodo;
begin
try
   FPuerto.AbortarTodasLasOperacionesAsincronicas;   //Detiene las oparaciones asincr�nicas.
   FPuerto.LimpiarBuffer(True, True);                //Borra los buffers de entrada y salida.
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia una cadena que contiene uno o varios comandos para el receptor.
//-----------------------------------------------------------------------------
procedure TReceptorAR5000A.Enviar(Comando: String);
begin
try
   if FPuerto.Abierto then                           //Si el puerto esta abierto:
      begin
      AbortarTodo;                                   //Aborta todas las operaciones.
      FPuerto.EnviarCadena(Comando);                 //Env� ael comando.
      end;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Recibe una cadena desde el receptor.
//-----------------------------------------------------------------------------
function TReceptorAR5000A.Recibir: String;
begin
try
   if FPuerto.Abierto then                                          //Si el puerto esta abierto:
      FPuerto.RecibirCadena(Result, FPuerto.CaracteresRecibidos);   //lee una cadena desde el puerto.
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Esta funci�n env�a una cadena al receptor y espera durante un tiempo
// la respuesta del receptor. Se asume que el receptor conectado es un AR-5000A.
//
// Entrada:
// Comando = Cadena de texto que contiene el comando que se desea enviar.
// Espera  = Tiempo de espera de la respuesta en milisegundos.
//
// Salida:
// Devuelve TRUE si el receptor responde antes de que se termine el
// tiempo de espera. De lo contrario, devuelve FALSE.
//-----------------------------------------------------------------------------
function TReceptorAR5000A.EnviarYComprobarRespuesta(Comando: String; Espera: Integer): Boolean;
var Inicio: Cardinal;                                       //Guarda el tiempo de inicio.
begin
Result := False;
try
   if FPuerto.Abierto then                                     //Si el puerto esta abierto:
      begin
      AbortarTodo;                                             //Termina todas las operaciones y limpia los buffers.
      FPuerto.EnviarCadena(Comando);                           //Env�a el comando.
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
function TReceptorAR5000A.DeFrecuenciaACadena(FrecuenciaMhz: Double): String;
begin
Result := 'RF' + FloatToStrF(FrecuenciaMhz, ffNumber, 15, CPrecisionDeFrecuencia);  //Crea la frecuencia.
Result := AnsiReplaceStr(Result, ',', '.');                                         //Reemplaza la coma por un punto.
end;

//-----------------------------------------------------------------------------
// Comprueba si el receptor ha respondido.
// Esta funci�n debe ser llamada exclusivamente despu�s
// de mandar un comando al receptor, para verificar que
// este halla recibido el comando.
//-----------------------------------------------------------------------------
function TReceptorAR5000A.ComprobarAccion: Boolean;
var Inicio: Cardinal;
    msg: String;
begin
Result := False;
try
   Inicio := GetTickCount;                            //Obtiene el tiempo de inicio.
   while True do                                      //Comienza a mandarle mensajes al receptor recursivamente.
         begin
         if FPuerto.CaracteresRecibidos > 0 then      //Si el receptor responde, se asume que este esta activado
            begin                                     //o que se activ� con la cadena de texto que se le mand�.
            Result := True;                           //Devuelve TRUE porque el receptor esta activado y conectado.
            Break;                                    //Sale del ciclo de comprobaci�n y de la funci�n.
            end;
         if Abs(GetTickCount - Inicio) > CTiempoDeRespuesta then Break;
         end;
   if not Result then
      begin
      msg := 'El receptor no responde.' + #13;
      msg := msg + 'Compruebe la conecci�n del puerto COM.';
      MessageBox(0, PChar(msg), 'Atenci�n', MB_ICONEXCLAMATION);
      end;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Establece la frecuencia de recepci�n.
//-----------------------------------------------------------------------------
procedure TReceptorAR5000A.EstablecerFrecuencia(Valor: Double);
begin
RecibirLaFrecuencia(Valor);
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el tipo de demodulador.
//-----------------------------------------------------------------------------
procedure TReceptorAR5000A.EstablecerDemodulador(Valor: Byte);
begin
try
   AbortarTodo;
   FPuerto.EnviarCadena('MD' + IntToStr(Valor));                     //Env�a el comando para cambiar de demodulador.
   if ComprobarAccion then                                           //Si el receptor responde:
      begin
      FModo := Valor;                                                //Registra el cambio de Demodulador.
      AnchoDeBanda := FAnchoDeBanda;                                 //Mantiene el ancho de banda actual.
      FiltroPasaAltos := FFiltroPasaAltos;                           //Mantiene el filtro Pasa Altos actual.
      FiltroPasaBajos := FFiltroPasaBajos;                           //Mantiene el filtro pasa Bajos actual.
      ControlAutomaticoDeGanancia := FControlAutomaticoDeGanancia;   //mantiene el estado actual del AGC.
      end;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el ancho de banda.
//-----------------------------------------------------------------------------
procedure TReceptorAR5000A.EstablecerAnchoDeBanda(Valor: Byte);
begin
try
   AbortarTodo;
   FPuerto.EnviarCadena('BW' + IntToStr(Valor));   //Env�a un comando para cambiar el ancho de banda.
   if ComprobarAccion then                         //Si el receptor responde:
      begin
      FAnchoDeBanda := Valor;                      //Registra el cambio de Ancho de Banda.
      FiltroPasaAltos := FFiltroPasaAltos;         //Mantiene el filtro Pasa Altos actual.
      FiltroPasaBajos := FFiltroPasaBajos;         //Mantiene el filtro pasa Bajos actual.
      DeEmphasis := FDeEmphasis;                   //Mantiene el valor actual de De-Emphasis.
      end;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el atenuador.
//-----------------------------------------------------------------------------
procedure TReceptorAR5000A.EstablecerAtenuador(Valor: Byte);
begin
try
   AbortarTodo;
   if Valor <> 255 then
      FPuerto.EnviarCadena('AT' + IntToStr(Valor))
   else
      FPuerto.EnviarCadena('ATF');
   if ComprobarAccion then FAtenuador := Valor;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el AGC.
//-----------------------------------------------------------------------------
procedure TReceptorAR5000A.EstablecerAGC(Valor: Byte);
begin
try
   AbortarTodo;
   if Valor <> 255 then
      FPuerto.EnviarCadena('AC' + IntToStr(Valor))
   else
      FPuerto.EnviarCadena('ACF');
   if ComprobarAccion then FControlAutomaticoDeGanancia := Valor;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que cambie el Filtro Pasa Altos.
//-----------------------------------------------------------------------------
procedure TReceptorAR5000A.EstablecerFiltroPasaAltos(Valor: Byte);
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
procedure TReceptorAR5000A.EstablecerFiltroPasaBajos(Valor: Byte);
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
// Envia un comando al receptor para que camboie el nivel del audio.
//-----------------------------------------------------------------------------
procedure TReceptorAR5000A.EstablecerNivelDelAudio(Valor: Byte);
begin
try
   AbortarTodo;
   FPuerto.EnviarCadena('VL' + IntToStr(Valor));
   if ComprobarAccion then FNivelDelAudio := Valor;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que establecer la antena.
//-----------------------------------------------------------------------------
procedure TReceptorAR5000A.EstablecerAntena(Valor: Byte);
begin
try
   AbortarTodo;
   FPuerto.EnviarCadena('AN' + IntToStr(Valor));
   if ComprobarAccion then FAntena := Valor;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que establecer la antena.
//-----------------------------------------------------------------------------
procedure TReceptorAR5000A.EstablecerDeEmphasis(Valor: Byte);
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
// Envia un comando al receptor para que establecer la salida de FI.
//-----------------------------------------------------------------------------
procedure TReceptorAR5000A.EstablecerSalidaDeFI(Valor: Byte);
begin
try
   AbortarTodo;
   FPuerto.EnviarCadena('AI' + IntToStr(Valor));
   if ComprobarAccion then FSalidaDeFI := Valor;
except
   //Nada
end;
end;

//-----------------------------------------------------------------------------
// Envia un comando al receptor para que establecer el nivel del Squelch.
//-----------------------------------------------------------------------------
procedure TReceptorAR5000A.EstablecerSquelch(Valor: Byte);
begin
try
   AbortarTodo;
   FPuerto.EnviarCadena('RQ' + IntToStr(Valor));
   if ComprobarAccion then FSalidaDeFI := Valor;
except
   //Nada
end;
end;

end.
