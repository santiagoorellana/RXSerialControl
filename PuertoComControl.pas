
///////////////////////////////////////////////////////////////////////////////
// Nombre: PuertoComControl
// Autor: Santiago A. Orellana Pérez.
// Creado: 15/05/2013
// Escrito para: Delphi 6 y 7
// Funcionamiento: Objeto que controla y permite la comunicación
//                 con el puerto serie de la computadora. Este objeto puede
//                 guardar su configuración en un fichero del tipo INI y
//                 cargarla nuevamente.
// Base: Esta librería se escribió utilizando como base la librería escrita
//       por Dejan Crnila (1998 - 2002) y Lars B. Dybdahl (2003).
//       Disponible desde: http://comport.sf.net/
///////////////////////////////////////////////////////////////////////////////

unit PuertoComControl;

interface

uses
  Windows, Messages, Classes, SysUtils, IniFiles, Registry;

type
  TPort = string;

  TBaudRate = (brDeCostumbre, br110, br300, br600, br1200, br2400, br4800, br9600, br14400,
               br19200, br38400, br56000, br57600, br115200, br128000, br256000);

  TStopBits = (sbUno, sbUnoYMedio, sbDos);

  TDataBits = (dbCinco, dbSeis, dbSiete, dbOcho);

  TParityBits = (prNinguno, prImpar, prPar, prMarca, prEspacio);

  TDTRFlowControl = (dtrDesactivado, dtrActivado, dtrHandshake);

  TRTSFlowControl = (rtsDesactivado, rtsActivado, rtsHandshake, rtsToggle);

  TFlowControl = (fcHardware, fcSoftware, fcNinguno, fcDeCostumbre);

  TComEvent = (evRxChar, evTxEmpty, evRxFlag, evRing, evBreak, evCTS, evDSR,
               evError, evRLSD, evRx80Full);

  TComEvents = set of TComEvent;

  TComSignal = (csCTS, csDSR, csRing, csRLSD);

  TComSignals = set of TComSignal;

  TComError = (ceFrame, ceRxParity, ceOverrun, ceBreak, ceIO, ceMode, ceRxOver, ceTxFull);

  TComErrors = set of TComError;

  TSyncMethod = (smThreadSync, smWindowSync, smNone);

  TTipoDeGuardado = (stEnRegistro, stFicheroINI);

  TStoredProp = (spBasic, spFlowControl, spBuffer, spTimeouts, spParity, spOthers);

  TStoredProps = set of TStoredProp;

  TRxCharEvent = procedure(Sender: TObject; Count: Integer) of object;

  TRxBufEvent = procedure(Sender: TObject; const Buffer; Count: Integer) of object;

  TComErrorEvent = procedure(Sender: TObject; Errors: TComErrors) of object;

  TComSignalEvent = procedure(Sender: TObject; OnOff: Boolean) of object;

//------------------------------------------------------------------------------
// Tipos que se emplean para las llamadas asincrónicas.
//------------------------------------------------------------------------------
  TOperationKind = (okWrite, okRead);

  TAsync = record
           Overlapped: TOverlapped;
           Kind: TOperationKind;
           Data: Pointer;
           Size: Integer;
           end;

  PAsync = ^TAsync;

////////////////////////////////////////////////////////////////////////////////
// Estas son las clases que se emplean para el acceso al puerto serie COM.
////////////////////////////////////////////////////////////////////////////////

  TPuertoCOM = class;      //Esta clase se implementa más abajo.

//------------------------------------------------------------------------------
// Subproceso que monitorea en segundo plano los eventos del puerto.
//------------------------------------------------------------------------------
  TComThread = class(TThread)
  private
    FComPort: TPuertoCOM;
    FStopEvent: THandle;
    FEvents: TComEvents;
  protected
    procedure DispatchComMsg;
    procedure DoEvents;
    procedure Execute; override;
    procedure SendEvents;
    procedure Stop;
  public
    constructor Create(AComPort: TPuertoCOM);
    destructor Destroy; override;
  end;

//------------------------------------------------------------------------------
// Propiedad que establece los tiempos de salida (Timeouts) para las
// operaciones de lectura y escritura (read/write).
//------------------------------------------------------------------------------
  TComTimeouts = class(TPersistent)
  private
    FComPort: TPuertoCOM;
    FReadInterval: Integer;
    FReadTotalM: Integer;
    FReadTotalC: Integer;
    FWriteTotalM: Integer;
    FWriteTotalC: Integer;
    procedure SetComPort(const AComPort: TPuertoCOM);
    procedure SetReadInterval(const Value: Integer);
    procedure SetReadTotalM(const Value: Integer);
    procedure SetReadTotalC(const Value: Integer);
    procedure SetWriteTotalM(const Value: Integer);
    procedure SetWriteTotalC(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property ComPort: TPuertoCOM read FComPort;
  published
    property ReadInterval: Integer read FReadInterval write SetReadInterval default -1;
    property ReadTotalMultiplier: Integer read FReadTotalM write SetReadTotalM default 0;
    property ReadTotalConstant: Integer read FReadTotalC write SetReadTotalC default 0;
    property WriteTotalMultiplier: Integer read FWriteTotalM write SetWriteTotalM default 100;
    property WriteTotalConstant: Integer read FWriteTotalC write SetWriteTotalC default 1000;
  end;

//------------------------------------------------------------------------------
// Clase para el establecimiento del flujo de ontrol. (flow control)
//------------------------------------------------------------------------------
  TComFlowControl = class(TPersistent)
  private
    FComPort: TPuertoCOM;
    FOutCTSFlow: Boolean;
    FOutDSRFlow: Boolean;
    FControlDTR: TDTRFlowControl;
    FControlRTS: TRTSFlowControl;
    FXonXoffOut: Boolean;
    FXonXoffIn:  Boolean;
    FDSRSensitivity: Boolean;
    FTxContinueOnXoff: Boolean;
    FXonChar: Char;
    FXoffChar: Char;
    procedure SetComPort(const AComPort: TPuertoCOM);
    procedure SetOutCTSFlow(const Value: Boolean);
    procedure SetOutDSRFlow(const Value: Boolean);
    procedure SetControlDTR(const Value: TDTRFlowControl);
    procedure SetControlRTS(const Value: TRTSFlowControl);
    procedure SetXonXoffOut(const Value: Boolean);
    procedure SetXonXoffIn(const Value: Boolean);
    procedure SetDSRSensitivity(const Value: Boolean);
    procedure SetTxContinueOnXoff(const Value: Boolean);
    procedure SetXonChar(const Value: Char);
    procedure SetXoffChar(const Value: Char);
    procedure SetFlowControl(const Value: TFlowControl);
    function GetFlowControl: TFlowControl;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property ComPort: TPuertoCOM read FComPort;
  published
    property FlowControl: TFlowControl read GetFlowControl write SetFlowControl stored False;
    property OutCTSFlow: Boolean read FOutCTSFlow write SetOutCTSFlow;
    property OutDSRFlow: Boolean read FOutDSRFlow write SetOutDSRFlow;
    property ControlDTR: TDTRFlowControl read FControlDTR write SetControlDTR;
    property ControlRTS: TRTSFlowControl read FControlRTS write SetControlRTS;
    property XonXoffOut: Boolean read FXonXoffOut write SetXonXoffOut;
    property XonXoffIn:  Boolean read FXonXoffIn write SetXonXoffIn;
    property DSRSensitivity: Boolean read FDSRSensitivity write SetDSRSensitivity default False;
    property TxContinueOnXoff: Boolean read FTxContinueOnXoff write SetTxContinueOnXoff default False;
    property XonChar: Char read FXonChar write SetXonChar default #17;
    property XoffChar: Char read FXoffChar write SetXoffChar default #19;
  end;

//------------------------------------------------------------------------------
// Clase para el establecimiento de la paridad (parity settings)
//------------------------------------------------------------------------------
  TComParity = class(TPersistent)
  private
    FComPort: TPuertoCOM;
    FBits: TParityBits;
    FCheck: Boolean;
    FReplace: Boolean;
    FReplaceChar: Char;
    procedure SetComPort(const AComPort: TPuertoCOM);
    procedure SetBits(const Value: TParityBits);
    procedure SetCheck(const Value: Boolean);
    procedure SetReplace(const Value: Boolean);
    procedure SetReplaceChar(const Value: Char);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property ComPort: TPuertoCOM read FComPort;
  published
    property Bits: TParityBits read FBits write SetBits;
    property Check: Boolean read FCheck write SetCheck default False;
    property Replace: Boolean read FReplace write SetReplace default False;
    property ReplaceChar: Char read FReplaceChar write SetReplaceChar default #0;
  end;

//------------------------------------------------------------------------------
// Para el establecimiento del tamaño de los buffers (buffer size settings).
//------------------------------------------------------------------------------
  TComBuffer = class(TPersistent)
  private
    FComPort: TPuertoCOM;
    FInputSize: Integer;
    FOutputSize: Integer;
    procedure SetComPort(const AComPort: TPuertoCOM);
    procedure SetInputSize(const Value: Integer);
    procedure SetOutputSize(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property ComPort: TPuertoCOM read FComPort;
  published
    property InputSize: Integer read FInputSize write SetInputSize default 1024;
    property OutputSize: Integer read FOutputSize write SetOutputSize default 1024;
  end;

//------------------------------------------------------------------------------
// Clase principal que representa al puerto serie COM.
//------------------------------------------------------------------------------
  TPuertoCOM = class
  private
    FEventThread: TComThread;
    FThreadCreated: Boolean;
    FHandle: THandle;
    FWindow: THandle;
    FUpdateCount: Integer;
    FTriggersOnRxChar: Boolean;
    FEventThreadPriority: TThreadPriority;
    FConnected: Boolean;
    FBaudRate: TBaudRate;
    FCustomBaudRate: Integer;
    FPort: TPort;
    FStopBits: TStopBits;
    FDataBits: TDataBits;
    FDiscardNull: Boolean;
    FEventChar: Char;
    FEvents: TComEvents;
    FBuffer: TComBuffer;
    FParity: TComParity;
    FTimeouts: TComTimeouts;
    FFlowControl: TComFlowControl;
    FSyncMethod: TSyncMethod;
    FStoredProps: TStoredProps;
    FOnRxChar: TRxCharEvent;
    FOnRxBuf: TRxBufEvent;
    FOnTxEmpty: TNotifyEvent;
    FOnBreak: TNotifyEvent;
    FOnRing: TNotifyEvent;
    FOnCTSChange: TComSignalEvent;
    FOnDSRChange: TComSignalEvent;
    FOnRLSDChange: TComSignalEvent;
    FOnError: TComErrorEvent;
    FOnRxFlag: TNotifyEvent;
    FOnAfterOpen: TNotifyEvent;
    FOnAfterClose: TNotifyEvent;
    FOnBeforeOpen: TNotifyEvent;
    FOnBeforeClose: TNotifyEvent;
    FOnRx80Full: TNotifyEvent;
    function GetTriggersOnRxChar: Boolean;
    procedure SetTriggersOnRxChar(const Value: Boolean);
    procedure SetConnected(const Value: Boolean);
    procedure SetBaudRate(const Value: TBaudRate);
    procedure SetCustomBaudRate(const Value: Integer);
    procedure SetPort(const Value: TPort);
    procedure SetStopBits(const Value: TStopBits);
    procedure SetDataBits(const Value: TDataBits);
    procedure SetDiscardNull(const Value: Boolean);
    procedure SetEventChar(const Value: Char);
    procedure SetSyncMethod(const Value: TSyncMethod);
    procedure SetEventThreadPriority(const Value: TThreadPriority);
    procedure SetParity(const Value: TComParity);
    procedure SetTimeouts(const Value: TComTimeouts);
    procedure SetBuffer(const Value: TComBuffer);
    procedure SetFlowControl(const Value: TComFlowControl);
    procedure CheckSignals(Open: Boolean);
    procedure WindowMethod(var Message: TMessage);
    procedure CallAfterOpen;
    procedure CallAfterClose;
    procedure CallBeforeOpen;
    procedure CallBeforeClose;
    procedure CallRxChar;
    procedure CallTxEmpty;
    procedure CallBreak;
    procedure CallRing;
    procedure CallRxFlag;
    procedure CallCTSChange;
    procedure CallDSRChange;
    procedure CallError;
    procedure CallRLSDChange;
    procedure CallRx80Full;
  protected
    procedure DoAfterClose; dynamic;
    procedure DoAfterOpen; dynamic;
    procedure DoBeforeClose; dynamic;
    procedure DoBeforeOpen; dynamic;
    procedure DoRxChar(Count: Integer); dynamic;
    procedure DoRxBuf(const Buffer; Count: Integer); dynamic;
    procedure DoTxEmpty; dynamic;
    procedure DoBreak; dynamic;
    procedure DoRing; dynamic;
    procedure DoRxFlag; dynamic;
    procedure DoCTSChange(OnOff: Boolean); dynamic;
    procedure DoDSRChange(OnOff: Boolean); dynamic;
    procedure DoError(Errors: TComErrors); dynamic;
    procedure DoRLSDChange(OnOff: Boolean); dynamic;
    procedure DoRx80Full; dynamic;
    procedure StoreRegistry(Reg: TRegistry); virtual;
    procedure StoreIniFile(IniFile: TIniFile); virtual;
    procedure LoadRegistry(Reg: TRegistry); virtual;
    procedure LoadIniFile(IniFile: TIniFile); virtual;
    procedure CreateHandle; virtual;
    procedure DestroyHandle; virtual;
    procedure ApplyDCB; dynamic;
    procedure ApplyTimeouts; dynamic;
    procedure ApplyBuffer; dynamic;
    procedure SetupComPort; virtual;
  public
    Nombre: String;
    FinDeCadena: String;
    constructor Crear;
    destructor Destroy; override;
    procedure IniciarActualizacion;
    procedure FinalizarActualizacion;
    procedure GuardarConfiguracion(TipoDeGuardado: TTipoDeGuardado; GuardarEn: String);
    procedure LoadSettings(TipoDeGuardado: TTipoDeGuardado; GuardarEn: String);
    procedure Abrir;
    procedure Cerrar;
    procedure MostrarDialogoDeConfiguracion;
    function CaracteresRecibidos: Integer;
    function CaracteresEnviados: Integer;
    function Indicadores: TComSignals;
    function BaderasDeEstado: TComStateFlags;
    procedure EstablecerDTR(OnOff: Boolean);
    procedure EstablecerRTS(OnOff: Boolean);
    procedure EstablecerXonXoff(OnOff: Boolean);
    procedure EstablecerBreak(OnOff: Boolean);
    procedure LimpiarBuffer(Input, Output: Boolean);
    function LastErrors: TComErrors;
    function EnviarBuffer(const Buffer; Longitud: Integer): Integer;
    function EnviarCadena(const Str: string): Integer;
    function RecibirBuffer(var Buffer; Longitud: Integer): Integer;
    function RecibirCadena(var Str: string; Longitud: Integer): Integer;
    function EnviarBufferAsincronicamente(const Buffer; Count: Integer; var AsyncPtr: PAsync): Integer;
    function EnviarCadenaAsincronicamente(Str: string; var AsyncPtr: PAsync): Integer;
    function RecibirBufferAsincronicamente(var Buffer; Count: Integer; var AsyncPtr: PAsync): Integer;
    function RecibirCadenaAsincronicamente(var Str: string; Count: Integer; var AsyncPtr: PAsync): Integer;
    function EsperarFinDeOperacionAsincronica(var AsyncPtr: PAsync): Integer;
    function OperacionAsincronicaCompletada(AsyncPtr: PAsync): Boolean;
    procedure EsperarPorUnEvento(var Events: TComEvents; StopEvent: THandle; Timeout: Integer);
    procedure AbortarTodasLasOperacionesAsincronicas;
    procedure EnviarCaracter(Ch: Char);
    property HandleDelPuerto: THandle read FHandle;
    property TriggersOnRxChar: Boolean read GetTriggersOnRxChar write SetTriggersOnRxChar;
    property PrioridadDelSubprocesoMonitor: TThreadPriority read FEventThreadPriority write SetEventThreadPriority;
    property StoredProps: TStoredProps read FStoredProps write FStoredProps;
    property Abierto: Boolean read FConnected write SetConnected default False;
    property Velocidad: TBaudRate read FBaudRate write SetBaudRate;
    property VelocidadPersonalizada: Integer read FCustomBaudRate write SetCustomBaudRate;
    property Puerto: TPort read FPort write SetPort;
    property Paridad: TComParity read FParity write SetParity;
    property BitsDeParada: TStopBits read FStopBits write SetStopBits;
    property CantidadDeBits: TDataBits read FDataBits write SetDataBits;
    property DiscardNull: Boolean read FDiscardNull write SetDiscardNull default False;
    property EventChar: Char read FEventChar write SetEventChar default #0;
    property Events: TComEvents read FEvents write FEvents;
    property Buffer: TComBuffer read FBuffer write SetBuffer;
    property ControlDeFlujo: TComFlowControl read FFlowControl write SetFlowControl;
    property Timeouts: TComTimeouts read FTimeouts write SetTimeouts;
    property SyncMethod: TSyncMethod read FSyncMethod write SetSyncMethod default smThreadSync;

    property OnAfterOpen: TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnAfterClose: TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnBeforeOpen: TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnBeforeClose: TNotifyEvent read FOnBeforeClose write FOnBeforeClose;
    property OnRxChar: TRxCharEvent read FOnRxChar write FOnRxChar;
    property OnRxBuf: TRxBufEvent read FOnRxBuf write FOnRxBuf;
    property OnTxEmpty: TNotifyEvent read FOnTxEmpty write FOnTxEmpty;
    property OnBreak: TNotifyEvent read FOnBreak write FOnBreak;
    property OnRing: TNotifyEvent read FOnRing write FOnRing;
    property OnCTSChange: TComSignalEvent read FOnCTSChange write FOnCTSChange;
    property OnDSRChange: TComSignalEvent read FOnDSRChange write FOnDSRChange;
    property OnRLSDChange: TComSignalEvent read FOnRLSDChange write FOnRLSDChange;
    property OnRxFlag: TNotifyEvent read FOnRxFlag write FOnRxFlag;
    property OnError: TComErrorEvent read FOnError write FOnError;
    property OnRx80Full: TNotifyEvent read FOnRx80Full write FOnRx80Full;
  end;


  TComStrEvent = procedure(Sender: TObject; const Str: string) of object;
  TCustPacketEvent = procedure(Sender: TObject; const Str: string; var Pos: Integer) of object;

//------------------------------------------------------------------------------
// Clase para el manejo de las exepciones del objeto.
//------------------------------------------------------------------------------
  EComPort = class(Exception)
  private
    FWinCode: Integer;
    FCode: Integer;
  public
    constructor Create(ACode: Integer; AWinCode: Integer);
    constructor CreateNoWinCode(ACode: Integer);
    property WinCode: Integer read FWinCode write FWinCode;
    property Code: Integer read FCode write FCode;
  end;

//------------------------------------------------------------------------------
//Procedimientos adicionales.
//------------------------------------------------------------------------------
procedure InitAsync(var AsyncPtr: PAsync);
procedure DoneAsync(var AsyncPtr: PAsync);
function PuertosComExistentes(Ports: TStrings): Integer;

//------------------------------------------------------------------------------
//Funciones para la conversión de datos.
//------------------------------------------------------------------------------
function StrToBaudRate(Str: string): TBaudRate;
function StrToStopBits(Str: string): TStopBits;
function StrToDataBits(Str: string): TDataBits;
function StrToParity(Str: string): TParityBits;
function StrToFlowControl(Str: string): TFlowControl;
function BaudRateToStr(BaudRate: TBaudRate): string;
function StopBitsToStr(StopBits: TStopBits): string;
function DataBitsToStr(DataBits: TDataBits): string;
function ParityToStr(Parity: TParityBits): string;
function FlowControlToStr(FlowControl: TFlowControl): string;

const WaitInfinite = Integer(INFINITE);

//Códigos de los tipos de errores.
const CError_OpenFailed      = 1;
const CError_WriteFailed     = 2;
const CError_ReadFailed      = 3;
const CError_InvalidAsync    = 4;
const CError_PurgeFailed     = 5;
const CError_AsyncCheck      = 6;
const CError_SetStateFailed  = 7;
const CError_TimeoutsFailed  = 8;
const CError_SetupComFailed  = 9;
const CError_ClearComFailed  = 10;
const CError_ModemStatFailed = 11;
const CError_EscapeComFailed = 12;
const CError_TransmitFailed  = 13;
const CError_ConnChangeProp  = 14;
const CError_EnumPortsFailed = 15;
const CError_StoreFailed     = 16;
const CError_LoadFailed      = 17;
const CError_RegFailed       = 18;
const CError_LedStateFailed  = 19;
const CError_ThreadCreated   = 20;
const CError_WaitFailed      = 21;
const CError_RegError        = 22;
 
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

implementation

uses Controls, Forms, WinSpool, PuertoComConfiguracion;

var ComErrorMessages: array[1..23] of widestring;

//------------------------------------------------------------------------------
// Constantes auxiliares no definidad en Windows.pas
//------------------------------------------------------------------------------
const
  dcb_Binary           = $00000001;
  dcb_Parity           = $00000002;
  dcb_OutxCTSFlow      = $00000004;
  dcb_OutxDSRFlow      = $00000008;
  dcb_DTRControl       = $00000030;
  dcb_DSRSensivity     = $00000040;
  dcb_TxContinueOnXoff = $00000080;
  dcb_OutX             = $00000100;
  dcb_InX              = $00000200;
  dcb_ErrorChar        = $00000400;
  dcb_Null             = $00000800;
  dcb_RTSControl       = $00003000;
  dcb_AbortOnError     = $00004000;

//------------------------------------------------------------------------------
// Mensaje de ventana com port.
//------------------------------------------------------------------------------
const CM_COMPORT = WM_USER + 1;

////////////////////////////////////////////////////////////////////////////////
/////////////       Funciones auxiliares y procedimientos.   ///////////////////
////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------------------------------
// Convierte de TComEvents a Integer.
//------------------------------------------------------------------------------
function EventsToInt(const Events: TComEvents): Integer;
begin
Result := 0;
if evRxChar in Events then Result := Result or EV_RXCHAR;
if evRxFlag in Events then Result := Result or EV_RXFLAG;
if evTxEmpty in Events then Result := Result or EV_TXEMPTY;
if evRing in Events then Result := Result or EV_RING;
if evCTS in Events then Result := Result or EV_CTS;
if evDSR in Events then Result := Result or EV_DSR;
if evRLSD in Events then Result := Result or EV_RLSD;
if evError in Events then Result := Result or EV_ERR;
if evBreak in Events then Result := Result or EV_BREAK;
if evRx80Full in Events then Result := Result or EV_RX80FULL;
end;

//------------------------------------------------------------------------------
// Convierte de Integer a TComEvents.
//------------------------------------------------------------------------------
function IntToEvents(Mask: Integer): TComEvents;
begin
Result := [];
if (EV_RXCHAR and Mask) <> 0 then Result := Result + [evRxChar];
if (EV_TXEMPTY and Mask) <> 0 then Result := Result + [evTxEmpty];
if (EV_BREAK and Mask) <> 0 then Result := Result + [evBreak];
if (EV_RING and Mask) <> 0 then Result := Result + [evRing];
if (EV_CTS and Mask) <> 0 then Result := Result + [evCTS];
if (EV_DSR and Mask) <> 0 then Result := Result + [evDSR];
if (EV_RXFLAG and Mask) <> 0 then Result := Result + [evRxFlag];
if (EV_RLSD and Mask) <> 0 then Result := Result + [evRLSD];
if (EV_ERR and Mask) <> 0 then Result := Result + [evError];
if (EV_RX80FULL and Mask) <> 0 then Result := Result + [evRx80Full];
end;

////////////////////////////////////////////////////////////////////////////////
//////    Métodos de la clase del subproceso que monitorea los eventos.   //////
////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------------------------------
// Crea el subproceso que monitorea.
//------------------------------------------------------------------------------
constructor TComThread.Create(AComPort: TPuertoCOM);
begin
inherited Create(True);
FStopEvent := CreateEvent(nil, True, False, nil);                      //Crea un evento.
FComPort := AComPort;
Priority := FComPort.PrioridadDelSubprocesoMonitor;                    //Establece la prioridad del subproceso.
SetCommMask(FComPort.HandleDelPuerto, EventsToInt(FComPort.Events));   //Selecciona el evento que será monitoreado.
Resume;                                                                //Ejexcuta el subproceso.
end;

//------------------------------------------------------------------------------
// Destruye el subproceso.
//------------------------------------------------------------------------------
destructor TComThread.Destroy;
begin
Stop;
inherited Destroy;
end;

//------------------------------------------------------------------------------
// Estas son las acciones que realiza el subproceso.
// Entra en un ciclo donde se comprueba constantemente la ocurrencia de eventos.
//------------------------------------------------------------------------------
procedure TComThread.Execute;
var EventHandles: array[0..1] of THandle;
    Overlapped: TOverlapped;
    Signaled, BytesTrans, Mask: DWORD;
begin
FillChar(Overlapped, SizeOf(Overlapped), 0);
Overlapped.hEvent := CreateEvent(nil, True, True, nil);
EventHandles[0] := FStopEvent;
EventHandles[1] := Overlapped.hEvent;
repeat
   //Espera a que ocurra un evento de puerto serie.
   WaitCommEvent(FComPort.HandleDelPuerto, Mask, @Overlapped);
   Signaled := WaitForMultipleObjects(2, @EventHandles, False, INFINITE);
   //Si ocurre un evento, lo despacha.
   if (Signaled = WAIT_OBJECT_0 + 1) and
      GetOverlappedResult(FComPort.HandleDelPuerto, Overlapped, BytesTrans, False) then
      begin
      FEvents := IntToEvents(Mask);
      DispatchComMsg;
      end;
until Signaled <> (WAIT_OBJECT_0 + 1);
//Limpia los buffers.
SetCommMask(FComPort.HandleDelPuerto, 0);
PurgeComm(FComPort.HandleDelPuerto, PURGE_TXCLEAR or PURGE_RXCLEAR);
CloseHandle(Overlapped.hEvent);
CloseHandle(FStopEvent);
end;

//-----------------------------------------------------------------------------
// Detiene el subproceso.
//-----------------------------------------------------------------------------
procedure TComThread.Stop;
begin
SetEvent(FStopEvent);
Sleep(0);
end;

//-----------------------------------------------------------------------------
// Despacha los eventos.
//-----------------------------------------------------------------------------
procedure TComThread.DispatchComMsg;
begin
case FComPort.SyncMethod of
     smThreadSync: Synchronize(DoEvents);
     smWindowSync: SendEvents;
     smNone: DoEvents;    
     end;
end;

//-----------------------------------------------------------------------------
// Envia eventos al objeto "TPuertoCOM" usando mensajes de ventana.
//-----------------------------------------------------------------------------
procedure TComThread.SendEvents;
begin
if evError in FEvents then SendMessage(FComPort.FWindow, CM_COMPORT, EV_ERR, 0);
if evRxChar in FEvents then SendMessage(FComPort.FWindow, CM_COMPORT, EV_RXCHAR, 0);
if evTxEmpty in FEvents then SendMessage(FComPort.FWindow, CM_COMPORT, EV_TXEMPTY, 0);
if evBreak in FEvents then SendMessage(FComPort.FWindow, CM_COMPORT, EV_BREAK, 0);
if evRing in FEvents then SendMessage(FComPort.FWindow, CM_COMPORT, EV_RING, 0);
if evCTS in FEvents then SendMessage(FComPort.FWindow, CM_COMPORT, EV_CTS, 0);
if evDSR in FEvents then SendMessage(FComPort.FWindow, CM_COMPORT, EV_DSR, 0);
if evRxFlag in FEvents then SendMessage(FComPort.FWindow, CM_COMPORT, EV_RXFLAG, 0);
if evRing in FEvents then SendMessage(FComPort.FWindow, CM_COMPORT, EV_RLSD, 0);
if evRx80Full in FEvents then SendMessage(FComPort.FWindow, CM_COMPORT, EV_RX80FULL, 0);
end;

//-----------------------------------------------------------------------------
// Llama eventos.
//-----------------------------------------------------------------------------
procedure TComThread.DoEvents;
begin
if evError in FEvents then FComPort.CallError;
if evRxChar in FEvents then FComPort.CallRxChar;
if evTxEmpty in FEvents then FComPort.CallTxEmpty;
if evBreak in FEvents then FComPort.CallBreak;
if evRing in FEvents then FComPort.CallRing;
if evCTS in FEvents then FComPort.CallCTSChange;
if evDSR in FEvents then FComPort.CallDSRChange;
if evRxFlag in FEvents then FComPort.CallRxFlag;
if evRLSD in FEvents then FComPort.CallRLSDChange;
if evRx80Full in FEvents then FComPort.CallRx80Full;
end;

////////////////////////////////////////////////////////////////////////////////
//////////       Métodos de la clase "TComTimeouts".             ///////////////
////////////////////////////////////////////////////////////////////////////////

//-----------------------------------------------------------------------------
// Crea el objeto.
//-----------------------------------------------------------------------------
constructor TComTimeouts.Create;
begin
inherited Create;
FReadInterval := -1;
FWriteTotalM := 100;
FWriteTotalC := 1000;
end;

//-----------------------------------------------------------------------------
// Copia las propiedades hacia otra clase.
//-----------------------------------------------------------------------------
procedure TComTimeouts.AssignTo(Dest: TPersistent);
begin
if Dest is TComTimeouts then
   begin
   with TComTimeouts(Dest) do
        begin
        FReadInterval := Self.ReadInterval;
        FReadTotalM   := Self.ReadTotalMultiplier;
        FReadTotalC   := Self.ReadTotalConstant;
        FWriteTotalM  := Self.WriteTotalMultiplier;
        FWriteTotalC  := Self.WriteTotalConstant;
        end;
   end
else
   inherited AssignTo(Dest);
end;

//-----------------------------------------------------------------------------
// Selecciona el propio TPuertoCOM.
//-----------------------------------------------------------------------------
procedure TComTimeouts.SetComPort(const AComPort: TPuertoCOM);
begin
FComPort := AComPort;
end;

//-----------------------------------------------------------------------------
// Establece el intervalo de lectura.
//-----------------------------------------------------------------------------
procedure TComTimeouts.SetReadInterval(const Value: Integer);
begin
if Value <> FReadInterval then
   begin
   FReadInterval := Value;
   if FComPort <> nil then FComPort.ApplyTimeouts;
   end;
end;

//-----------------------------------------------------------------------------
// Establece lectura total.
//-----------------------------------------------------------------------------
procedure TComTimeouts.SetReadTotalC(const Value: Integer);
begin
if Value <> FReadTotalC then
   begin
   FReadTotalC := Value;
   if FComPort <> nil then FComPort.ApplyTimeouts;
   end;
end;

//-----------------------------------------------------------------------------
// Establece lectura total de multiplicador.
//-----------------------------------------------------------------------------
procedure TComTimeouts.SetReadTotalM(const Value: Integer);
begin
if Value <> FReadTotalM then
   begin
   FReadTotalM := Value;
   if FComPort <> nil then FComPort.ApplyTimeouts;
   end;
end;

//-----------------------------------------------------------------------------
// Establece escritura total de constante.
//-----------------------------------------------------------------------------
procedure TComTimeouts.SetWriteTotalC(const Value: Integer);
begin
if Value <> FWriteTotalC then
   begin
   FWriteTotalC := Value;
   if FComPort <> nil then FComPort.ApplyTimeouts;
   end;
end;

//-----------------------------------------------------------------------------
// Establece escritura total de multiplicador.
//-----------------------------------------------------------------------------
procedure TComTimeouts.SetWriteTotalM(const Value: Integer);
begin
if Value <> FWriteTotalM then
   begin
   FWriteTotalM := Value;
   if FComPort <> nil then FComPort.ApplyTimeouts;
   end;
end;

////////////////////////////////////////////////////////////////////////////////
///////////////  Métodos de la clase TComFlowControl      //////////////////////
////////////////////////////////////////////////////////////////////////////////

//-----------------------------------------------------------------------------
// Crea el objeto.
//-----------------------------------------------------------------------------
constructor TComFlowControl.Create;
begin
inherited Create;
FXonChar := #17;
FXoffChar := #19;
end;

//-----------------------------------------------------------------------------
// Copia las propiedades a otra clase.
//-----------------------------------------------------------------------------
procedure TComFlowControl.AssignTo(Dest: TPersistent);
begin
if Dest is TComFlowControl then
   begin
   with TComFlowControl(Dest) do
        begin
        FOutCTSFlow       := Self.OutCTSFlow;
        FOutDSRFlow       := Self.OutDSRFlow;
        FControlDTR       := Self.ControlDTR;
        FControlRTS       := Self.ControlRTS;
        FXonXoffOut       := Self.XonXoffOut;
        FXonXoffIn        := Self.XonXoffIn;
        FTxContinueOnXoff := Self.TxContinueOnXoff;
        FDSRSensitivity   := Self.DSRSensitivity;
        FXonChar          := Self.XonChar;
        FXoffChar         := Self.XoffChar;
        end
   end
else
   inherited AssignTo(Dest);
end;

//-----------------------------------------------------------------------------
// Selecciona el propio TPuertoCOM de esta clase.
//-----------------------------------------------------------------------------
procedure TComFlowControl.SetComPort(const AComPort: TPuertoCOM);
begin
FComPort := AComPort;
end;

//-----------------------------------------------------------------------------
// Establece el control de flujo de entrada para DTR (data-terminal-ready)
//-----------------------------------------------------------------------------
procedure TComFlowControl.SetControlDTR(const Value: TDTRFlowControl);
begin
if Value <> FControlDTR then
   begin
   FControlDTR := Value;
   if FComPort <> nil then FComPort.ApplyDCB;
   end;
end;

//-----------------------------------------------------------------------------
// Establece el control de flujo de entrada para RTS (request-to-send)
//-----------------------------------------------------------------------------
procedure TComFlowControl.SetControlRTS(const Value: TRTSFlowControl);
begin
if Value <> FControlRTS then
   begin
   FControlRTS := Value;
   if FComPort <> nil then FComPort.ApplyDCB;
   end;
end;

//-----------------------------------------------------------------------------
// Establece el control de flujo de salida para CTS (clear-to-send)
//-----------------------------------------------------------------------------
procedure TComFlowControl.SetOutCTSFlow(const Value: Boolean);
begin
if Value <> FOutCTSFlow then
   begin
   FOutCTSFlow := Value;
   if FComPort <> nil then FComPort.ApplyDCB;
   end;
end;

//-----------------------------------------------------------------------------
// Establece el control de flujo de salida para DSR (data-set-ready)
//-----------------------------------------------------------------------------
procedure TComFlowControl.SetOutDSRFlow(const Value: Boolean);
begin
if Value <> FOutDSRFlow then
   begin
   FOutDSRFlow := Value;
   if FComPort <> nil then FComPort.ApplyDCB;
   end;
end;

//-----------------------------------------------------------------------------
// Establece el control de flujo de entrada por software.
//-----------------------------------------------------------------------------
procedure TComFlowControl.SetXonXoffIn(const Value: Boolean);
begin
if Value <> FXonXoffIn then
   begin
   FXonXoffIn := Value;
   if FComPort <> nil then FComPort.ApplyDCB;
   end;
end;

//-----------------------------------------------------------------------------
// Establece el control de flujo de salida por software.
//-----------------------------------------------------------------------------
procedure TComFlowControl.SetXonXoffOut(const Value: Boolean);
begin
if Value <> FXonXoffOut then
   begin
   FXonXoffOut := Value;
   if FComPort <> nil then FComPort.ApplyDCB;
   end;
end;

//-----------------------------------------------------------------------------
// Establece la sensibilidad del DSR.
//-----------------------------------------------------------------------------
procedure TComFlowControl.SetDSRSensitivity(const Value: Boolean);
begin
  if Value <> FDSRSensitivity then
  begin
    FDSRSensitivity := Value;
    if FComPort <> nil then
      FComPort.ApplyDCB;
  end;
end;

//-----------------------------------------------------------------------------
// Establece la transferencia continua cuando Xoff es enviado.
//-----------------------------------------------------------------------------
procedure TComFlowControl.SetTxContinueOnXoff(const Value: Boolean);
begin
if Value <> FTxContinueOnXoff then
   begin
   FTxContinueOnXoff := Value;
   if FComPort <> nil then FComPort.ApplyDCB;
   end;
end;

//-----------------------------------------------------------------------------
// Establece el caracter Xon.
//-----------------------------------------------------------------------------
procedure TComFlowControl.SetXonChar(const Value: Char);
begin
if Value <> FXonChar then
   begin
   FXonChar := Value;
   if FComPort <> nil then FComPort.ApplyDCB;
   end;
end;

//-----------------------------------------------------------------------------
// Establece el caracter Xoff.
//-----------------------------------------------------------------------------
procedure TComFlowControl.SetXoffChar(const Value: Char);
begin
if Value <> FXoffChar then
   begin
   FXoffChar := Value;
   if FComPort <> nil then FComPort.ApplyDCB;
   end;
end;

//-----------------------------------------------------------------------------
// Obtiene el flujo de control común. (common flow control)
//-----------------------------------------------------------------------------
function TComFlowControl.GetFlowControl: TFlowControl;
begin
if (FControlRTS = rtsHandshake) and (FOutCTSFlow) and
   (not FXonXoffIn) and (not FXonXoffOut) then
   Result := fcHardware
else
   if (FControlRTS = rtsDesactivado) and (not FOutCTSFlow) and
      (FXonXoffIn) and (FXonXoffOut) then
      Result := fcSoftware
   else
      if (FControlRTS = rtsDesactivado) and (not FOutCTSFlow) and
         (not FXonXoffIn) and (not FXonXoffOut) then
         Result := fcNinguno
      else
         Result := fcDeCostumbre;
end;

//-----------------------------------------------------------------------------
// Establece el flujo de control común. (common flow control)
//-----------------------------------------------------------------------------
procedure TComFlowControl.SetFlowControl(const Value: TFlowControl);
begin
if Value <> fcDeCostumbre then
   begin
   FControlRTS := rtsDesactivado;
   FOutCTSFlow := False;
   FXonXoffIn := False;
   FXonXoffOut := False;
   case Value of
        fcHardware: begin
                    FControlRTS := rtsHandshake;
                    FOutCTSFlow := True;
                    end;
        fcSoftware: begin
                    FXonXoffIn := True;
                    FXonXoffOut := True;
                    end;
        end;
   end;
if FComPort <> nil then FComPort.ApplyDCB;
end;

////////////////////////////////////////////////////////////////////////////////
///////////////     Métodos de la clase TComParity.      ///////////////////////
////////////////////////////////////////////////////////////////////////////////

//-----------------------------------------------------------------------------
// Crea el objeto.
//-----------------------------------------------------------------------------
constructor TComParity.Create;
begin
inherited Create;
FBits := prNinguno;
end;

//-----------------------------------------------------------------------------
// Copia las propiedades a otra clase.
//-----------------------------------------------------------------------------
procedure TComParity.AssignTo(Dest: TPersistent);
begin
if Dest is TComParity then
   begin
   with TComParity(Dest) do
        begin
        FBits        := Self.Bits;
        FCheck       := Self.Check;
        FReplace     := Self.Replace;
        FReplaceChar := Self.ReplaceChar;
        end
   end
else
   inherited AssignTo(Dest);
end;

//-----------------------------------------------------------------------------
// Selecciona el TPuertoCOM de esta misma clase.
//-----------------------------------------------------------------------------
procedure TComParity.SetComPort(const AComPort: TPuertoCOM);
begin
FComPort := AComPort;
end;

//-----------------------------------------------------------------------------
// Establece el bit de paridad. (parity bits)
//-----------------------------------------------------------------------------
procedure TComParity.SetBits(const Value: TParityBits);
begin
if Value <> FBits then
   begin
   FBits := Value;
   if FComPort <> nil then FComPort.ApplyDCB;
   end;
end;

//-----------------------------------------------------------------------------
// Establece el chequeo de paridad (check parity)
//-----------------------------------------------------------------------------
procedure TComParity.SetCheck(const Value: Boolean);
begin
if Value <> FCheck then
   begin
   FCheck := Value;
   if FComPort <> nil then FComPort.ApplyDCB;
   end;
end;

//-----------------------------------------------------------------------------
// Establece el reemplazo de error de paridad.
//-----------------------------------------------------------------------------
procedure TComParity.SetReplace(const Value: Boolean);
begin
if Value <> FReplace then
   begin
   FReplace := Value;
   if FComPort <> nil then FComPort.ApplyDCB;
   end;
end;

//-----------------------------------------------------------------------------
// Establece el remplazo de caracter.
//-----------------------------------------------------------------------------
procedure TComParity.SetReplaceChar(const Value: Char);
begin
if Value <> FReplaceChar then
   begin
   FReplaceChar := Value;
   if FComPort <> nil then FComPort.ApplyDCB;
   end;
end;

////////////////////////////////////////////////////////////////////////////////
//////////////////    Métodos de la clase TComBuffer.   ////////////////////////
////////////////////////////////////////////////////////////////////////////////

//-----------------------------------------------------------------------------
// Crea el objeto.
//-----------------------------------------------------------------------------
constructor TComBuffer.Create;
begin
inherited Create;
FInputSize := 1024;
FOutputSize := 1024;
end;

//-----------------------------------------------------------------------------
// Copia la spropiedades a otra clase.
//-----------------------------------------------------------------------------
procedure TComBuffer.AssignTo(Dest: TPersistent);
begin
if Dest is TComBuffer then
   begin
   with TComBuffer(Dest) do
        begin
        FOutputSize  := Self.OutputSize;
        FInputSize   := Self.InputSize;
        end;
   end
else
   inherited AssignTo(Dest);
end;

//-----------------------------------------------------------------------------
// Selecciona el propio TPuertoCOM.
//-----------------------------------------------------------------------------
procedure TComBuffer.SetComPort(const AComPort: TPuertoCOM);
begin
FComPort := AComPort;
end;

//-----------------------------------------------------------------------------
// Establece el tamaño del buffer de entrada.
//-----------------------------------------------------------------------------
procedure TComBuffer.SetInputSize(const Value: Integer);
begin
if Value <> FInputSize then
   begin
   FInputSize := Value;
   if (FInputSize mod 2) = 1 then Dec(FInputSize);
   if FComPort <> nil then FComPort.ApplyBuffer;
   end;
end;

//-----------------------------------------------------------------------------
// Establece el tamaño del buffer de salida.
//-----------------------------------------------------------------------------
procedure TComBuffer.SetOutputSize(const Value: Integer);
begin
if Value <> FOutputSize then
   begin
   FOutputSize := Value;
   if (FOutputSize mod 2) = 1 then Dec(FOutputSize);
   if FComPort <> nil then FComPort.ApplyBuffer;
   end; 
end;

////////////////////////////////////////////////////////////////////////////////
////////////////  Métodos de la clase TPuertoCOM       /////////////////////
////////////////////////////////////////////////////////////////////////////////

//-----------------------------------------------------------------------------
// Crea el objeto.
//-----------------------------------------------------------------------------
constructor TPuertoCOM.Crear;
begin
inherited Create;
FinDeCadena := '';
FTriggersOnRxChar := True;
FEventThreadPriority := tpNormal;
FBaudRate := br9600;
FCustomBaudRate := 9600;
FPort := 'COM1';
FStopBits := sbUno;
FDataBits := dbOcho;
FEvents := [evRxChar, evTxEmpty, evRxFlag, evRing, evBreak,
            evCTS, evDSR, evError, evRLSD, evRx80Full];
FHandle := INVALID_HANDLE_VALUE;
FStoredProps := [spBasic];
FParity := TComParity.Create;
FParity.SetComPort(Self);
FFlowControl := TComFlowControl.Create;
FFlowControl.SetComPort(Self);
FTimeouts := TComTimeouts.Create;
FTimeouts.SetComPort(Self);
FBuffer := TComBuffer.Create;
FBuffer.SetComPort(Self);
end;

//-----------------------------------------------------------------------------
// Destruye el objeto.
//-----------------------------------------------------------------------------
destructor TPuertoCOM.Destroy;
begin
Cerrar;
FBuffer.Free;
FFlowControl.Free;
FTimeouts.Free;
FParity.Free;
inherited Destroy;
end;

//-----------------------------------------------------------------------------
// Crea un manejador para el puerto serie.
// Lo que hace es abrir el puerto serie mediante el API de Windows y
// este le devuelve un handle que apunta al dispositivo.
//-----------------------------------------------------------------------------
procedure TPuertoCOM.CreateHandle;
begin
FHandle := CreateFile(PChar('\\.\' + FPort),
                      GENERIC_READ or GENERIC_WRITE,
                      0,
                      nil,
                      OPEN_EXISTING,
                      FILE_FLAG_OVERLAPPED,
                      0);
if FHandle = INVALID_HANDLE_VALUE then
   raise EComPort.Create(CError_OpenFailed, GetLastError);
end;

//-----------------------------------------------------------------------------
// Destruye el manejador del puerto serie.
//-----------------------------------------------------------------------------
procedure TPuertoCOM.DestroyHandle;
begin
if FHandle <> INVALID_HANDLE_VALUE then CloseHandle(FHandle);
end;

//-----------------------------------------------------------------------------
// Llama los eventos que han sido despachados usando mensajes de ventana.
//-----------------------------------------------------------------------------
procedure TPuertoCOM.WindowMethod(var Message: TMessage);
begin
with Message do
     if Msg = CM_COMPORT then
        try
           if InSendMessage then
           ReplyMessage(0);
           if FConnected then
              case wParam of
                   EV_RXCHAR:   CallRxChar;
                   EV_TXEMPTY:  CallTxEmpty;
                   EV_BREAK:    CallBreak;
                   EV_RING:     CallRing;
                   EV_CTS:      CallCTSChange;
                   EV_DSR:      CallDSRChange;
                   EV_RXFLAG:   CallRxFlag;
                   EV_RLSD:     CallRLSDChange;
                   EV_ERR:      CallError;
                   EV_RX80FULL: CallRx80Full;
                   end
        except
           Application.HandleException(Self);
        end
     else
        Result := DefWindowProc(FWindow, Msg, wParam, lParam);
end;

//-----------------------------------------------------------------------------
// Impide cambios desde la aplicación en tiempo de ejecución.
//-----------------------------------------------------------------------------
procedure TPuertoCOM.IniciarActualizacion;
begin
FUpdateCount := FUpdateCount + 1;
end;

//-----------------------------------------------------------------------------
// Aplica los cambios creados desde la llamada a BeginUpdate.
//-----------------------------------------------------------------------------
procedure TPuertoCOM.FinalizarActualizacion;
begin
if FUpdateCount > 0 then
   begin
   FUpdateCount := FUpdateCount - 1;
   if FUpdateCount = 0 then SetupComPort;
   end;
end;

//-----------------------------------------------------------------------------
// Abre el puerto de comunicaciones.
//-----------------------------------------------------------------------------
procedure TPuertoCOM.Abrir;
begin
//Si el puerto no esta conectado:
if not FConnected then
   begin
   CallBeforeOpen;
   CreateHandle;             //Esta función abre el puerto y devuelve el handle del mismo.
   FConnected := True;       //Indica que se ha conectado.
   try
      SetupComPort;          //Inicializa el puerto.
   except
      DestroyHandle;
      FConnected := False;   //Si no se ha podido abrir el puerto, lo indica.
      raise;
   end;
   if (FEvents = []) then
      FThreadCreated := False
   else
      begin
      if (FSyncMethod = smWindowSync) then
      {$IFDEF DELPHI_6_OR_HIGHER}
      {$WARN SYMBOL_DEPRECATED OFF}
      {$ENDIF}
      FWindow := AllocateHWnd(WindowMethod);
      {$IFDEF DELPHI_6_OR_HIGHER}
      {$WARN SYMBOL_DEPRECATED ON}
     {$ENDIF}
      FEventThread := TComThread.Create(Self);
      FThreadCreated := True;
      end;
   CallAfterOpen;            //El puerto ha sido abierto sin problemas.
   end;
end;

//-----------------------------------------------------------------------------
// Cierra el puerto.
//-----------------------------------------------------------------------------
procedure TPuertoCOM.Cerrar;
begin
if FConnected then                          //Si el puerto esta conectado.
   begin
   CallBeforeClose;
   AbortarTodasLasOperacionesAsincronicas;  //Aborta todas los operaciones pendientes.
   if FThreadCreated then                   //Detiene el monitoreo de eventos.
      begin
      FEventThread.Free;
      FThreadCreated := False;
      if FSyncMethod = smWindowSync then
      {$IFDEF DELPHI_6_OR_HIGHER}
      {$WARN SYMBOL_DEPRECATED OFF}
      {$ENDIF}
         DeallocateHWnd(FWindow);
      {$IFDEF DELPHI_6_OR_HIGHER}
      {$WARN SYMBOL_DEPRECATED ON}
      {$ENDIF}
      end;
   DestroyHandle;                           //Cierra el puerto.
   FConnected := False;
   CallAfterClose;                          //El puerto ha sido cerreado.
   end;
end;

//-----------------------------------------------------------------------------
// Aplica las propiedades del puerto.
//-----------------------------------------------------------------------------
procedure TPuertoCOM.ApplyDCB;
const CParityBits: array[TParityBits] of Integer =
      (NOPARITY, ODDPARITY, EVENPARITY, MARKPARITY, SPACEPARITY);
      CStopBits: array[TStopBits] of Integer =
      (ONESTOPBIT, ONE5STOPBITS, TWOSTOPBITS);
      CBaudRate: array[TBaudRate] of Integer =
      (0, CBR_110, CBR_300, CBR_600, CBR_1200, CBR_2400, CBR_4800, CBR_9600,
       CBR_14400, CBR_19200, CBR_38400, CBR_56000, CBR_57600, CBR_115200,
       CBR_128000, CBR_256000);
      CDataBits: array[TDataBits] of Integer = (5, 6, 7, 8);
      CControlRTS: array[TRTSFlowControl] of Integer =
                   (RTS_CONTROL_DISABLE shl 12,
                    RTS_CONTROL_ENABLE shl 12,
                    RTS_CONTROL_HANDSHAKE shl 12,
                    RTS_CONTROL_TOGGLE shl 12);
      CControlDTR: array[TDTRFlowControl] of Integer =
                   (DTR_CONTROL_DISABLE shl 4,
                    DTR_CONTROL_ENABLE shl 4,
                    DTR_CONTROL_HANDSHAKE shl 4);
var DCB: TDCB;

begin
if FConnected and (FUpdateCount = 0) then
   begin
   DCB.DCBlength := SizeOf(TDCB);
   DCB.XonLim := FBuffer.InputSize div 4;
   DCB.XoffLim := DCB.XonLim;
   DCB.EvtChar := Char(FEventChar);
   DCB.Flags := dcb_Binary;
   if FDiscardNull then DCB.Flags := DCB.Flags or dcb_Null;
   with FFlowControl do
        begin
        DCB.XonChar := XonChar;
        DCB.XoffChar := XoffChar;
        if OutCTSFlow then DCB.Flags := DCB.Flags or dcb_OutxCTSFlow;
        if OutDSRFlow then DCB.Flags := DCB.Flags or dcb_OutxDSRFlow;
        DCB.Flags := DCB.Flags or CControlDTR[ControlDTR] or CControlRTS[ControlRTS];
        if XonXoffOut then DCB.Flags := DCB.Flags or dcb_OutX;
        if XonXoffIn then DCB.Flags := DCB.Flags or dcb_InX;
        if DSRSensitivity then DCB.Flags := DCB.Flags or dcb_DSRSensivity;
        if TxContinueOnXoff then DCB.Flags := DCB.Flags or dcb_TxContinueOnXoff;
        end;
   DCB.Parity := CParityBits[FParity.Bits];
   DCB.StopBits := CStopBits[FStopBits];
   if FBaudRate <> brDeCostumbre then
      DCB.BaudRate := CBaudRate[FBaudRate]
   else
      DCB.BaudRate := FCustomBaudRate;
   DCB.ByteSize := CDataBits[FDataBits];
   if FParity.Check then
      begin
      DCB.Flags := DCB.Flags or dcb_Parity;
      if FParity.Replace then
         begin
         DCB.Flags := DCB.Flags or dcb_ErrorChar;
         DCB.ErrorChar := Char(FParity.ReplaceChar);
         end;
      end;
   if not SetCommState(FHandle, DCB) then
      raise EComPort.Create(CError_SetStateFailed, GetLastError);
   end;
end;

//-----------------------------------------------------------------------------
// Aplica las propiedades de Timeout.
//-----------------------------------------------------------------------------
procedure TPuertoCOM.ApplyTimeouts;
var Timeouts: TCommTimeouts;

  function GetTOValue(const Value: Integer): DWORD;
  begin
  if Value = -1 then Result := MAXDWORD else Result := Value;
  end;

begin
//Si no esta conectado o esta dentro de un bloque BeginUpdate/EndUpdate
if FConnected and (FUpdateCount = 0) then
   begin
   Timeouts.ReadIntervalTimeout := GetTOValue(FTimeouts.ReadInterval);
   Timeouts.ReadTotalTimeoutMultiplier := GetTOValue(FTimeouts.ReadTotalMultiplier);
   Timeouts.ReadTotalTimeoutConstant := GetTOValue(FTimeouts.ReadTotalConstant);
   Timeouts.WriteTotalTimeoutMultiplier := GetTOValue(FTimeouts.WriteTotalMultiplier);
   Timeouts.WriteTotalTimeoutConstant := GetTOValue(FTimeouts.WriteTotalConstant);
   if not SetCommTimeouts(FHandle, Timeouts) then
      raise EComPort.Create(CError_TimeoutsFailed, GetLastError);
   end;
end;

//-----------------------------------------------------------------------------
// Aplica los buffers.
//-----------------------------------------------------------------------------
procedure TPuertoCOM.ApplyBuffer;
begin
if FConnected and (FUpdateCount = 0) then
   if not SetupComm(FHandle, FBuffer.InputSize, FBuffer.OutputSize) then
      raise EComPort.Create(CError_SetupComFailed, GetLastError);
end;

//-----------------------------------------------------------------------------
// Inicializa el puerto.
//-----------------------------------------------------------------------------
procedure TPuertoCOM.SetupComPort;
begin
ApplyBuffer;
ApplyDCB;
ApplyTimeouts;
end;

//-----------------------------------------------------------------------------
// Obtiene el número de Bytes en el buffer de entrada.
//-----------------------------------------------------------------------------
function TPuertoCOM.CaracteresRecibidos: Integer;
var Errors: DWORD;
    ComStat: TComStat;
begin
if not ClearCommError(FHandle, Errors, @ComStat) then
   raise EComPort.Create(CError_ClearComFailed, GetLastError);
Result := ComStat.cbInQue;
end;

//------------------------------------------------------------------------------
// Obtiene el número de Bytes en el buffer de salida.
//------------------------------------------------------------------------------
function TPuertoCOM.CaracteresEnviados: Integer;
var Errors: DWORD;
    ComStat: TComStat;
begin
if not ClearCommError(FHandle, Errors, @ComStat) then
   raise EComPort.Create(CError_ClearComFailed, GetLastError);
Result := ComStat.cbOutQue;
end;

//------------------------------------------------------------------------------
// Obtiene las señales que se encuentran en estado activado (high state).
//------------------------------------------------------------------------------
function TPuertoCOM.Indicadores: TComSignals;
var Status: DWORD;
begin
if not GetCommModemStatus(FHandle, Status) then
   raise EComPort.Create(CError_ModemStatFailed, GetLastError);
Result := [];
if (MS_CTS_ON and Status) <> 0 then Result := Result + [csCTS];
if (MS_DSR_ON and Status) <> 0 then Result := Result + [csDSR];
if (MS_RING_ON and Status) <> 0 then Result := Result + [csRing];
if (MS_RLSD_ON and Status) <> 0 then Result := Result + [csRLSD];
end;

//------------------------------------------------------------------------------
// Obtiene las banderas del estado del puerto.
//------------------------------------------------------------------------------
function TPuertoCOM.BaderasDeEstado: TComStateFlags;
var Errors: DWORD;
    ComStat: TComStat;
begin
if not ClearCommError(FHandle, Errors, @ComStat) then
   raise EComPort.Create(CError_ClearComFailed, GetLastError);
Result := ComStat.Flags;
end;

//------------------------------------------------------------------------------
// Establece la ruptura de linea por hardware.
//------------------------------------------------------------------------------
procedure TPuertoCOM.EstablecerBreak(OnOff: Boolean);
var Act: Integer;
begin
if OnOff then Act := Windows.SETBREAK else Act := Windows.CLRBREAK;
if not EscapeCommFunction(FHandle, Act) then
   raise EComPort.Create(CError_EscapeComFailed, GetLastError);
end;

//------------------------------------------------------------------------------
// Establece la señal DTR.
//------------------------------------------------------------------------------
procedure TPuertoCOM.EstablecerDTR(OnOff: Boolean);
var Act: DWORD;
begin
if OnOff then Act := Windows.SETDTR else Act := Windows.CLRDTR;
if not EscapeCommFunction(FHandle, Act) then
   raise EComPort.Create(CError_EscapeComFailed, GetLastError);
end;

//------------------------------------------------------------------------------
// Establece la señal RTS.
//------------------------------------------------------------------------------
procedure TPuertoCOM.EstablecerRTS(OnOff: Boolean);
var Act: DWORD;
begin
if OnOff then Act := Windows.SETRTS else Act := Windows.CLRRTS;
if not EscapeCommFunction(FHandle, Act) then
   raise EComPort.Create(CError_EscapeComFailed, GetLastError);
end;

//------------------------------------------------------------------------------
// Establece el estado XonXoff.
//------------------------------------------------------------------------------
procedure TPuertoCOM.EstablecerXonXoff(OnOff: Boolean);
var Act: DWORD;
begin
if OnOff then Act := Windows.SETXON else Act := Windows.SETXOFF;
if not EscapeCommFunction(FHandle, Act) then
   raise EComPort.Create(CError_EscapeComFailed, GetLastError);
end;

//------------------------------------------------------------------------------
// Limpia los buffers de entrada y salida.
//------------------------------------------------------------------------------
procedure TPuertoCOM.LimpiarBuffer(Input, Output: Boolean);
var Flag: DWORD;
begin
Flag := 0;
if Input then Flag := PURGE_RXCLEAR;
if Output then Flag := Flag or PURGE_TXCLEAR;
if not PurgeComm(FHandle, Flag) then
   raise EComPort.Create(CError_PurgeFailed, GetLastError);
end;

//------------------------------------------------------------------------------
// Retorna el último error ocurrido en el puerto.
//------------------------------------------------------------------------------
function TPuertoCOM.LastErrors: TComErrors;
var Errors: DWORD;
    ComStat: TComStat;
begin
if not ClearCommError(FHandle, Errors, @ComStat) then
   raise EComPort.Create(CError_ClearComFailed, GetLastError);
Result := [];
if (CE_FRAME and Errors) <> 0 then Result := Result + [ceFrame];
if ((CE_RXPARITY and Errors) <> 0) and FParity.Check then
   Result := Result + [ceRxParity];
if (CE_OVERRUN and Errors) <> 0 then Result := Result + [ceOverrun];
if (CE_RXOVER and Errors) <> 0 then Result := Result + [ceRxOver];
if (CE_TXFULL and Errors) <> 0 then Result := Result + [ceTxFull];
if (CE_BREAK and Errors) <> 0 then Result := Result + [ceBreak];
if (CE_IOE and Errors) <> 0 then Result := Result + [ceIO];
if (CE_MODE and Errors) <> 0 then Result := Result + [ceMode];
end;

//------------------------------------------------------------------------------
// Prepara la variable PAsync para una operación de lectura/escritura.
//------------------------------------------------------------------------------
procedure PrepareAsync(AKind: TOperationKind; const Buffer; Count: Integer; AsyncPtr: PAsync);
begin
with AsyncPtr^ do
     begin
     Kind := AKind;
     if Data <> nil then FreeMem(Data);
     GetMem(Data, Count);
     Move(Buffer, Data^, Count);
     Size := Count;
     end;
end;

//------------------------------------------------------------------------------
// Realiza una operación de escritura asincrónica.
//------------------------------------------------------------------------------
function TPuertoCOM.EnviarBufferAsincronicamente(const Buffer; Count: Integer; var AsyncPtr: PAsync): Integer;
var Success: Boolean;
    BytesTrans: DWORD;
begin
if AsyncPtr = nil then
   raise EComPort.CreateNoWinCode(CError_InvalidAsync);
PrepareAsync(okWrite, Buffer, Count, AsyncPtr);
Success := WriteFile(FHandle,
                     Buffer,
                     Count,
                     BytesTrans,
                     @AsyncPtr^.Overlapped) or
                     (GetLastError = ERROR_IO_PENDING);
if not Success then
   raise EComPort.Create(CError_WriteFailed, GetLastError);
Result := BytesTrans;
end;

//------------------------------------------------------------------------------
// Realiza una operación de escritura asincrónica.
//------------------------------------------------------------------------------
function TPuertoCOM.EnviarBuffer(const Buffer; Longitud: Integer): Integer;
var AsyncPtr: PAsync;
begin
InitAsync(AsyncPtr);
try
   EnviarBufferAsincronicamente(Buffer, Longitud, AsyncPtr);
   Result := EsperarFinDeOperacionAsincronica(AsyncPtr);
finally
   DoneAsync(AsyncPtr);
end;
end;

//------------------------------------------------------------------------------
// Realiza una operación de escritura asincrónica.
//------------------------------------------------------------------------------
function TPuertoCOM.EnviarCadenaAsincronicamente(Str: string; var AsyncPtr: PAsync): Integer;
begin
Str := Str + FinDeCadena;
if Length(Str) > 0 then
   Result := EnviarBufferAsincronicamente(Str[1], Length(Str), AsyncPtr)
else
   Result := 0;
end;

//------------------------------------------------------------------------------
// Realiza una operación de escritura sincrónica.
//------------------------------------------------------------------------------
function TPuertoCOM.EnviarCadena(const Str: string): Integer;
var AsyncPtr: PAsync;
begin
InitAsync(AsyncPtr);
try
   EnviarCadenaAsincronicamente(Str, AsyncPtr);
   Result := EsperarFinDeOperacionAsincronica(AsyncPtr);
finally
   DoneAsync(AsyncPtr);
end;
end;

//------------------------------------------------------------------------------
// Realiza una operación de lectura asincrónica.
//------------------------------------------------------------------------------
function TPuertoCOM.RecibirBufferAsincronicamente(var Buffer; Count: Integer; var AsyncPtr: PAsync): Integer;
var Success: Boolean;
    BytesTrans: DWORD;
begin
if AsyncPtr = nil then
   raise EComPort.CreateNoWinCode(CError_InvalidAsync);
AsyncPtr^.Kind := okRead;
Success := ReadFile(FHandle,
                    Buffer,
                    Count,
                    BytesTrans,
                    @AsyncPtr^.Overlapped) or
                    (GetLastError = ERROR_IO_PENDING);
if not Success then
   raise EComPort.Create(CError_ReadFailed, GetLastError);
Result := BytesTrans;
end;

//------------------------------------------------------------------------------
// Realiza una operación de lectura sincrónica.
//------------------------------------------------------------------------------
function TPuertoCOM.RecibirBuffer(var Buffer; Longitud: Integer): Integer;
var AsyncPtr: PAsync;
begin
InitAsync(AsyncPtr);
try
   RecibirBufferAsincronicamente(Buffer, Longitud, AsyncPtr);
   Result := EsperarFinDeOperacionAsincronica(AsyncPtr);
finally
   DoneAsync(AsyncPtr);
end;
end;

//------------------------------------------------------------------------------
// Realiza una operación de lectura asincrónica.
//------------------------------------------------------------------------------
function TPuertoCOM.RecibirCadenaAsincronicamente(var Str: string; Count: Integer; var AsyncPtr: PAsync): Integer;
begin
SetLength(Str, Count);
if Count > 0 then
   Result := RecibirBufferAsincronicamente(Str[1], Count, AsyncPtr)
else
   Result := 0;
end;

//------------------------------------------------------------------------------
// Realiza una operación de lectura sincrónica.
//------------------------------------------------------------------------------
function TPuertoCOM.RecibirCadena(var Str: string; Longitud: Integer): Integer;
var AsyncPtr: PAsync;
begin
InitAsync(AsyncPtr);
try
   RecibirCadenaAsincronicamente(Str, Longitud, AsyncPtr);
   Result := EsperarFinDeOperacionAsincronica(AsyncPtr);
   SetLength(Str, Result);
finally
   DoneAsync(AsyncPtr);
end;
end;

function ErrorCode(AsyncPtr: PAsync): Integer;
begin
Result := 0;
case AsyncPtr^.Kind of
     okWrite: Result := CError_WriteFailed;
     okRead:  Result := CError_ReadFailed;
     end;
end;

//------------------------------------------------------------------------------
// Espera a que termine una operación asincrónica.
//------------------------------------------------------------------------------
function TPuertoCOM.EsperarFinDeOperacionAsincronica(var AsyncPtr: PAsync): Integer;
var BytesTrans, Signaled: DWORD;
    Success: Boolean;
begin
if AsyncPtr = nil then
   raise EComPort.CreateNoWinCode(CError_InvalidAsync);
Signaled := WaitForSingleObject(AsyncPtr^.Overlapped.hEvent, INFINITE);
Success := (Signaled = WAIT_OBJECT_0) and
           (GetOverlappedResult(FHandle, AsyncPtr^.Overlapped, BytesTrans, False));
if not Success then
   raise EComPort.Create(ErrorCode(AsyncPtr), GetLastError);
Result := BytesTrans;
end;

//------------------------------------------------------------------------------
// Detiene todas las operaciones asincrónicas.
//------------------------------------------------------------------------------
procedure TPuertoCOM.AbortarTodasLasOperacionesAsincronicas;
begin
if not PurgeComm(FHandle, PURGE_TXABORT or PURGE_RXABORT) then
   raise EComPort.Create(CError_PurgeFailed, GetLastError);
end;

//------------------------------------------------------------------------------
// Detecta cuando una operación asincrónica se ha completado.
//------------------------------------------------------------------------------
function TPuertoCOM.OperacionAsincronicaCompletada(AsyncPtr: PAsync): Boolean;
var BytesTrans: DWORD;
begin
if AsyncPtr = nil then
   raise EComPort.CreateNoWinCode(CError_InvalidAsync);
Result := GetOverlappedResult(FHandle, AsyncPtr^.Overlapped, BytesTrans, False);
if not Result then
   if (GetLastError <> ERROR_IO_PENDING) and
      (GetLastError <> ERROR_IO_INCOMPLETE) then
      raise EComPort.Create(CError_AsyncCheck, GetLastError);
end;

//------------------------------------------------------------------------------
// Espera los eventos que ocurrirán en el puerto serie.
//------------------------------------------------------------------------------
procedure TPuertoCOM.EsperarPorUnEvento(var Events: TComEvents; StopEvent: THandle; Timeout: Integer);
var Overlapped: TOverlapped;
    Mask: DWORD;
    Success: Boolean;
    Signaled, EventHandleCount: Integer;
    EventHandles: array[0..1] of THandle;
begin
//No se puede llamar el método si el hilo está corriendo.
if FThreadCreated then
   raise EComPort.CreateNoWinCode(CError_ThreadCreated);
FillChar(Overlapped, SizeOf(TOverlapped), 0);
Overlapped.hEvent := CreateEvent(nil, True, False, nil);
EventHandles[0] := Overlapped.hEvent;
if StopEvent <> 0 then
   begin
   EventHandles[1] := StopEvent;
   EventHandleCount := 2;
   end
else
   EventHandleCount := 1;
try
   SetCommMask(FHandle, EventsToInt(Events));
   Success := WaitCommEvent(FHandle, Mask, @Overlapped);
   if (Success) or (GetLastError = ERROR_IO_PENDING) then
      begin
      Signaled := WaitForMultipleObjects(EventHandleCount, @EventHandles, False, Timeout);
      Success := (Signaled = WAIT_OBJECT_0) or
                 (Signaled = WAIT_OBJECT_0 + 1) or
                 (Signaled = WAIT_TIMEOUT);
      SetCommMask(FHandle, 0);
      end;
   if not Success then
      raise EComPort.Create(CError_WaitFailed, GetLastError);
   Events := IntToEvents(Mask);
finally
   CloseHandle(Overlapped.hEvent);
end;
end;

//------------------------------------------------------------------------------
// Transmite un caracter por encima de cualquier dato que se
// encuentre pendiente en el buffer de salida.
//------------------------------------------------------------------------------
procedure TPuertoCOM.EnviarCaracter(Ch: Char);
begin
if not TransmitCommChar(FHandle, Ch) then
   raise EComPort.Create(CError_TransmitFailed, GetLastError);
end;

//------------------------------------------------------------------------------
// Muestra el diálogo de configuración del puerto.
//------------------------------------------------------------------------------
procedure TPuertoCOM.MostrarDialogoDeConfiguracion;
begin
ComfigurarPuertoCOM(self);
end;

//------------------------------------------------------------------------------
// Convierte de Boolean a String.
//------------------------------------------------------------------------------
function BoolToStr(const Value: Boolean): string;
begin
if Value then Result := 'Yes' else Result := 'No';
end;

//------------------------------------------------------------------------------
// Convierte de String a Boolean.
//------------------------------------------------------------------------------
function StrToBool(const Value: string): Boolean;
begin
if UpperCase(Value) = 'YES' then Result := True else Result := False;
end;

//------------------------------------------------------------------------------
// Convierte de DTR a String.
//------------------------------------------------------------------------------
function DTRToStr(DTRFlowControl: TDTRFlowControl): string;
const DTRStrings: array[TDTRFlowControl] of string =
      ('Disable', 'Enable', 'Handshake');
begin
Result := DTRStrings[DTRFlowControl];
end;

//------------------------------------------------------------------------------
// Convierte de STR a String.
//------------------------------------------------------------------------------
function RTSToStr(RTSFlowControl: TRTSFlowControl): string;
const RTSStrings: array[TRTSFlowControl] of string =
      ('Disable', 'Enable', 'Handshake', 'Toggle');
begin
Result := RTSStrings[RTSFlowControl];
end;

//------------------------------------------------------------------------------
// Convierte de String a RTS.
//------------------------------------------------------------------------------
function StrToRTS(Str: string): TRTSFlowControl;
var I: TRTSFlowControl;
begin
I := Low(TRTSFlowControl);
while (I <= High(TRTSFlowControl)) do
      begin
      if UpperCase(Str) = UpperCase(RTSToStr(I)) then Break;
      I := Succ(I);
      end;
if I > High(TRTSFlowControl) then
   Result := rtsDesactivado
else
   Result := I;
end;

//------------------------------------------------------------------------------
// Convierte de String a DTR.
//------------------------------------------------------------------------------
function StrToDTR(Str: string): TDTRFlowControl;
var I: TDTRFlowControl;
begin
I := Low(TDTRFlowControl);
while (I <= High(TDTRFlowControl)) do
      begin
      if UpperCase(Str) = UpperCase(DTRToStr(I)) then Break;
      I := Succ(I);
      end;
if I > High(TDTRFlowControl) then
   Result := dtrDesactivado
else
   Result := I;
end;

//------------------------------------------------------------------------------
// Convierte de String a Char.
//------------------------------------------------------------------------------
function StrToChar(Str: string): Char;
var A: Integer;
begin
if Length(Str) > 0 then
   begin
   if (Str[1] = '#') and (Length(Str) > 1) then
       begin
       try
          A := StrToInt(Copy(Str, 2, Length(Str) - 1));
       except
          A := 0;
       end;
       Result := Chr(Byte(A));
       end
   else
       Result := Str[1];
   end
else
   Result := #0;
end;

//------------------------------------------------------------------------------
// Convierte de Char a String.
//------------------------------------------------------------------------------
function CharToStr(Ch: Char): string;
begin
if Ch in [#33..#127] then Result := Ch else Result := '#' + IntToStr(Ord(Ch));
end;

//------------------------------------------------------------------------------
// Guarda la configuración en un fichero INI.
//------------------------------------------------------------------------------
procedure TPuertoCOM.StoreIniFile(IniFile: TIniFile);
begin
if spBasic in FStoredProps then
   begin
   IniFile.WriteString(Nombre, 'Port', Puerto);
   IniFile.WriteString(Nombre, 'BaudRate', BaudRateToStr(Velocidad));
   if Velocidad = brDeCostumbre then
      IniFile.WriteInteger(Nombre, 'CustomBaudRate', VelocidadPersonalizada);
   IniFile.WriteString(Nombre, 'StopBits', StopBitsToStr(BitsDeParada));
   IniFile.WriteString(Nombre, 'DataBits', DataBitsToStr(CantidadDeBits));
   IniFile.WriteString(Nombre, 'Parity', ParityToStr(Paridad.Bits));
   IniFile.WriteString(Nombre, 'FlowControl', FlowControlToStr(ControlDeFlujo.FlowControl));
   end;
if spOthers in FStoredProps then
   begin
   IniFile.WriteString(Nombre, 'EventChar', CharToStr(EventChar));
   IniFile.WriteString(Nombre, 'DiscardNull', BoolToStr(DiscardNull));
   end;
if spParity in FStoredProps then
   begin
   IniFile.WriteString(Nombre, 'Parity.Check', BoolToStr(Paridad.Check));
   IniFile.WriteString(Nombre, 'Parity.Replace', BoolToStr(Paridad.Replace));
   IniFile.WriteString(Nombre, 'Parity.ReplaceChar', CharToStr(Paridad.ReplaceChar));
   end;
if spBuffer in FStoredProps then
   begin
   IniFile.WriteInteger(Nombre, 'Buffer.OutputSize', Buffer.OutputSize);
   IniFile.WriteInteger(Nombre, 'Buffer.InputSize', Buffer.InputSize);
   end;
if spTimeouts in FStoredProps then
   begin
   IniFile.WriteInteger(Nombre, 'Timeouts.ReadInterval', Timeouts.ReadInterval);
   IniFile.WriteInteger(Nombre, 'Timeouts.ReadTotalConstant', Timeouts.ReadTotalConstant);
   IniFile.WriteInteger(Nombre, 'Timeouts.ReadTotalMultiplier', Timeouts.ReadTotalMultiplier);
   IniFile.WriteInteger(Nombre, 'Timeouts.WriteTotalConstant', Timeouts.WriteTotalConstant);
   IniFile.WriteInteger(Nombre, 'Timeouts.WriteTotalMultiplier', Timeouts.WriteTotalMultiplier);
   end;
if spFlowControl in FStoredProps then
   begin
   IniFile.WriteString(Nombre, 'FlowControl.ControlRTS', RTSToStr(ControlDeFlujo.ControlRTS));
   IniFile.WriteString(Nombre, 'FlowControl.ControlDTR', DTRToStr(ControlDeFlujo.ControlDTR));
   IniFile.WriteString(Nombre, 'FlowControl.DSRSensitivity', BoolToStr(ControlDeFlujo.DSRSensitivity));
   IniFile.WriteString(Nombre, 'FlowControl.OutCTSFlow', BoolToStr(ControlDeFlujo.OutCTSFlow));
   IniFile.WriteString(Nombre, 'FlowControl.OutDSRFlow', BoolToStr(ControlDeFlujo.OutDSRFlow));
   IniFile.WriteString(Nombre, 'FlowControl.TxContinueOnXoff', BoolToStr(ControlDeFlujo.TxContinueOnXoff));
   IniFile.WriteString(Nombre, 'FlowControl.XonXoffIn', BoolToStr(ControlDeFlujo.XonXoffIn));
   IniFile.WriteString(Nombre, 'FlowControl.XonXoffOut', BoolToStr(ControlDeFlujo.XonXoffOut));
   IniFile.WriteString(Nombre, 'FlowControl.XoffChar', CharToStr(ControlDeFlujo.XoffChar));
   IniFile.WriteString(Nombre, 'FlowControl.XonChar', CharToStr(ControlDeFlujo.XonChar));
   end;
end;

//------------------------------------------------------------------------------
// Guarda la configuración en el registro.
//------------------------------------------------------------------------------
procedure TPuertoCOM.StoreRegistry(Reg: TRegistry);
begin
if spBasic in FStoredProps then
   begin
   Reg.WriteString('Port', Puerto);
   Reg.WriteString('BaudRate', BaudRateToStr(Velocidad));
   if Velocidad = brDeCostumbre then
      Reg.WriteInteger('CustomBaudRate', VelocidadPersonalizada);
   Reg.WriteString('StopBits', StopBitsToStr(BitsDeParada));
   Reg.WriteString('DataBits', DataBitsToStr(CantidadDeBits));
   Reg.WriteString('Parity', ParityToStr(Paridad.Bits));
   Reg.WriteString('FlowControl', FlowControlToStr(ControlDeFlujo.FlowControl));
   end;
if spOthers in FStoredProps then
   begin
   Reg.WriteString('EventChar', CharToStr(EventChar));
   Reg.WriteString('DiscardNull', BoolToStr(DiscardNull));
   end;
if spParity in FStoredProps then
   begin
   Reg.WriteString('Parity.Check', BoolToStr(Paridad.Check));
   Reg.WriteString('Parity.Replace', BoolToStr(Paridad.Replace));
   Reg.WriteString('Parity.ReplaceChar', CharToStr(Paridad.ReplaceChar));
   end;
if spBuffer in FStoredProps then
   begin
   Reg.WriteInteger('Buffer.OutputSize', Buffer.OutputSize);
   Reg.WriteInteger('Buffer.InputSize', Buffer.InputSize);
   end;
if spTimeouts in FStoredProps then
   begin
   Reg.WriteInteger('Timeouts.ReadInterval', Timeouts.ReadInterval);
   Reg.WriteInteger('Timeouts.ReadTotalConstant', Timeouts.ReadTotalConstant);
   Reg.WriteInteger('Timeouts.ReadTotalMultiplier', Timeouts.ReadTotalMultiplier);
   Reg.WriteInteger('Timeouts.WriteTotalConstant', Timeouts.WriteTotalConstant);
   Reg.WriteInteger('Timeouts.WriteTotalMultiplier', Timeouts.WriteTotalMultiplier);
   end;
if spFlowControl in FStoredProps then
   begin
   Reg.WriteString('FlowControl.ControlRTS', RTSToStr(ControlDeFlujo.ControlRTS));
   Reg.WriteString('FlowControl.ControlDTR', DTRToStr(ControlDeFlujo.ControlDTR));
   Reg.WriteString('FlowControl.DSRSensitivity', BoolToStr(ControlDeFlujo.DSRSensitivity));
   Reg.WriteString('FlowControl.OutCTSFlow', BoolToStr(ControlDeFlujo.OutCTSFlow));
   Reg.WriteString('FlowControl.OutDSRFlow', BoolToStr(ControlDeFlujo.OutDSRFlow));
   Reg.WriteString('FlowControl.TxContinueOnXoff', BoolToStr(ControlDeFlujo.TxContinueOnXoff));
   Reg.WriteString('FlowControl.XonXoffIn', BoolToStr(ControlDeFlujo.XonXoffIn));
   Reg.WriteString('FlowControl.XonXoffOut', BoolToStr(ControlDeFlujo.XonXoffOut));
   Reg.WriteString('FlowControl.XoffChar', CharToStr(ControlDeFlujo.XoffChar));
   Reg.WriteString('FlowControl.XonChar', CharToStr(ControlDeFlujo.XonChar));
   end;
end;

//------------------------------------------------------------------------------
// Carga la configuración desde un fichero INI.
//------------------------------------------------------------------------------
procedure TPuertoCOM.LoadIniFile(IniFile: TIniFile);
begin
if spBasic in FStoredProps then
   begin
   Puerto := IniFile.ReadString(Nombre, 'Port', Puerto);
   Velocidad := StrToBaudRate(IniFile.ReadString(Nombre, 'BaudRate', BaudRateToStr(Velocidad)));
   if Velocidad = brDeCostumbre then
      VelocidadPersonalizada := IniFile.ReadInteger(Nombre, 'CustomBaudRate', 9600);
   BitsDeParada := StrToStopBits(IniFile.ReadString(Nombre, 'StopBits', StopBitsToStr(BitsDeParada)));
   CantidadDeBits := StrToDataBits(IniFile.ReadString(Nombre, 'DataBits', DataBitsToStr(CantidadDeBits)));
   Paridad.Bits := StrToParity(IniFile.ReadString(Nombre, 'Parity', ParityToStr(Paridad.Bits)));
   ControlDeFlujo.FlowControl := StrToFlowControl(
   IniFile.ReadString(Nombre, 'FlowControl', FlowControlToStr(ControlDeFlujo.FlowControl)));
   end;
if spOthers in FStoredProps then
   begin
   EventChar := StrToChar(IniFile.ReadString(Nombre, 'EventChar', CharToStr(EventChar)));
   DiscardNull := StrToBool(IniFile.ReadString(Nombre, 'DiscardNull', BoolToStr(DiscardNull)));
   end;
if spParity in FStoredProps then
   begin
   Paridad.Check := StrToBool(IniFile.ReadString(Nombre, 'Parity.Check', BoolToStr(Paridad.Check)));
   Paridad.Replace := StrToBool(IniFile.ReadString(Nombre, 'Parity.Replace', BoolToStr(Paridad.Replace)));
   Paridad.ReplaceChar := StrToChar(IniFile.ReadString(Nombre, 'Parity.ReplaceChar', CharToStr(Paridad.ReplaceChar)));
   end;
if spBuffer in FStoredProps then
   begin
   Buffer.OutputSize := IniFile.ReadInteger(Nombre, 'Buffer.OutputSize', Buffer.OutputSize);
   Buffer.InputSize := IniFile.ReadInteger(Nombre, 'Buffer.InputSize', Buffer.InputSize);
   end;
if spTimeouts in FStoredProps then
   begin
   Timeouts.ReadInterval := IniFile.ReadInteger(Nombre, 'Timeouts.ReadInterval', Timeouts.ReadInterval);
   Timeouts.ReadTotalConstant := IniFile.ReadInteger(Nombre, 'Timeouts.ReadTotalConstant', Timeouts.ReadTotalConstant);
   Timeouts.ReadTotalMultiplier := IniFile.ReadInteger(Nombre, 'Timeouts.ReadTotalMultiplier', Timeouts.ReadTotalMultiplier);
   Timeouts.WriteTotalConstant := IniFile.ReadInteger(Nombre, 'Timeouts.WriteTotalConstant', Timeouts.WriteTotalConstant);
   Timeouts.WriteTotalMultiplier := IniFile.ReadInteger(Nombre, 'Timeouts.WriteTotalMultiplier', Timeouts.WriteTotalMultiplier);
   end;
if spFlowControl in FStoredProps then
   begin
   ControlDeFlujo.ControlRTS := StrToRTS(IniFile.ReadString(Nombre, 'FlowControl.ControlRTS', RTSToStr(ControlDeFlujo.ControlRTS)));
   ControlDeFlujo.ControlDTR := StrToDTR(IniFile.ReadString(Nombre, 'FlowControl.ControlDTR', DTRToStr(ControlDeFlujo.ControlDTR)));
   ControlDeFlujo.DSRSensitivity := StrToBool(IniFile.ReadString(Nombre, 'FlowControl.DSRSensitivity', BoolToStr(ControlDeFlujo.DSRSensitivity)));
   ControlDeFlujo.OutCTSFlow := StrToBool(IniFile.ReadString(Nombre, 'FlowControl.OutCTSFlow', BoolToStr(ControlDeFlujo.OutCTSFlow)));
   ControlDeFlujo.OutDSRFlow := StrToBool(IniFile.ReadString(Nombre, 'FlowControl.OutDSRFlow', BoolToStr(ControlDeFlujo.OutCTSFlow)));
   ControlDeFlujo.TxContinueOnXoff := StrToBool(IniFile.ReadString(Nombre, 'FlowControl.TxContinueOnXoff', BoolToStr(ControlDeFlujo.TxContinueOnXoff)));
   ControlDeFlujo.XonXoffIn := StrToBool(IniFile.ReadString(Nombre, 'FlowControl.XonXoffIn', BoolToStr(ControlDeFlujo.XonXoffIn)));
   ControlDeFlujo.XonXoffOut := StrToBool(IniFile.ReadString(Nombre, 'FlowControl.XonXoffOut', BoolToStr(ControlDeFlujo.XonXoffOut)));
   ControlDeFlujo.XoffChar := StrToChar(IniFile.ReadString(Nombre, 'FlowControl.XoffChar', CharToStr(ControlDeFlujo.XoffChar)));
   ControlDeFlujo.XonChar := StrToChar(IniFile.ReadString(Nombre, 'FlowControl.XonChar', CharToStr(ControlDeFlujo.XonChar)));
   end;
end;

//------------------------------------------------------------------------------
// Carga la configuración desde el registro de Windows.
//------------------------------------------------------------------------------
procedure TPuertoCOM.LoadRegistry(Reg: TRegistry);
begin
if spBasic in FStoredProps then
   begin
   Puerto := Reg.ReadString('Port');
   Velocidad := StrToBaudRate(Reg.ReadString('BaudRate'));
   if Velocidad = brDeCostumbre then
      VelocidadPersonalizada := Reg.ReadInteger('CustomBaudRate');
   BitsDeParada := StrToStopBits(Reg.ReadString('StopBits'));
   CantidadDeBits := StrToDataBits(Reg.ReadString('DataBits'));
   Paridad.Bits := StrToParity(Reg.ReadString('Parity'));
   ControlDeFlujo.FlowControl := StrToFlowControl(Reg.ReadString('FlowControl'));
   end;
if spOthers in FStoredProps then
   begin
   EventChar := StrToChar(Reg.ReadString('EventChar'));
   DiscardNull := StrToBool(Reg.ReadString('DiscardNull'));
   end;
if spParity in FStoredProps then
   begin
   Paridad.Check := StrToBool(Reg.ReadString('Parity.Check'));
   Paridad.Replace := StrToBool(Reg.ReadString('Parity.Replace'));
   Paridad.ReplaceChar := StrToChar(Reg.ReadString('Parity.ReplaceChar'));
   end;
if spBuffer in FStoredProps then
   begin
   Buffer.OutputSize := Reg.ReadInteger('Buffer.OutputSize');
   Buffer.InputSize := Reg.ReadInteger('Buffer.InputSize');
   end;
if spTimeouts in FStoredProps then
   begin
   Timeouts.ReadInterval := Reg.ReadInteger('Timeouts.ReadInterval');
   Timeouts.ReadTotalConstant := Reg.ReadInteger('Timeouts.ReadTotalConstant');
   Timeouts.ReadTotalMultiplier := Reg.ReadInteger('Timeouts.ReadTotalMultiplier');
   Timeouts.WriteTotalConstant := Reg.ReadInteger('Timeouts.WriteTotalConstant');
   Timeouts.WriteTotalMultiplier := Reg.ReadInteger('Timeouts.WriteTotalMultiplier');
   end;
if spFlowControl in FStoredProps then
   begin
   ControlDeFlujo.ControlRTS := StrToRTS(Reg.ReadString('FlowControl.ControlRTS'));
   ControlDeFlujo.ControlDTR := StrToDTR(Reg.ReadString('FlowControl.ControlDTR'));
   ControlDeFlujo.DSRSensitivity := StrToBool(Reg.ReadString('FlowControl.DSRSensitivity'));
   ControlDeFlujo.OutCTSFlow := StrToBool(Reg.ReadString('FlowControl.OutCTSFlow'));
   ControlDeFlujo.OutDSRFlow := StrToBool(Reg.ReadString('FlowControl.OutDSRFlow'));
   ControlDeFlujo.TxContinueOnXoff := StrToBool(Reg.ReadString('FlowControl.TxContinueOnXoff'));
   ControlDeFlujo.XonXoffIn := StrToBool(Reg.ReadString('FlowControl.XonXoffIn'));
   ControlDeFlujo.XonXoffOut := StrToBool(Reg.ReadString('FlowControl.XonXoffOut'));
   ControlDeFlujo.XoffChar := StrToChar(Reg.ReadString('FlowControl.XoffChar'));
   ControlDeFlujo.XonChar := StrToChar(Reg.ReadString('FlowControl.XonChar'));
   end;
end;

//------------------------------------------------------------------------------
// Inicializa el registro.
//------------------------------------------------------------------------------
procedure SetRegistry(Reg: TRegistry; Key: string; Name: string);
var I: Integer;
    Temp: string;
begin
I := Pos('\', Key);
if I > 0 then
   begin
   Temp := Copy(Key, 1, I - 1);
   if UpperCase(Temp) = 'HKEY_LOCAL_MACHINE' then
      Reg.RootKey := HKEY_LOCAL_MACHINE
   else
      if UpperCase(Temp) = 'HKEY_CURRENT_USER' then
         Reg.RootKey := HKEY_CURRENT_USER;
   Key := Copy(Key, I + 1, Length(Key) - I);
   if Key[Length(Key)] <> '\' then Key := Key + '\';
   Key := Key + Name;
   Reg.OpenKey(Key, True);
   end;
end;

//------------------------------------------------------------------------------
// Almacena la configuración.
//------------------------------------------------------------------------------
procedure TPuertoCOM.GuardarConfiguracion(TipoDeGuardado: TTipoDeGuardado; GuardarEn: string);
var IniFile: TIniFile;
    Reg: TRegistry;
begin
try
   if TipoDeGuardado = stEnRegistro then
      begin
      Reg := TRegistry.Create;
      try
         SetRegistry(Reg, GuardarEn, Nombre);
         StoreRegistry(Reg);
      finally
         Reg.Free;
      end
      end
   else
      begin
      IniFile := TIniFile.Create(GuardarEn);
      try
         StoreIniFile(IniFile);
      finally
         IniFile.Free;
      end
      end;
except
   raise EComPort.CreateNoWinCode(CError_StoreFailed);
end;
end;

//------------------------------------------------------------------------------
// Carga la configuración.
//------------------------------------------------------------------------------
procedure TPuertoCOM.LoadSettings(TipoDeGuardado: TTipoDeGuardado; GuardarEn: string);
var IniFile: TIniFile;
    Reg: TRegistry;
begin
IniciarActualizacion;
try
   try
      if TipoDeGuardado = stEnRegistro then
         begin
         Reg := TRegistry.Create;
         try
            SetRegistry(Reg, GuardarEn, Nombre);
            LoadRegistry(Reg);
         finally
            Reg.Free;
         end
         end
      else
         begin
         IniFile := TIniFile.Create(GuardarEn);
         try
            LoadIniFile(IniFile);
         finally
            IniFile.Free;
         end
         end;
   finally
      FinalizarActualizacion;
   end;
except
   raise EComPort.CreateNoWinCode(CError_LoadFailed);
end;
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.DoBeforeClose;
begin
if Assigned(FOnBeforeClose) then FOnBeforeClose(Self);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.DoBeforeOpen;
begin
if Assigned(FOnBeforeOpen) then FOnBeforeOpen(Self);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.DoAfterOpen;
begin
if Assigned(FOnAfterOpen) then FOnAfterOpen(Self);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.DoAfterClose;
begin
if Assigned(FOnAfterClose) then FOnAfterClose(Self);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.DoRxChar(Count: Integer);
begin
if Assigned(FOnRxChar) then FOnRxChar(Self, Count);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.DoRxBuf(const Buffer; Count: Integer);
begin
if Assigned(FOnRxBuf) then FOnRxBuf(Self, Buffer, Count);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.DoBreak;
begin
if Assigned(FOnBreak) then FOnBreak(Self);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.DoTxEmpty;
begin
if Assigned(FOnTxEmpty) then FOnTxEmpty(Self);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.DoRing;
begin
if Assigned(FOnRing) then FOnRing(Self);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.DoCTSChange(OnOff: Boolean);
begin
if Assigned(FOnCTSChange) then FOnCTSChange(Self, OnOff);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.DoDSRChange(OnOff: Boolean);
begin
if Assigned(FOnDSRChange) then FOnDSRChange(Self, OnOff);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.DoRLSDChange(OnOff: Boolean);
begin
if Assigned(FOnRLSDChange) then FOnRLSDChange(Self, OnOff);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.DoError(Errors: TComErrors);
begin
if Assigned(FOnError) then FOnError(Self, Errors);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.DoRxFlag;
begin
if Assigned(FOnRxFlag) then FOnRxFlag(Self);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.DoRx80Full;
begin
if Assigned(FOnRx80Full) then FOnRx80Full(Self);
end;

//------------------------------------------------------------------------------
// set signals to false on close, and to proper value on open,
// because OnXChange events are not called automatically
//------------------------------------------------------------------------------
procedure TPuertoCOM.CheckSignals(Open: Boolean);
begin
if Open then
   begin
   CallCTSChange;
   CallDSRChange;
   CallRLSDChange;
   end
else
   begin
   DoCTSChange(False);
   DoDSRChange(False);
   DoRLSDChange(False);
   end;
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.CallAfterClose;
begin
DoAfterClose;
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.CallAfterOpen;
begin
DoAfterOpen;
CheckSignals(True);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.CallBeforeClose;
begin
CheckSignals(False);
DoBeforeClose;
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.CallBeforeOpen;
begin
DoBeforeOpen;
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.CallBreak;
begin
DoBreak;
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.CallCTSChange;
var OnOff: Boolean;
begin
OnOff := csCTS in Indicadores;
DoCTSChange(OnOff);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.CallDSRChange;
var OnOff: Boolean;
begin
OnOff := csDSR in Indicadores;
DoDSRChange(OnOff);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.CallRLSDChange;
var OnOff: Boolean;
begin
OnOff := csRLSD in Indicadores;
DoRLSDChange(OnOff);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.CallError;
var Errors: TComErrors;
begin
Errors := LastErrors;
if Errors <> [] then DoError(Errors);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.CallRing;
begin
DoRing;
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.CallRx80Full;
begin
DoRx80Full;
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.CallRxChar;
var Count: Integer;

  //Lee desde el buffer de entrada.
  procedure PerformRead(var P: Pointer);
  begin
  GetMem(P, Count);
  RecibirBuffer(P^, Count);
  DoRxBuf(P^, Count);    
  end;

begin
Count := CaracteresRecibidos;
if Count > 0 then DoRxChar(Count);
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.CallRxFlag;
begin
DoRxFlag;
end;

//------------------------------------------------------------------------------
procedure TPuertoCOM.CallTxEmpty;
begin
DoTxEmpty;
end;

//------------------------------------------------------------------------------
// Establece la propiedad "connected".
//------------------------------------------------------------------------------
procedure TPuertoCOM.SetConnected(const Value: Boolean);
begin
if Value <> FConnected then if Value then Abrir else Cerrar;
end;

//------------------------------------------------------------------------------
// Establece la velocidad de comunicación (baud rate).
//------------------------------------------------------------------------------
procedure TPuertoCOM.SetBaudRate(const Value: TBaudRate);
begin
if Value <> FBaudRate then
   begin
   FBaudRate := Value;
   ApplyDCB;
   end;
end;

//------------------------------------------------------------------------------
// Establece una velocidad de comunicación arbitraria.
//------------------------------------------------------------------------------
procedure TPuertoCOM.SetCustomBaudRate(const Value: Integer);
begin
if Value <> FCustomBaudRate then
   begin
   FCustomBaudRate := Value;
   ApplyDCB;
   end;
end;

//------------------------------------------------------------------------------
// Establece los bits de datos (data bits).
//------------------------------------------------------------------------------
procedure TPuertoCOM.SetDataBits(const Value: TDataBits);
begin
if Value <> FDataBits then
   begin
   FDataBits := Value;
   ApplyDCB;
   end;
end;

//------------------------------------------------------------------------------
// Establece descartar el caracter nulo.
//------------------------------------------------------------------------------
procedure TPuertoCOM.SetDiscardNull(const Value: Boolean);
begin
if Value <> FDiscardNull then
   begin
   FDiscardNull := Value;
   ApplyDCB;
   end;
end;

//------------------------------------------------------------------------------
// Establece el caracter de evento.
//------------------------------------------------------------------------------
procedure TPuertoCOM.SetEventChar(const Value: Char);
begin
if Value <> FEventChar then
   begin
   FEventChar := Value;
   ApplyDCB;
   end;
end;

//------------------------------------------------------------------------------
// Establece el puerto.
//------------------------------------------------------------------------------
procedure TPuertoCOM.SetPort(const Value: TPort);
begin
if Value <> FPort then
   begin
   FPort := Value;
   if FConnected then
      begin
      Cerrar;
      Abrir;
      end;
   end;
end;

//------------------------------------------------------------------------------
// Establece los bits de parada (stop bits).
//------------------------------------------------------------------------------
procedure TPuertoCOM.SetStopBits(const Value: TStopBits);
begin
if Value <> FStopBits then
   begin
   FStopBits := Value;
   ApplyDCB;
   end;
end;

//------------------------------------------------------------------------------
// Establece la sincronización de métodos.
//------------------------------------------------------------------------------
procedure TPuertoCOM.SetSyncMethod(const Value: TSyncMethod);
begin
if Value <> FSyncMethod then
   begin
   if FConnected then
      raise EComPort.CreateNoWinCode(CError_ConnChangeProp)
   else
      FSyncMethod := Value;
   end;
end;

//------------------------------------------------------------------------------
// Establece el disparador RxChar.
//------------------------------------------------------------------------------
procedure TPuertoCOM.SetTriggersOnRxChar(const Value: Boolean);
begin
FTriggersOnRxChar := Value;
end;

//------------------------------------------------------------------------------
// Establece la prioridad del subproceso que monitorea los eventos.
//------------------------------------------------------------------------------
procedure TPuertoCOM.SetEventThreadPriority(const Value: TThreadPriority);
begin
if Value <> FEventThreadPriority then
   begin
   if FConnected then
      raise EComPort.CreateNoWinCode(CError_ConnChangeProp)
   else
      FEventThreadPriority := Value;
   end;
end;

//------------------------------------------------------------------------------
// Retorna True si RxChar es disparado cuando los datos llegan al buffer de entrada.
//------------------------------------------------------------------------------
function TPuertoCOM.GetTriggersOnRxChar: Boolean;
begin
Result := FTriggersOnRxChar;
end;

//------------------------------------------------------------------------------
// Establece el control de flujo.
//------------------------------------------------------------------------------
procedure TPuertoCOM.SetFlowControl(const Value: TComFlowControl);
begin
FFlowControl.Assign(Value);
ApplyDCB;
end;

//------------------------------------------------------------------------------
// Establece el parámetro Parity.
//------------------------------------------------------------------------------
procedure TPuertoCOM.SetParity(const Value: TComParity);
begin
FParity.Assign(Value);
ApplyDCB;
end;

//------------------------------------------------------------------------------
// Establece los Timeouts (Tiempos de salida)
//------------------------------------------------------------------------------
procedure TPuertoCOM.SetTimeouts(const Value: TComTimeouts);
begin
FTimeouts.Assign(Value);
ApplyTimeouts;
end;

//------------------------------------------------------------------------------
// Establece el buffer.
//------------------------------------------------------------------------------
procedure TPuertoCOM.SetBuffer(const Value: TComBuffer);
begin
FBuffer.Assign(Value);
ApplyBuffer;
end;

///////////////////////////////////////////////////////////////////////////////
////////////////      Métodos de la clase EComPort.      //////////////////////
///////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------------------------------
// Crea una Exception con un código de error de Windows.
//------------------------------------------------------------------------------
constructor EComPort.Create(ACode: Integer; AWinCode: Integer);
begin
FWinCode := AWinCode;
FCode := ACode;
inherited CreateFmt(ComErrorMessages[ACode] + (' (Código de error de windows: %d)'), [AWinCode]);
end;

//------------------------------------------------------------------------------
// Crea una Exception.
//------------------------------------------------------------------------------
constructor EComPort.CreateNoWinCode(ACode: Integer);
begin
FWinCode := -1;
FCode := ACode;
inherited Create(ComErrorMessages[ACode]);
end;

///////////////////////////////////////////////////////////////////////////////
////////////      Otras funciones y procedimientos.       /////////////////////
///////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------------------------------
// Inicialización de la variable PAsync usada en llamadas asincrónicas.
//------------------------------------------------------------------------------
procedure InitAsync(var AsyncPtr: PAsync);
begin
New(AsyncPtr);
with AsyncPtr^ do
     begin
     FillChar(Overlapped, SizeOf(TOverlapped), 0);
     Overlapped.hEvent := CreateEvent(nil, True, True, nil);
     Data := nil;
     Size := 0;
     end;
end;

//------------------------------------------------------------------------------
// Clean-up de la variable PAsync.
//------------------------------------------------------------------------------
procedure DoneAsync(var AsyncPtr: PAsync);
begin
with AsyncPtr^ do
     begin
     CloseHandle(Overlapped.hEvent);
     if Data <> nil then FreeMem(Data);
     end;
Dispose(AsyncPtr);
AsyncPtr := nil;
end;

//------------------------------------------------------------------------------
// Llena la lista con los puertos series existentes y devuelve la cantidad.
//------------------------------------------------------------------------------
function PuertosComExistentes(Ports: TStrings): Integer;
var KeyHandle: HKEY;
    ErrCode, Index: Integer;
    ValueName, Data: string;
    ValueLen, DataLen, ValueType: DWORD;
    TmpPorts: TStringList;
begin
Result := 0;
ErrCode := RegOpenKeyEx(HKEY_LOCAL_MACHINE,
                        'HARDWARE\DEVICEMAP\SERIALCOMM',
                        0,
                        KEY_READ,
                        KeyHandle);
if ErrCode <> ERROR_SUCCESS then
   raise EComPort.Create(CError_RegError, ErrCode);
TmpPorts := TStringList.Create;
try
   Index := 0;
   repeat
      ValueLen := 256;
      DataLen := 256;
      SetLength(ValueName, ValueLen);
      SetLength(Data, DataLen);
      ErrCode := RegEnumValue(KeyHandle,
                              Index,
                              PChar(ValueName),
                              {$IFDEF DELPHI_4_OR_HIGHER}
                                 Cardinal(ValueLen),
                              {$ELSE}
                                 ValueLen,
                              {$ENDIF}
                              nil,
                              @ValueType,
                              PByte(PChar(Data)),
                              @DataLen);
      if ErrCode = ERROR_SUCCESS then
         begin
         SetLength(Data, DataLen - 1);
         TmpPorts.Add(Data);
         Inc(Index);
         end
      else
         if ErrCode <> ERROR_NO_MORE_ITEMS then
            raise EComPort.Create(CError_RegError, ErrCode);
   until (ErrCode <> ERROR_SUCCESS);
   TmpPorts.Sort;
   Ports.Assign(TmpPorts);
   Result := Ports.Count;
finally
   RegCloseKey(KeyHandle);
   TmpPorts.Free;
end;
end;

//------------------------------------------------------------------------------
// Convierte de String a Baud Rate.
//------------------------------------------------------------------------------
function StrToBaudRate(Str: string): TBaudRate;
var I: TBaudRate;
begin
I := Low(TBaudRate);
while (I <= High(TBaudRate)) do
      begin
      if UpperCase(Str) = UpperCase(BaudRateToStr(TBaudRate(I))) then Break;
      I := Succ(I);
      end;
if I > High(TBaudRate) then Result := br9600 else Result := I;
end;

//------------------------------------------------------------------------------
// Convierte de String a Stop Bits.
//------------------------------------------------------------------------------
function StrToStopBits(Str: string): TStopBits;
var I: TStopBits;
begin
I := Low(TStopBits);
while (I <= High(TStopBits)) do
      begin
      if UpperCase(Str) = UpperCase(StopBitsToStr(TStopBits(I))) then Break;
      I := Succ(I);
      end;
if I > High(TStopBits) then Result := sbUno else Result := I;
end;

//------------------------------------------------------------------------------
// Convierte de String a Data Bits.
//------------------------------------------------------------------------------
function StrToDataBits(Str: string): TDataBits;
var I: TDataBits;
begin
I := Low(TDataBits);
while (I <= High(TDataBits)) do
      begin
      if UpperCase(Str) = UpperCase(DataBitsToStr(I)) then Break;
      I := Succ(I);
      end;
if I > High(TDataBits) then Result := dbOcho else Result := I;
end;

//------------------------------------------------------------------------------
// Convierte de String a Parity.
//------------------------------------------------------------------------------
function StrToParity(Str: string): TParityBits;
var I: TParityBits;
begin
I := Low(TParityBits);
while (I <= High(TParityBits)) do
      begin
      if UpperCase(Str) = UpperCase(ParityToStr(I)) then Break;
      I := Succ(I);
      end;
if I > High(TParityBits) then Result := prNinguno else Result := I;
end;

//------------------------------------------------------------------------------
// Convierte de String a Flow Control.
//------------------------------------------------------------------------------
function StrToFlowControl(Str: string): TFlowControl;
var I: TFlowControl;
begin
I := Low(TFlowControl);
while (I <= High(TFlowControl)) do
      begin
      if UpperCase(Str) = UpperCase(FlowControlToStr(I)) then Break;
      I := Succ(I);
      end;
if I > High(TFlowControl) then Result := fcDeCostumbre else Result := I;
end;

//------------------------------------------------------------------------------
// Convierte de Baud Rate a String.
//------------------------------------------------------------------------------
function BaudRateToStr(BaudRate: TBaudRate): string;
const BaudRateStrings: array[TBaudRate] of string =
      ('De costumbre', '110', '300', '600', '1200', '2400',
       '4800', '9600', '14400', '19200', '38400', '56000',
       '57600', '115200', '128000', '256000');
begin
Result := BaudRateStrings[BaudRate];
end;

//------------------------------------------------------------------------------
// Convierte de Stop Bits a String.
//------------------------------------------------------------------------------
function StopBitsToStr(StopBits: TStopBits): string;
const StopBitsStrings: array[TStopBits] of string = ('1', '1.5', '2');
begin
Result := StopBitsStrings[StopBits];
end;

//------------------------------------------------------------------------------
// Convierte de Data Bits a String.
//------------------------------------------------------------------------------
function DataBitsToStr(DataBits: TDataBits): string;
const DataBitsStrings: array[TDataBits] of string = ('5', '6', '7', '8');
begin
Result := DataBitsStrings[DataBits];
end;

//------------------------------------------------------------------------------
// Convierte de Parity a  String.
//------------------------------------------------------------------------------
function ParityToStr(Parity: TParityBits): string;
const ParityBitsStrings: array[TParityBits] of string =
      ('Ninguno', 'Impar', 'Par', 'Marca', 'Espacio');
begin
Result := ParityBitsStrings[Parity];
end;

//------------------------------------------------------------------------------
// Convierte de Flow Control a String.
//------------------------------------------------------------------------------
function FlowControlToStr(FlowControl: TFlowControl): string;
const FlowControlStrings: array[TFlowControl] of string =
      ('Hardware', 'Software', 'Ninguno', 'De costumbre');
begin
Result := FlowControlStrings[FlowControl];
end;


end.
