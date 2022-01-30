///////////////////////////////////////////////////////////////////////////////
// Autor: Santiago A. Orellana Pérez.
// Creado: 30/05/2013
// Escrito para: Delphi 6 y 7
// Utilización: Implementa una interface para controlar un receptor
//              por medio de operaciones básicas.
///////////////////////////////////////////////////////////////////////////////

unit ReceptorBasico;

interface

//-----------------------------------------------------------------------------
// Esta es la interface de los receptores.
// Todas las clases que hereden esta interface deberán implementar
// los métodos que se mencionan en la interface.
//-----------------------------------------------------------------------------
type
   IReceptor = interface
      procedure ConfigurarPuerto;                                 //Permite configurar el puerto de comunicación con el receptor.
      function Activar: Boolean;                                  //Activa el receptor.
      function Inicializar: Boolean;                              //Establece una configuración por defecto.
      function FrecuenciaMinima: Double;                          //Devuelve la mínima frecuencia de trabajo.
      function FrecuenciaMaxima: Double;                          //Devuelve la frecuencia máxima de trabajo.
      function FrecuenciaValida(FrecuenciaMhz: Double): Boolean;  //Devuelve TRUE si es posible establecer la frecuencia.
      function TiempoMinimoDeCambioDeFrecuencia: Integer;         //Es el tiempo de espera en milisegundos, para cambiar d euna frecuencia a otra.
      function RecibirLaFrecuencia(Mhz: Double): Boolean;         //Establece una frecuencia de trabajo.
      procedure ConfiguracionAvanzada;                            //Abre un diálogo que permite al usuario configurar el receptor.
      procedure Apagar;                                           //Apaga el receptor.
      procedure Eliminar;                                         //Llama al destructor..
   end;



implementation

end.
