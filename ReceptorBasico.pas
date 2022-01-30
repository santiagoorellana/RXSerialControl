///////////////////////////////////////////////////////////////////////////////
// Autor: Santiago A. Orellana P�rez.
// Creado: 30/05/2013
// Escrito para: Delphi 6 y 7
// Utilizaci�n: Implementa una interface para controlar un receptor
//              por medio de operaciones b�sicas.
///////////////////////////////////////////////////////////////////////////////

unit ReceptorBasico;

interface

//-----------------------------------------------------------------------------
// Esta es la interface de los receptores.
// Todas las clases que hereden esta interface deber�n implementar
// los m�todos que se mencionan en la interface.
//-----------------------------------------------------------------------------
type
   IReceptor = interface
      procedure ConfigurarPuerto;                                 //Permite configurar el puerto de comunicaci�n con el receptor.
      function Activar: Boolean;                                  //Activa el receptor.
      function Inicializar: Boolean;                              //Establece una configuraci�n por defecto.
      function FrecuenciaMinima: Double;                          //Devuelve la m�nima frecuencia de trabajo.
      function FrecuenciaMaxima: Double;                          //Devuelve la frecuencia m�xima de trabajo.
      function FrecuenciaValida(FrecuenciaMhz: Double): Boolean;  //Devuelve TRUE si es posible establecer la frecuencia.
      function TiempoMinimoDeCambioDeFrecuencia: Integer;         //Es el tiempo de espera en milisegundos, para cambiar d euna frecuencia a otra.
      function RecibirLaFrecuencia(Mhz: Double): Boolean;         //Establece una frecuencia de trabajo.
      procedure ConfiguracionAvanzada;                            //Abre un di�logo que permite al usuario configurar el receptor.
      procedure Apagar;                                           //Apaga el receptor.
      procedure Eliminar;                                         //Llama al destructor..
   end;



implementation

end.
