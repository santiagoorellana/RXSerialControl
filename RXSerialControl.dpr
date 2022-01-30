program RXSerialControl;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  PuertoComControl in 'PuertoComControl.pas',
  PuertoComConfiguracion in 'PuertoComConfiguracion.pas' {FormConfiguracion},
  ReceptorBasico in 'ReceptorBasico.pas',
  ReceptorARONE in 'ReceptorARONE.pas',
  ReceptorARONEConfiguracion in 'ReceptorARONEConfiguracion.pas' {FormReceptorARONEConfiguracion},
  ReceptorAR5000A in 'ReceptorAR5000A.pas',
  ReceptorAR5000AConfiguracion in 'ReceptorAR5000AConfiguracion.pas' {FormReceptorAR5000AConfiguracion},
  ReceptorICR75 in 'ReceptorICR75.pas',
  ReceptorICR75Configuracion in 'ReceptorICR75Configuracion.pas' {FormReceptorICR75Configuracion},
  ReceptorICR8500Configuracion in 'ReceptorICR8500Configuracion.pas' {FormReceptorICR8500Configuracion},
  ReceptorICR8500 in 'ReceptorICR8500.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
