program ExemploTDI;

uses
  XPMan,
  Forms,
  uFormPrincipal in 'uFormPrincipal.pas' {FormPrincipal},
  uFormCadastroDeClientes in 'uFormCadastroDeClientes.pas' {FormCadastroDeClientes},
  uFormParametrosDoSistema in 'uFormParametrosDoSistema.pas' {FormParametrosDoSistema},
  uFormPadrao in 'uFormPadrao.pas' {FormPadrao},
  TDI in '..\TDI\TDI.pas',
  VisualizaImagensDasGuiasAbertas in '..\TDI\VisualizaImagensDasGuiasAbertas.pas',
  uFormGuiasAbertas in 'uFormGuiasAbertas.pas' {FormGuiasAbertas},
  PageControlEx in '..\TDI\PageControlEx.pas',
  TabCloseButton in '..\TDI\TabCloseButton.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPrincipal, FormPrincipal);
  Application.Run;
end.
