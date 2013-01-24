unit uFormGuiasAbertas;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TDI, StdCtrls, ExtCtrls, Buttons;

type
  TFormGuiasAbertas = class(TForm)
    ScrollBox1: TScrollBox;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    Visualizador: IVisualizador;
  public

  end;

var
  FormGuiasAbertas: TFormGuiasAbertas;

implementation

uses DateUtils, Types, VisualizaImagensDasGuiasAbertas, uFormPrincipal;

{$R *.dfm}

procedure TFormGuiasAbertas.FormActivate(Sender: TObject);
begin
    Visualizador := TVisualizaImagensDasGuiasAbertas.Create(ScrollBox1, FTDI);
  FTDI.VisualizarAbas(Visualizador, TFormGuiasAbertas);
end;

procedure TFormGuiasAbertas.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Visualizador := nil;
end;

procedure TFormGuiasAbertas.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  Visualizador := TVisualizaImagensDasGuiasAbertas.Create(ScrollBox1, FTDI);
  FTDI.VisualizarAbas(Visualizador, TFormGuiasAbertas);
end;

end.
