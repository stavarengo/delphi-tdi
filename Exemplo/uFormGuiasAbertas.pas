unit uFormGuiasAbertas;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TDI, StdCtrls, ExtCtrls, Buttons;

type
  TFormGuiasAbertas = class(TForm)
    ScrollBox1: TScrollBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    Visualizador: IVisualizador;
  public

  end;

var
  FormGuiasAbertas: TFormGuiasAbertas;

implementation

uses DateUtils, Types, VisualizaImagensDasGuiasAbertas, uFormPrincipal;

{$R *.dfm}

procedure TFormGuiasAbertas.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Visualizador := nil;
end;

procedure TFormGuiasAbertas.FormShow(Sender: TObject);
begin
  Visualizador := TVisualizaImagensDasGuiasAbertas.Create(ScrollBox1, FTDI);
  FTDI.VisualizarAbas(Visualizador, TFormGuiasAbertas);
end;

end.
