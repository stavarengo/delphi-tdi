unit uFormPrincipal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, TDI, ImgList;

type
  TFormPrincipal = class(TForm)
    Panel1: TPanel;
    BitBtn1: TSpeedButton;
    BitBtn2: TSpeedButton;
    SpeedButton1: TSpeedButton;
    CBMultinstancia: TCheckBox;
    CBPermitirFecharTodas: TCheckBox;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure CBPermitirFecharTodasClick(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormPrincipal: TFormPrincipal;
  FTDI: TTDI;

implementation

uses uFormCadastroDeClientes, uFormPadrao, uFormParametrosDoSistema,
  uFormGuiasAbertas, uFormConfirmaFechar;

{$R *.dfm}

procedure TFormPrincipal.FormCreate(Sender: TObject);
begin
  FTDI := TTDI.Create(Self, TFormPadrao);

  FTDI.MostrarMenuPopup := True;
end;

procedure TFormPrincipal.BitBtn1Click(Sender: TObject);
begin
 FTDI.MostrarFormulario(TFormCadastroDeClientes, CBMultinstancia.Checked);
end;

procedure TFormPrincipal.BitBtn2Click(Sender: TObject);
begin
  FTDI.MostrarFormulario(TFormParametrosDoSistema, CBMultinstancia.Checked);
end;

procedure TFormPrincipal.SpeedButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TFormPrincipal.CBPermitirFecharTodasClick(Sender: TObject);
begin
  if CBPermitirFecharTodas.Checked then
    FTDI.FormPadrao := nil
  else
    FTDI.FormPadrao := TFormPadrao;
end;

procedure TFormPrincipal.SpeedButton2Click(Sender: TObject);
begin
  FTDI.MostrarFormulario(TFormGuiasAbertas, False);
end;

procedure TFormPrincipal.SpeedButton3Click(Sender: TObject);
begin
  FTDI.MostrarFormulario(TFormConfirmaFechar, CBMultinstancia.Checked);
end;

end.
