unit VisualizaImagensDasGuiasAbertas;

{ *********************************************************************** }
{ Classe TVisualizaImagensDasGuiasAbertas                                 }
{   Implementa os métodos da interface IVisualizador para exibir uma      }
{    lista de cada formulário aberto.                                     }
{   Esta classe exibe uma LISTA DE IMAGENS de cada formulário aberto e    }
{    permitir que o usuário va para a guia de um formulário especifico    }
{    ao clicar em uma das imagens da lista.                               }
{                                                                         }
{                                                                         }
{   AUTOR: Rafael Stavarengo - faelsta@gmail.com - 08/2009                }
{                                                                         }
{   http://www.devmedia.com.br/articles/viewcomp.asp?comp=11692           }
{                                                                         }
{ Principais métodos                                                      }
{   Veja os comentários da interface IVisualizador.                       }
{                                                                         }
{ *********************************************************************** }

interface

uses TDI, Controls, ExtCtrls, StdCtrls, Types, Forms;

type
  TVisualizaImagensDasGuiasAbertas = class(TInterfacedObject, IVisualizador)
  private
    LargauraDosPaineis: Integer;
    AlturaDosPaineis: Integer;
    OndeOsPaineisSeraoExibidos: TWinControl;
    TDI: TTDI;

    {Retorna um TImagem devidamente ajustado dentro do seu parente}
    function GetImage(ParenteDoImage: TWinControl): TImage;
    {Retorna um TLabel devidamente ajustado dentro do seu parente}
    function GetLabel(ParenteDoLabel: TWinControl): TLabel;
    {Retorna uma panel devidamente ajustado dentro do objeto onde os
     paineis serão exibidos - OndeOsPaineisSeraoExibidos: TWinControl.
     Este atributo é definido no construtor da classe}
    function GetPanel: TPanel;
    {Percorre o objeto onde os paineis estão sendo exibidos e calcula a
     posição onde o proximo painel deve ser colocado}
    function GetPosicaoDoProximoPanel: TRect;

    procedure OnCliqueDosPaineis(Sender: TObject);
  public
    {Parametros do construtor
      OndeOsPaineisSeraoExibidos: é um TWinControl onde serão exibidas as
        imagens de cada formulário aberto. Recomendo que você use um TScroolBox
        pois tera um melhor efeito visual.
      TDI: objeto TTDI onde os formulário estão sendo exibidos. Este parametro
        não é obrigatório e a referencia deste parametro é usado apenas no
        evento OnClick da lista de imagens. Se você passar nil para este
        parametro o evento OnClick da lista de imagens não será criado}
    constructor Create(OndeOsPaineisSeraoExibidos: TWinControl; TDI: TTDI); reintroduce;
    {Veja descriçãod este método na classe IVisualizador}
    procedure ListarFormulario(FormularioAberto: TForm);
  end;

implementation

uses Classes, Graphics, SysUtils;

const
  ESPACO_ENTRE_OS_PAINEIS = 16;

{ TVisualizaImagensDasGuiasAbertas }

procedure TVisualizaImagensDasGuiasAbertas.ListarFormulario(FormularioAberto: TForm);
var
  Painel: TPanel;
  FormImage: TBitmap;
begin
  {armazena uma referencia à um novo painel}
  Painel := GetPanel;
  
  {Armazena um ponteiro para o ClassType do formulário que será listado.
   Este ponteiro será usado pelo evento OnCliqueDosPaineis}
  Painel.Tag := Integer(Pointer(FormularioAberto.ClassType));

  {Pede um novo TLabel que será exibido dentro do Painel criado acima}
  GetLabel(Painel).Caption := FormularioAberto.Caption;

  FormImage := FormularioAberto.GetFormImage;//pega a imagem do formulário
  try
    {Copia a imagem do formulário para o novo TImagem criado dentro do painel}
    GetImage(Painel).Picture.Assign(FormImage);
  finally
    FreeAndNil(FormImage);
  end;
end;

function TVisualizaImagensDasGuiasAbertas.GetImage(ParenteDoImage: TWinControl): TImage;
begin
  Result := TImage.Create(ParenteDoImage);
  Result.Parent := ParenteDoImage;
  Result.Align := alClient;
  Result.Visible := True;
  Result.Center := True;
  Result.Stretch := True;

  if Assigned(TDI) then
  begin
    Result.OnClick := OnCliqueDosPaineis;
    Result.Cursor := crHandPoint;
  end;
end;

function TVisualizaImagensDasGuiasAbertas.GetLabel(ParenteDoLabel: TWinControl): TLabel;
begin
  Result := TLabel.Create(ParenteDoLabel);
  Result.Parent := ParenteDoLabel;
  Result.Align := alBottom;
  Result.Layout := tlCenter;
  Result.Alignment := taCenter;
  Result.Font.Style := [fsBold];
  Result.WordWrap := True;
  Result.Visible := True;

  if Assigned(TDI) then
  begin
    Result.OnClick := OnCliqueDosPaineis;
    Result.Cursor := crHandPoint;
  end;
end;

function TVisualizaImagensDasGuiasAbertas.GetPosicaoDoProximoPanel: TRect;
var
  i: Integer;
  P: TPanel;
begin
  Result.Left := ESPACO_ENTRE_OS_PAINEIS;
  Result.Top := ESPACO_ENTRE_OS_PAINEIS;

  for i := 0 to OndeOsPaineisSeraoExibidos.ComponentCount - 1 do
  begin
    if OndeOsPaineisSeraoExibidos.Components[i] is TPanel then
    begin
      P := (OndeOsPaineisSeraoExibidos.Components[i] as TPanel);

      if P.Left >= Result.Left then
        Result.Left := P.Left + LargauraDosPaineis + ESPACO_ENTRE_OS_PAINEIS;

      if P.Top >= Result.Top then
        Result.Top := P.Top;

      if Result.Left + LargauraDosPaineis >= OndeOsPaineisSeraoExibidos.Width then
      begin
        Result.Top := Result.Top + AlturaDosPaineis + ESPACO_ENTRE_OS_PAINEIS;
        Result.Left := ESPACO_ENTRE_OS_PAINEIS;
      end;
    end;
  end;
end;

function TVisualizaImagensDasGuiasAbertas.GetPanel: TPanel;
var
  PosicaoDoPainel: TRect;
begin
  PosicaoDoPainel := GetPosicaoDoProximoPanel;
  Result := TPanel.Create(OndeOsPaineisSeraoExibidos);
  Result.Parent := OndeOsPaineisSeraoExibidos;
  Result.BevelInner := bvLowered;
  Result.BevelOuter := bvRaised;
  Result.Width := LargauraDosPaineis;
  Result.Height := AlturaDosPaineis;
  Result.Top := PosicaoDoPainel.Top;
  Result.Left := PosicaoDoPainel.Left;
  Result.Visible := True;

  if Assigned(TDI) then
  begin
    Result.OnClick := OnCliqueDosPaineis;
    Result.Cursor := crHandPoint;
  end;
end;

constructor TVisualizaImagensDasGuiasAbertas.Create(OndeOsPaineisSeraoExibidos: TWinControl; TDI: TTDI);
begin
  inherited Create;

  {Ajusta para que a largura e altura de cada imagem dos formulários tenha
   20% do tamanho da resolução do usuário}
  AlturaDosPaineis := Trunc(Screen.Height * 0.2);
  LargauraDosPaineis := Trunc(Screen.Width * 0.2);

  Self.OndeOsPaineisSeraoExibidos := OndeOsPaineisSeraoExibidos;
  Self.TDI := TDI;
end;

procedure TVisualizaImagensDasGuiasAbertas.OnCliqueDosPaineis(Sender: TObject);
var
  Auxiliar: TPanel;
  ClasseDoForm: TFormClass;
begin
  if not Assigned(TDI) then
    Exit;

  Auxiliar := nil;

  try
    if Sender is TPanel then
      Auxiliar := (Sender as TPanel)
    else if (Sender is TLabel) or (Sender is TImage) then
      if TWinControl(Sender).Parent is TPanel then
        Auxiliar := (TWinControl(Sender).Parent as TPanel);

    {Recupera o ClassType cujo o valor do ponteiro foi armazenado no Tag
     pelo método ListarFormulario}
    ClasseDoForm := TFormClass(Pointer(Auxiliar.Tag));

    {Se havia uma informação válida no Tag}
    if Assigned(ClasseDoForm) then
      TDI.MostrarFormulario(ClasseDoForm, False);
  except
    {Como existe muito cast de objetos no bloco acima alem de manipulação de
     ponteiros que podem ser invalidos quando um formulário for fechado,
     coloquei este try..except porque essas exceções não influenciam 
     funcionamento da classe.
     Tome cuidado! Se o usuário estiver clicando nas imagens e nada acontece,
     pode estar acontecendo uma exceção neste ponto}
  end;
end;

end.
