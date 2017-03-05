unit TDI;
{ *********************************************************************** }
{ Classe TTDI                                                             }
{   Encapsula todo o código necessário para utilizar a interface TDI      }
{                                                                         }
{   AUTOR: Rafael Stavarengo - faelsta@gmail.com - 09/2012                }
{   http://stavarengo.com                                                 }
{                                                                         }
{   http://www.devmedia.com.br/artigo-clube-delphi-110-formularios-em-abas/14379           }
{   http://code.google.com/p/delphi-tdi/                                  }
{                                                                         }
{ Principais métodos                                                      }
{   MostrarFormulario(Classe: TComponentClass; Multi: Boolean);           }
{   Fechar(Todas: Boolean);                                               }
{   Formulario(Pagina: Integer): TForm;									  }
{ Veja mais informações sobre estes métodos em seu escopo.                }
{                                                                         }
{ Propriedades                                                            }
{   FormPadrao: TFormClass;                                               }
{    Este é o formulário que será aberto sempre que todas as abas         }
{    forem fechadas.                                                      }
{                                                                         }
{   MostrarMenuPopup: Boolean;                                            }
{    Nesta propriedade você pode definir se o menu popup com as opções    }
{    'Fechar' e 'Fechar todas' será exibido.                              }
{                                                                         }
{   PageControl: TPageControl;                                            }
{    Esta é uma propriedade somente leitura que dá acesso ao TPageControl }
{    onde as abas (TTabSheets) estão sendo exibidas.                      }
{                                                                         }
{                                                                         }
{ *********************************************************************** }

interface

uses ComCtrls, Forms, Messages, Controls, Menus, Classes, TabCloseButton;

const
  WM_CLOSE_TAB = WM_USER + 1;

type
  IVisualizador = interface;//declaracao antecipada

  //Sobrescreve o TPageControl nativo para com nossa classe que sabe desenhar
  //o botão fechar nas abas.
  TPageControl = class(TTabCloseButton);

  TTDI = class(TWinControl)
  private
    FPageControl: TPageControl;
    FFormPadrao: TFormClass;
    FPopup: TPopupMenu;

    procedure SetMostrarMenuPopup(const Value: Boolean);
    procedure SetFormPadrao(const Value: TFormClass);
    function GetFormPadrao: TFormClass;
    function GetMostrarMenuPopup: Boolean;
    function GetPageControl: TPageControl;

    procedure OnTabHide(Sender: TObject);
    procedure MenuFechar(Sender: TObject);
    procedure MenuFecharTodas(Sender: TObject);
    procedure CriarFormulario(Classe: TFormClass);
    procedure CriarPageControl;
    procedure WM_CLOSETAB(var Msg: TMessage); message WM_CLOSE_TAB;
    function NovaAba: TTabSheet;
    function Pagina(aClasseForm: TFormClass): TTabSheet;
    procedure PageControlDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PageControlDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure PageControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PageControlCloseClick(Sender: TObject);
    procedure CloseTab(Pagina: Integer);
  public
    constructor Create(AOwner: TWinControl; aFormPadrao: TFormClass); reintroduce;
    destructor Destroy; override;

    procedure MostrarFormulario(Classe: TFormClass; Multi: Boolean);

    procedure Fechar(Todas: Boolean);

    procedure VisualizarAbas(Visualizador: IVisualizador; NaoExibir: TFormClass);
	{Você informa o index da aba e o método retorna o formulário que está sendo 
	 exibido nela}
	function Formulario(Pagina: Integer): TForm;
  published
    property MostrarMenuPopup: Boolean read GetMostrarMenuPopup write SetMostrarMenuPopup;
    property PageControl: TPageControl read GetPageControl;

    {FormPadrao é classe do formulario que sempre sera aberto quando todos os
     outros forms estão fechados}
    property FormPadrao: TFormClass read GetFormPadrao write SetFormPadrao;
  end;

  {Interface para classes que desejam exibir uma lista dos formulários abertos}
  IVisualizador = interface
    ['{07EF861E-5B9F-4534-8D6B-3A62BB8C4F80}']
    {ListarFormulario é chamado uma vez para cada formulário aberto nas abas.
     O parâmetro FormularioAberto, possui uma referência para o formulário
     que está aberto}
    procedure ListarFormulario(FormularioAberto: TForm);
  end;

implementation

uses SysUtils, Windows;

const
  INDEX_FORM = 00;//o primeiro componente da TTabSheet eh sempre o formulario

{ TTDI }

procedure TTDI.MostrarFormulario(Classe: TFormClass; Multi: Boolean);
var
  Anterior: TTabSheet;
begin
  if not Multi then//se nao pode criar mais de uma instacia da classe
  begin
    Anterior := PageControl.ActivePage;
    PageControl.ActivePage := Pagina(Classe);

    if PageControl.ActivePage <> nil then//se encontrou uma instacia da classe
    begin
      if Anterior <> PageControl.ActivePage then PageControl.Change();
      Exit;//sai pq nao pode criar outra
    end;
  end;

  CriarFormulario(Classe);
end;

constructor TTDI.Create(AOwner: TWinControl; aFormPadrao: TFormClass);
begin
  inherited Create(AOwner);

  Parent       := AOwner;
  ParentWindow := AOwner.Handle;

  CriarPageControl;

  FFormPadrao := aFormPadrao;
  if Assigned(FFormPadrao) then
  begin
    MostrarFormulario(FFormPadrao, False);
  end;
end;

function TTDI.NovaAba: TTabSheet;
{adiciona uma nova aba ao PageControl e retorna a nova aba como resultado}

    {Alem de criar um novo TabSheet a funcao NovaAba ativa a TabSheet criada,
     assim apos executar NovaAba, a propriedade ActivePage sempre será a
     ultima TabSheet criada}
var
  Tab: TTabSheet;
begin
  Tab := TTabSheet.Create(PageControl);

  Tab.PageControl := PageControl;
  Tab.TabVisible  := True;
  Tab.Caption     := 'Carregando...';
  Tab.OnHide      := OnTabHide;
  Tab.PopupMenu   := nil;

  PageControl.Visible := True;  //exibe o PageControl (é criado oculto, caso não tenha form padrão)
  PageControl.ActivePageIndex := Tab.PageIndex;
  PageControl.Change();

  PageControl.UpdateCloseButtons;

  Result := Tab;
end;

function TTDI.Pagina(aClasseForm: TFormClass): TTabSheet;
{procura por um formulario passado no parametro Classe e retorna
 o TTabSheet onde este formulario se encontra.}
var
  i: Integer;
begin
{ TODO : FAZER VOLTAR PARA PRIMEIRA INSTACIA QUANDO EXISTE MAIS DE UMA E A ULTIMA JA ESTA ATIVA }

  Result := nil;//se nao econtrar retorna nil

  i := 0;//inicia variavel

  {verifica se a pagina ativa tem uma instacia da classe em questao,
   se tiver, define i igual a posicao da pagina ativa para comecar
   a procurar dali para frente. Se nao fizermos isso as abas ficaram
   alternando entre a primeira e a segunda instancia sem nunca passar
   para a terceira instacia, caso ela exista.}
  if PageControl.ActivePage <> nil then//se tiver uma pagina ativa
    if Formulario(PageControl.ActivePageIndex) is aClasseForm then
      i := PageControl.ActivePageIndex;

  //loop por todas as paginas
  for i := i to PageControl.PageCount - 1 do
    if Formulario(i) is aClasseForm then
    begin
      Result := PageControl.Pages[i];

      {se a pagina encontrada ja estiver ativa}
      if not (PageControl.ActivePage = Result) then
      begin
        {sai do loop apenas se a pagina nao estiver ativa, pq se a pagina
         ja estiver ativa nos vamos procurar por outra instancia desta classe.
         Isso fara com que caso exista mais de uma instacia criada, cada chamada
         desta funcao o resultado não seja a ultima instacia encontrada}
        Break;
      end;                                                      
    end;
end;

procedure TTDI.SetFormPadrao(const Value: TFormClass);
begin
  if FFormPadrao <> Value then
  begin
    FFormPadrao := Value;

    if Assigned(FFormPadrao) and not Assigned(PageControl.ActivePage) then
      MostrarFormulario(Value, False);
  end;
end;

function TTDI.GetFormPadrao: TFormClass;
begin
  Result := FFormPadrao;
end;

procedure TTDI.SetMostrarMenuPopup(const Value: Boolean);

  procedure AddMenuItem(aCaption: string; Atalho: TShortCut; aClick: TNotifyEvent);
  begin
    FPopup.Items.Add(TMenuItem.Create(FPopup));

    with FPopup.Items[FPopup.Items.Count - 1] do
    begin
      ParentWindow := PageControl.Handle;
      OnClick := aClick;
      ShortCut := Atalho;
      Caption := aCaption;
    end;
  end;

begin
  if Value then
  begin
    if not Assigned(FPopup) then
    begin
      FPopup := TPopupMenu.Create(PageControl);
      FPopup.Name := 'pop';
                                 //Atalho 16499 = CTRL + F4
      AddMenuItem('Fechar janela', 16499, MenuFechar);
      AddMenuItem('Fechar todas as janelas', 0, MenuFecharTodas);
    end;

    PageControl.PopupMenu := FPopup;
  end
  else
    PageControl.PopupMenu := nil
end;

function TTDI.GetMostrarMenuPopup: Boolean;
begin
  Result := Assigned(PageControl.PopupMenu);
end;

procedure TTDI.Fechar(Todas: Boolean);
var
  i: Integer;
begin
  if PageControl.ActivePageIndex = -1 then
    Exit;

  case Todas of
    True:
    begin
      for i := PageControl.PageCount - 1 downto 0 do
        CloseTab(i);
    end;

    False:
     // PostMessage(Self.Handle, WM_CLOSE_TAB, PageControl.ActivePageIndex, 0);
      CloseTab(PageControl.ActivePageIndex);
  end;
end;

function TTDI.Formulario(Pagina: Integer): TForm;
{retorna o fomulario da pagina em questao}
begin
  Result := nil;

  if PageControl.PageCount > Pagina then
    with PageControl.Pages[Pagina] do
      if ComponentCount > 0 then
        if Components[INDEX_FORM] is TForm then
          Result := TForm(Components[INDEX_FORM]);
end;

procedure TTDI.OnTabHide(Sender: TObject);
begin
  {quando fechar uma aba verifica se esta configurado o formulario padrao
   caso esteja configurado e nao tenha mais nem uma outra aba aberta,
   entao abre o formulario padrao}
  if Assigned(FormPadrao) and (PageControl.PageCount = 1) then
  begin
    MostrarFormulario(FormPadrao, False);
    PageControl.ActivePage := Pagina(FormPadrao);
  end;

  {PageControl.PageCount = 1;;; isso se deve pq este vento eh chamado antes da aba
   sumir de fato. Ou seja, se existir somente uma aba e este evento for chamado
   significa que esta ultima aba esta sendo fechada e depois nao sobrará nenhuma outra aba}
end;

procedure TTDI.WM_CLOSETAB(var Msg: TMessage);
begin
  CloseTab(Msg.WParam);
end;

procedure TTDI.CloseTab(Pagina: Integer);
var
  Form: TForm;
begin
  Form := Formulario(Pagina);
  if Form <> nil then
  begin
    Form.Close;
    if not Form.Visible then
      Form.Free;

    //se o formulario nao existe mais
    if Formulario(Pagina) = nil then
    begin
      {se nao setarmos o ActivePage (abaixo), quando o usuário pedir para fechar
       todas, duas vezes seguidas, o OnHide nao será executado na segunda vez e
       o FormPadrao não sera mostrado}
      PageControl.ActivePage := nil;

      PageControl.Pages[Pagina].Free;//entao deleta a pagina
      PageControl.Change();
    end;
  end;

  PageControl.Visible := PageControl.PageCount > 0;  //oculta o PageControl, caso não tenha mais nenhuma aba
end;

procedure TTDI.MenuFechar(Sender: TObject);
begin
  Fechar(False);
end;

procedure TTDI.MenuFecharTodas(Sender: TObject);
begin
  Fechar(True);
end;

procedure TTDI.CriarFormulario(Classe: TFormClass);
  {cria o formulario a partir de sua classe}
const
  EspacosBotaoFechar = '      ';  //espaço destinado p/ o botão 'fechar' de cada aba
var
  Form: TForm;
begin
  Form := TFormClass(Classe).Create(NovaAba);
  with Form do
  begin

    //configura o formulario
    Align       := alClient;
    BorderStyle := bsNone;
    Parent      := PageControl.ActivePage;//ActivePage é ultima aba criada com NovaAba


    {O evento onActive do TForm não é executado pq o que se torna ativo
     na verdade é o TTabSheet onde o formulario foi criado. Sendo assim qualquer
     coisa escrita no onActive do formulário não será executado.
     Para contornar esta situação nos passamos o evento onActive do Form para o
     evento onEnter do TTabSheet. E assim simulamos com segurança o evento onActive}
    PageControl.ActivePage.OnEnter := OnActivate;

    PageControl.ActivePage.Caption := Caption + EspacosBotaoFechar;//transfere o caption do form para o caption da aba

    Show;//mostra o formulário

    {Embora comigo nunca tenha acontecido, algumas pessoas me avisaram sobre
     uma exceção de focus que a linha abaixo gera em casos bem especificos.
     Eu deixo descomentado e se você prefefir pode comentar a linha abaixo.}
    try
      Perform(WM_NEXTDLGCTL, 0, 0);//muda o foco para o primeiro controle do formulario
    except
    end;
  end;
end;

procedure TTDI.CriarPageControl;
begin
  FPageControl := TPageControl.Create(Self);
  with PageControl do
  begin
    Visible       := False;  //cria o PageControl oculto, caso não tenha form padrão informado
    Align         := alClient;
    Parent        := Self.Parent;
    ParentWindow  := Self.Parent.Handle;
    OnDragOver    := PageControlDragOver;
    OnMouseDown   := PageControlMouseDown;
    OnDragDrop    := PageControlDragDrop;
    OnCloseClick  := PageControlCloseClick;
    StyleElements := [];  //desabilita VCL skin p/ o PageControl (se estiver habilitado, os botões p/ fechar as abas não aparecem)
  end;
end;

function TTDI.GetPageControl: TPageControl;
begin
  Result := FPageControl;
end;

destructor TTDI.Destroy;
begin
  Fechar(True);
  inherited;
end;

procedure TTDI.VisualizarAbas(Visualizador: IVisualizador;
  NaoExibir: TFormClass);
var
  i: Integer;
  Form: TForm;
begin
  {Percorre todas as abas que estão abertas}
  for i := 0 to PageControl.PageCount - 1 do
  begin
    Form := Formulario(i);//armazena a referencia ao formulário da aba

    //se o formulário pode ser exibido
    if Form.ClassType <> NaoExibir then
      Visualizador.ListarFormulario(Form);
      //notifica o visualizador sobre a existencia deste formulário
  end;
end;

procedure TTDI.PageControlCloseClick(Sender: TObject);
begin
  Fechar(False);
end;

procedure TTDI.PageControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    TPageControl(Sender).BeginDrag(False);
end;


procedure TTDI.PageControlDragDrop(Sender, Source: TObject; X, Y: Integer);
const
  TCM_GETITEMRECT = $130A;
var
  i: Integer;
  r: TRect;
begin
  with TPageControl(Sender) do
  begin
    for i := 0 to PageCount - 1 do
    begin
      Perform(TCM_GETITEMRECT, i, lParam(@r));
      if PtInRect(r, Point(X, Y)) then
      begin
        if i <> ActivePage.PageIndex then
          ActivePage.PageIndex := i;
        Exit;
      end;
    end;
  end;
end;

procedure TTDI.PageControlDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Sender = Source then
    Accept := True;
end;

end.
