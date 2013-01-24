unit uFormConfirmaFechar;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Vcl.StdCtrls;

type
  TFormConfirmaFechar = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormConfirmaFechar: TFormConfirmaFechar;

implementation

{$R *.dfm}

procedure TFormConfirmaFechar.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
    CanClose := Application.MessageBox('Confirma fechar este formulário.', 'Atenção!', MB_YESNO) = IDYES;
end;

end.
