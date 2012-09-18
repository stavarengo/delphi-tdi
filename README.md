# Delphi TDI

Delphi-TDI é uma classe que você pode usar para criar projetos onde os formulários são abertos em abas, assim como nos navegadores mais modernos. 

Neste projeto você vai encontrar a classe `TTDI`, esta classe encapsula todo o código necessário para você, abrir em abas, os formulários do seu projeto, esteja ele sendo desenvolvido ou já funcionando há algum tempo.

## A classe TTDI é um componente?

Não. Nenhuma instalação precisa ser feita.

## Como usar o Delphi-TDI no meu projeto?

1. Baixe a última versão da classe TTDI [aqui](https://github.com/stavarengo/delphi-tdi/tags) ou clone este repositório com `git clone https://github.com/stavarengo/delphi-tdi.git`.
1. Adicione a unit TDI.pas no seu projeto.
1. Pronto! Agora é só começar a usar.

## A classe  TTDI

Abaixo segue uma lista dos métodos e propriedades publicas da classe, bem como uma explicação sobre cada um.

* `procedure MostrarFormulario(Classe: TFormClass; Multi: Boolean);`
Abre um formulário (parâmetro `Classe`) em uma aba. O formulário será aberto em uma nova aba se o parâmetro `Multi` for `True`. Caso `Multi` seja `False`, então a aba já existente será exibida para o usuário.

* `procedure Fechar(Todas: Boolean);`
Quando o parâmetro `Todas` for `True`, todas as abas serão fechadas, caso contrário somente a aba atual será fechada.

* `FormPadrao: TFormClass;`
`FormPadrao` é o formulário que será exibido sempre que todas as abas forem fechadas. Atribuir `nil` a esta propriedade fará com que nenhum formulário seja exibido ao fechar todas as abas:

* `MostrarMenuPopup: Boolean;`
Quando True mostra o menu popup com as opções "_Fechar_" e "_Fechar todas_" quando o usuário clica com o botão auxiliar do mouse sobre uma das abas abertas:

* `PageControl: TPageControl;`
Propriedade que permite o acesso ao `TPageControl` onde as abas (`TTabSheets`) estão sendo exibidas.

## Exemplos
1. **Como saber se existe formulários abertos dentro das abas controladas pela classe TTDI?**
 
Você pode fazer está verificação de várias formas diferentes. Uma das opções é verificar o `PageCount` do atributo `PageControl`. Supondo que você criou uma instância da TTDI usando uma variável chamada `vTdi`, faça o seguinte:
```
if vTdi.PageControl.PageCount then
begin
  // tem algum formulário aberto
end;
```