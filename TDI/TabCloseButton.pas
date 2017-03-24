unit TabCloseButton;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, UxTheme, Themes, Math, PageControlEx;

type
  TTabCloseButton = class(TPageControlEx)
  private
    FCloseButtonsRect: array of TRect;
    FCloseButtonShowPushed: Boolean;
    FOnCloseClick: TNotifyEvent;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); override;
    procedure MouseLeave(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    property OnCloseClick: TNotifyEvent read FOnCloseClick write FOnCloseClick;
    procedure UpdateCloseButtons;
  end;

implementation

constructor TTabCloseButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
 // TabWidth     := 150;  //comentado p/ deixar a largura das abas dinâmica
  OwnerDraw    := True;
  UpdateCloseButtons;
end;

destructor TTabCloseButton.Destroy;
begin

  inherited;
end;

procedure TTabCloseButton.UpdateCloseButtons;
var
  I: Integer;
begin
//  {$IF RTLVersion < 18}
  OnMouseLeave := MouseLeave; //CM_MOUSELEAVE not reliable before D2006
//  {$IFEND}

  SetLength(FCloseButtonsRect, PageCount);

  for I := 0 to Length(FCloseButtonsRect) - 1 do
  begin
    FCloseButtonsRect[I] := Rect(0, 0, 0, 0);
  end;
end;

procedure TTabCloseButton.DrawTab(TabIndex: Integer; const Rect: TRect;
  Active: Boolean);
const
  CloseBtnSize = 14;
var
  TabCaption: TPoint;
  CloseBtnRect: TRect;
  CloseBtnDrawState: Cardinal;
  CloseBtnDrawDetails: TThemedElementDetails;
begin
  //inherited;

  if InRange(TabIndex, 0, Length(FCloseButtonsRect) - 1) then
  begin
    TabCaption.Y := Rect.Top + 3;

    if Active then
    begin
      CloseBtnRect.Top := Rect.Top + 4;
      CloseBtnRect.Right := Rect.Right - 5;
      TabCaption.X := Rect.Left + 6;
    end
    else
    begin
      CloseBtnRect.Top := Rect.Top + 3;
      CloseBtnRect.Right := Rect.Right - 5;
      TabCaption.X := Rect.Left + 3;
    end;

    CloseBtnRect.Bottom := CloseBtnRect.Top + CloseBtnSize;
    CloseBtnRect.Left := CloseBtnRect.Right - CloseBtnSize;
    FCloseButtonsRect[TabIndex] := CloseBtnRect;

    Canvas.FillRect(Rect);
    Canvas.TextOut(TabCaption.X, TabCaption.Y, Pages[TabIndex].Caption);

    if not UseThemes then
    begin
      if (TabIndex = ActivePageIndex) and FCloseButtonShowPushed then
        CloseBtnDrawState := DFCS_CAPTIONCLOSE + DFCS_PUSHED
      else
        CloseBtnDrawState := DFCS_CAPTIONCLOSE;

      Windows.DrawFrameControl(Canvas.Handle,
        FCloseButtonsRect[TabIndex], DFC_CAPTION, CloseBtnDrawState);
    end
    else
    begin
      Dec(FCloseButtonsRect[TabIndex].Left);

      if (TabIndex = ActivePageIndex) and FCloseButtonShowPushed then
        CloseBtnDrawDetails := ThemeServices.GetElementDetails(twSmallCloseButtonPushed)
      else
        CloseBtnDrawDetails := ThemeServices.GetElementDetails(twSmallCloseButtonNormal);

      ThemeServices.DrawElement(Canvas.Handle, CloseBtnDrawDetails,
        FCloseButtonsRect[TabIndex]);
    end;
  end;
end;

procedure TTabCloseButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I, tabIndex: Integer;
  p: TPoint;
begin
  inherited;
  if Button = mbLeft then
  begin
    for I := 0 to Length(FCloseButtonsRect) - 1 do
    begin
      if PtInRect(FCloseButtonsRect[I], Point(X, Y)) then
      begin
        FCloseButtonShowPushed := True;
        Repaint;
      end;
    end;
  end
  else if Button in [mbMiddle, mbRight] then
  begin
    p := Self.ScreenToClient(Mouse.CursorPos); //pega a posição do mouse
    tabIndex := Self.IndexOfTabAt(p.X, p.Y);   //pega o index da aba clicada

    if tabIndex >= 0 then
      if ActivePageIndex <> tabIndex then
        ActivePageIndex := tabIndex;  //set a aba como ativa
  end;
end;

procedure TTabCloseButton.MouseLeave(Sender: TObject);
begin
  inherited;
  FCloseButtonShowPushed := False;
  Repaint;
end;

procedure TTabCloseButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Inside: Boolean;
begin
  inherited;
  if (ssLeft in Shift) and (ActivePageIndex >= 0) then
  begin
    Inside := PtInRect(FCloseButtonsRect[ActivePageIndex], Point(X, Y));

    if FCloseButtonShowPushed <> Inside then
    begin
      FCloseButtonShowPushed := Inside;
      Repaint;
    end;
  end;
end;

procedure TTabCloseButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and (ActivePageIndex >= 0) then
  begin
    if PtInRect(FCloseButtonsRect[ActivePageIndex], Point(X, Y)) then
    begin
      if Assigned(FOnCloseClick) then
        FOnCloseClick(Pages[ActivePageIndex]);

      Repaint;
    end;
  end
  else if (Button = mbMiddle) and (ActivePageIndex >= 0) then
  begin
    if Assigned(FOnCloseClick) then
      FOnCloseClick(Pages[ActivePageIndex]);

    Repaint;
  end;
end;

end.
