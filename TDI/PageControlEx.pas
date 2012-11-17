{**************************************************************************************}
{                                                                                      }
{ TTabControlEx and TPageControlEx for Delphi 7 or greater - themed owner-drawing      }
{ Version 1.2 (2011-05-17)                                                             }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is CCR.TabControlEx.pas.                                           }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009 Chris Rolliston. All Rights Reserved.         }
{                                                                                      }
{**************************************************************************************}

unit PageControlEx;
{
  Changes since original release:
  - Now works when Multiline is True (v1.1).
  - Fixed TPageControlEx's background when DoubleBuffered is True (v1.1).
  - Fixed pages not drawing correctly when their individual Enabled property has been
    set to False (v1.2).

  Notes:
  - Implements TTabControl and TPageControl descendents that remain themed when the
    OwnerDraw property is set to True. This allows handling OnDrawTab just to give the
    caption of a particular tab a bold font, for example.
  - For the theming code to take effect however, TabPosition needs to be tpTop and Style
    tsTabs (this is a limitation of the Windows theming API).
  - With regard to the implementation, the only really hacky part was the need to work
    around Canvas.Handle being mysteriously changed whenever a TCM_GETXXX gets
    processed. Restoring the correct handle in a handler for Canvas.OnChanging fairly
    elegantly worked around this however (if I do say so myself...).
  - The simplest way to use the controls is as interposer classes - i.e., in your form's
    PAS, add CCR.TabControlEx to the main uses clause, and insert the following before
    the declaration of the form:

      type
        TTabControl = class(CCR.TabControlEx.TTabControlEx);
        TPageControl = class(CCR.TabControlEx.TPageControlEx);

        TMyForm = class(TForm)
        ...

    Using them this way means you don't have to install anything into the IDE.
}
interface

uses
  Windows, Messages, CommCtrl, SysUtils, Classes, Graphics, Controls,
  ExtCtrls, ComCtrls;

type
  TCustomTabControlHelper = class(TComponent)
  private
    FCurrentDC: HDC;
    FHotTabIndex: Integer;
    FHotTabRect: TRect;
    {$IF RTLVersion < 18}
    FHotTrackTimer: TTimer; //CM_MOUSELEAVE not reliable before D2006
    {$IFEND}
    FOwner: TCustomTabControl;
    FSavedWndProc: TWndMethod;
    procedure CanvasChanging(Sender: TObject);
    procedure NewWndProc(var Message: TMessage);
  protected
    function DoOwnerDrawing(DC: HDC; DrawTabs: Boolean): Boolean;
  public
    constructor Create(AOwner: TCustomTabControl); reintroduce;
    destructor Destroy; override;
    function ThemedOwnerDraw: Boolean;
    procedure UpdateHotTabIndex(Dummy: TObject = nil);
    property HotTabIndex: Integer read FHotTabIndex;
    property Owner: TCustomTabControl read FOwner;
  end;

  TTabControlEx = class(TTabControl)
  private
    FOwnerDrawHelper: TCustomTabControlHelper;
  protected
    procedure PaintWindow(DC: HDC); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TPageControlEx = class(TPageControl)
  private
    FOwnerDrawHelper: TCustomTabControlHelper;
  protected
    procedure PaintWindow(DC: HDC); override;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses Math, Themes, Types;

{ TCustomTabControlHelper }

type
  TOwnerAccess = class(TCustomTabControl);

constructor TCustomTabControlHelper.Create(AOwner: TCustomTabControl);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  FHotTabIndex := -1;
  {$IF Declared(FHotTrackTimer)}
  FHotTrackTimer := TTimer.Create(nil);
  FHotTrackTimer.Enabled := False;
  FHotTrackTimer.Interval := 500;
  FHotTrackTimer.OnTimer := UpdateHotTabIndex;
  {$IFEND}
  FSavedWndProc := AOwner.WindowProc;
  AOwner.Canvas.OnChanging := CanvasChanging;
  AOwner.ControlState := AOwner.ControlState + [csCustomPaint];
  AOwner.WindowProc := NewWndProc;
end;

destructor TCustomTabControlHelper.Destroy;
begin
  {$IF Declared(FHotTrackTimer)}
  FHotTrackTimer.Free;
  {$IFEND}
  FOwner.WindowProc := FSavedWndProc;
  inherited;
end;

procedure TCustomTabControlHelper.CanvasChanging(Sender: TObject);
begin
  if FCurrentDC <> 0 then Owner.Canvas.Handle := FCurrentDC;
end;

function TCustomTabControlHelper.DoOwnerDrawing(DC: HDC; DrawTabs: Boolean): Boolean;

  procedure DoDrawTab(const Details: TThemedElementDetails;
    Index: Integer; Active: Boolean);
  var
    R, RefR: TRect;
  begin
    R := Owner.TabRect(Index);
    if TOwnerAccess(Owner).MultiLine then
      RefR := Owner.TabRect(Owner.IndexOfTabAt(5, 5))
    else
      RefR := R;
    if Active then
    begin
      Inc(R.Bottom, RefR.Top);
      Dec(R.Left, RefR.Top);
      Inc(R.Right, RefR.Top);
      R.Top := R.Top - RefR.Top;
      if R.Left = 0 then
        ExcludeClipRect(DC, 0, R.Bottom - 1, 1, R.Bottom)
      else if R.Right >= Pred(Owner.Width) then
      begin
        R.Right := Owner.Width - 2;
        ExcludeClipRect(DC, R.Right - 1, R.Bottom - 1, R.Right, R.Bottom);
      end;
    end;
    ThemeServices.DrawElement(DC, Details, R);
    Owner.Canvas.Brush := Owner.Brush;
    Owner.Canvas.Brush.Style := bsClear;
    Owner.Canvas.Font := TOwnerAccess(Owner).Font;
    if not Active then InflateRect(R, -RefR.Top, 0);
    TOwnerAccess(Owner).DrawTab(Index, R, Active);
    if Active and Owner.Focused then
    begin
      InflateRect(R, -3, -3);
      Owner.Canvas.Brush.Color := clWhite;
      Owner.Canvas.Brush.Style := bsSolid;
      Owner.Canvas.Font := TOwnerAccess(Owner).Font;
      SetBkColor(Owner.Canvas.Handle, clWhite);
      Owner.Canvas.DrawFocusRect(R);
    end;
  end;
var
  Details: TThemedElementDetails;
  R: TRect;
  I, SelIndex: Integer;
begin
  Result := ThemedOwnerDraw;
  if not Result then Exit;
  Details := ThemeServices.GetElementDetails(ttTabItemNormal);
  SelIndex := TOwnerAccess(Owner).TabIndex;
  FCurrentDC := DC;
  try
    Owner.Canvas.Handle := DC;
    if DrawTabs then
      for I := 0 to TOwnerAccess(Owner).Tabs.Count - 1 do
        if I <> SelIndex then
        begin
          if I = FHotTabIndex then
            DoDrawTab(ThemeServices.GetElementDetails(ttTabItemHot), I, False)
          else
            DoDrawTab(Details, I, False);
        end;
    if SelIndex < 0 then
      R := Rect(0, 0, Owner.Width, Owner.Height)
    else
    begin
      R := Owner.TabRect(SelIndex);
      R.Left := 0;
      R.Top := R.Bottom;
      R.Right := Owner.Width;
      R.Bottom := Owner.Height;
    end;
    ThemeServices.DrawElement(DC, ThemeServices.GetElementDetails(ttPane), R);
    if (SelIndex >= 0) and DrawTabs then
      DoDrawTab(ThemeServices.GetElementDetails(ttTabItemSelected), SelIndex, True);
  finally
    FCurrentDC := 0;
    Owner.Canvas.Handle := 0;
  end;
end;

procedure TCustomTabControlHelper.NewWndProc(var Message: TMessage);
begin
  case Message.Msg of
    CM_MOUSEENTER: UpdateHotTabIndex;
    CM_MOUSELEAVE: if FHotTabIndex >= 0 then UpdateHotTabIndex;
    WM_MOUSEMOVE:
      with TWMMouseMove(Message) do
        if (FHotTabIndex >= 0) and ((XPos < FHotTabRect.Left) or
          (XPos >= FHotTabRect.Right) or (YPos < FHotTabRect.Top) or
          (YPos >= FHotTabRect.Bottom)) then
          UpdateHotTabIndex;
  end;
  FSavedWndProc(Message);
end;

function TCustomTabControlHelper.ThemedOwnerDraw: Boolean;
begin
  with TOwnerAccess(Owner) do
    Result := OwnerDraw and ThemeServices.ThemesEnabled and
      (Style = tsTabs) and (TabPosition = tpTop);
end;

procedure TCustomTabControlHelper.UpdateHotTabIndex(Dummy: TObject);
var
  Index, SelIndex: Integer;

  procedure CheckInvalidate;
  var
    DoIt: Boolean;
  begin
    if not TOwnerAccess(Owner).OwnerDraw then Exit;
    if ThemeServices.ThemesEnabled then
      DoIt := (FHotTabIndex <> SelIndex)
    else
      DoIt := TOwnerAccess(Owner).HotTrack;
    if DoIt then InvalidateRect(Owner.Handle, @FHotTabRect, False);
  end;
begin
  with Owner.ScreenToClient(Mouse.CursorPos) do
    Index := Owner.IndexOfTabAt(X, Y);
  if Index = FHotTabIndex then Exit;
  SelIndex := TOwnerAccess(Owner).TabIndex;
  CheckInvalidate;
  if Index >= 0 then
    FHotTabRect := Owner.TabRect(Index)
  else
    SetRectEmpty(FHotTabRect);
  FHotTabIndex := Index;
  {$IF Declared(FHotTrackTimer)}
  FHotTrackTimer.Enabled := (FHotTabIndex >= 0);
  {$IFEND}
  CheckInvalidate;
end;

{ TTabControlEx }

constructor TTabControlEx.Create(AOwner: TComponent);
begin
  inherited;
  FOwnerDrawHelper := TCustomTabControlHelper.Create(Self);
end;

procedure TTabControlEx.PaintWindow(DC: HDC);
begin
  if not FOwnerDrawHelper.DoOwnerDrawing(DC, True) then
    inherited;
end;

{ TPageControlEx }

constructor TPageControlEx.Create(AOwner: TComponent);
begin
  inherited;
  FOwnerDrawHelper := TCustomTabControlHelper.Create(Self);
end;

procedure TPageControlEx.PaintWindow(DC: HDC);
begin
  if not FOwnerDrawHelper.DoOwnerDrawing(DC, True) then
    inherited;
end;

procedure TPageControlEx.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  SavedDCIndex: Integer;
  R: TRect;
begin
  if not DoubleBuffered or not FOwnerDrawHelper.ThemedOwnerDraw then
  begin
    inherited;
    Exit;
  end;
  SavedDCIndex := SaveDC(Message.DC);
  R := DisplayRect;
  ExcludeClipRect(Message.DC, R.Left, R.Top, R.Right, R.Bottom);
  inherited;
  SelectClipRgn(Message.DC, 0);
  ExcludeClipRect(Message.DC, 0, 0, Width, R.Top);
  FOwnerDrawHelper.DoOwnerDrawing(Message.DC, False);
  RestoreDC(Message.DC, SavedDCIndex);
end;

end.
