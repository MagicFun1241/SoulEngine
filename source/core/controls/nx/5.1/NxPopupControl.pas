{
  Next Grid
  Copyright (C) 1996-2002 by Berg
  All rights reserved.

  $id:PopupView.pas 9/8/2004 7:10:16 bn
  $id:NxPopup.pas 3/19/2005 3:45:23 bn
}

unit NxPopupControl;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, StdCtrls, ExtCtrls,
  SysUtils, DateUtils, Dialogs, ImgList, Forms, ClipBrd,
  NxConsts, NxClasses, NxStdCtrls, NxSharedCommon, NxThemesSupport, NxScrollControl;

const
  spaCalcButtonPos = 38;
  spaImageToText = 5;
  spaColorsTop = 28;
  spaRampColorsLeft = 10;

{$IFDEF VER140}
const
  {$EXTERNALSYM WM_NCMOUSEHOVER}
  WM_NCMOUSEHOVER     = $02A0;
  {$EXTERNALSYM WM_NCMOUSELEAVE}
  WM_NCMOUSELEAVE     = $02A2;
{$ENDIF}

const
  spDateStart = 15;

  sizCalcButtonHeight = 24;
  sizCalcButtonWidth = 30;
  sizMainButtonWidth = 51;
  sizDropDownButton = 16;
  sizListItemHeight = 16;
  sizColorStep = 10;
  sizColorBox = 11;
  sizFontPreviewBox = 20;
  
type
  TDropMode = (dmOpen, dmClosed);
  TSmallButtonKind = (sbkNormal, sbkTool);
  TColorMatrix = array [0..20, 0..11] of TColor;

  TColorOverEvent = procedure (Sender: TObject; Value: TColor) of object;
  TDialogCloseEvent = procedure (Sender: TObject; Execute: Boolean) of object;
  TInputEvent = procedure (Sender: TObject; Value: string) of object;

  TNxMiniButton = class(TGraphicControl)
  private
  	FDown: Boolean;
  	FHover: Boolean;
    FGlyph: TBitmap;
    FKind: TSmallButtonKind;
    procedure SetGlyph(const Value: TBitmap);
    procedure SetKind(const Value: TSmallButtonKind);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  	procedure Paint; override;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Kind: TSmallButtonKind read FKind write SetKind;
    property Text;
    property OnMouseUp;
  end;

  TNxPopupControl = class(TNxScrollControl)
  private
    FOwner: TComponent;
  	FDropMode: TDropMode;             
    FFullOpened: Boolean;
    FMouseDown: Boolean;
    FOnDeactivate: TNotifyEvent;
    FOnInput: TInputEvent;
  protected
    function GetActiveRect: TRect; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
  	procedure CreateWnd; override;
    procedure DoDeactivate; dynamic;
    procedure DoInput(Value: string); dynamic;
    procedure AcquireValue(X, Y: Integer); virtual; abstract;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
	  procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
	  procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BeforePopup; virtual;
    procedure Popup(X, Y: Integer); virtual;
    procedure ClosePopup; virtual;
    property Canvas;
    property DropMode: TDropMode read FDropMode write FDropMode;
    property Font;
    property FullOpened: Boolean read FFullOpened write FFullOpened;
    property Text;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnInput: TInputEvent read FOnInput write FOnInput;
  end;

  TNxPopupControlClass = class of TNxPopupControl;

  TNxDrawItemEvent = procedure(Sender: TObject; Index: Integer; Rect: TRect; State: TNxItemState) of object;

  TNxPopupList = class(TNxPopupControl)
  private
    FDisplayMode: TItemsDisplayMode;
    FDropDownColor: TColor;
    FDropDownCount: Integer;
    FHighlightTextColor: TColor;
    FHoverIndex: Integer;
    FImages: TCustomImageList;
    FItemIndex: Integer;
    FItemHeight: Integer;
    FItems: TNxStrings;
    FOldHoverIndex: Integer;
    FSelectionColor: TColor;
    FShowImages: Boolean;
    FOnDrawItem: TNxDrawItemEvent;
    FDrawingOptions: TDrawingOptions;
    procedure SetDropDownColor(const Value: TColor);
    procedure SetDropDownCount(const Value: Integer);
    procedure SetHighlightTextColor(const Value: TColor);
    procedure SetHoverIndex(const Value: Integer);
    procedure SetItems(const Value: TNxStrings);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetItemIndex(const Value: Integer);
    procedure SetItemHeight(const Value: Integer);
    procedure SetSelectionColor(const Value: TColor);
    procedure SetShowImages(const Value: Boolean);
  protected
  	procedure CreateWnd; override;
    procedure DoDrawItem(Index: Integer; Rect: TRect; State: TNxItemState); dynamic;
    function GetItemCount: Integer; virtual;
    function GetValueFromIndex(const Index: Integer): string;
    function GetVertScrollMax: Integer; virtual;
    function GetVertOffset(FromPos, ToPos: Integer): Integer; override;
    function GetVisibleCount: Integer;
    procedure AcquireValue(X, Y: Integer); override;
    procedure DrawItem(Index: Integer; ARect: TRect); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure RefreshItem(Index: Integer);
    { Windows Messages }
    procedure WMNCMouseHover(var Message: TMessage); message WM_NCMOUSEHOVER;
    procedure WMNCMouseLeave(var Message: TMessage); message WM_NCMOUSELEAVE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WndProc(var Message: TMessage); override;
  public
    procedure BeforePopup; override;
    constructor Create(AOwner: TComponent); override;
    procedure Popup(X, Y: Integer); override;
    property DisplayMode: TItemsDisplayMode read FDisplayMode write FDisplayMode;
    property DrawingOptions: TDrawingOptions read FDrawingOptions write FDrawingOptions;
    property DropDownColor: TColor read FDropDownColor write SetDropDownColor;
    property DropDownCount: Integer read FDropDownCount write SetDropDownCount;
    property HoverIndex: Integer read FHoverIndex;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property ItemHeight: Integer read FItemHeight write SetItemHeight;
    property Items: TNxStrings read FItems write SetItems;
    property Images: TCustomImageList read FImages write SetImages;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor;
    property HighlightTextColor: TColor read FHighlightTextColor write SetHighlightTextColor;
    property ShowImages: Boolean read FShowImages write SetShowImages;
    property OnDrawItem: TNxDrawItemEvent read FOnDrawItem write FOnDrawItem;
  end;

  TFontPopup = class(TNxPopupList)
  protected
    procedure DrawItem(Index: Integer; ARect: TRect); override;
  end;

  TOperationButtonKind = (obDivide, obMultiply, obMinus, obPlus);

  TNxOperationButton = class(TNxMiniButton)
  public
    Kind: TOperationButtonKind;
  end;

  TMemoryButtonKind = (mbClear, mbRestore, mbSave, mbAdd);

  TNxMemoryButton = class(TNxMiniButton)
  public
    Kind: TMemoryButtonKind;
  end;

  TMainButtonKind = (nbBackspace, nbCe, nbC);

  TNxMainButton = class(TNxMiniButton)
  public
    Kind: TMainButtonKind;
  end;

  TFunctionButtonKind = (fkSqrt, fkPercent {%}, fkRatio {1/x});

  TNxFunctionButton = class(TNxMiniButton)
  public
    Kind: TFunctionButtonKind;
  end;

  TCalcButtonKind = (ckBackspace, ckCe, ckC, ckMC, ck7, ck8, ck9,
    ckDiv, ckSqrt, ckMR, ck4, ck5, ck6, ckMultiply, ckPercent, ckMS, ck1,
    ck2, ck3, ckMinus, ckRatio, ckM, ck0, ckSign, ckDot, ckPlus, ckEqual);

  TNxCalcButton = class(TNxMiniButton)
  private
   FKind: TCalcButtonKind;
  public
    property Kind: TCalcButtonKind read FKind write FKind;
  end;

  TNxCalcPopup = class(TNxPopupControl)
  private
    FButtons: array[ckBackspace..ckEqual] of TNxCalcButton;
    function GetButtonCaption(ButtonKind: TCalcButtonKind): string;
    function GetButtonFontColor(ButtonKind: TCalcButtonKind): TColor;
    function GetMemoryRect: TRect;
  protected
  	procedure CreateWnd; override;
    procedure CreateButtons; virtual;
    procedure AcquireValue(X, Y: Integer); override;
    procedure DoButtonClick(Sender: TObject);
    procedure RefreshMemoryRect;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TNxDateViewMode = (dmDays, dmMonths, dmYears);

  TNxDatePopup = class(TNxPopupControl)
  private
    FDate: TDate;
    FDay: Word;
    FDayNames: TStrings;
    FDown: Boolean;
    FMonth: Word;
    FMonthNames: TStrings;
    FNoneButton: TNxMiniButton;
    FOldRect: TRect;
		FSelectedDate: TDate;
    FStartDay: TStartDayOfWeek;
    FTodayButton: TNxMiniButton;
    FYear: Word;
    FViewMode: TNxDateViewMode;
    function GetActualStartDay: TStartDayOfWeek;
    function GetDateAtPos(ACol, ARow: Integer): TDate;
    function GetDaySize: TSize;
    function GetDayInWeekChar(DayInWeek: Integer): WideChar; // dmajkic Char -> WideChar
    function GetDayOfWeek(Day: TDateTime): Integer;
    function GetMonthName(Month: Integer): WideString;
    procedure SetDayNames(Value: TStrings);
    procedure SelectDate(X, Y: Integer);
    procedure SelectMonth(X, Y: Integer);
    procedure SelectYear(X, Y: Integer);
    procedure SetDate(const Value: TDate);
    procedure SetDay(const Value: Word);
    procedure SetMonth(const Value: Word);
    procedure SetMonthNames(const Value: TStrings);
    procedure SetPastMonth(var AYear, AMonth: Integer; Decrease: Boolean = True);
    procedure SetYear(const Value: Word);
    procedure SetViewMode(const Value: TNxDateViewMode);
    function GetMonthSize: TSize;
  protected
    function GetActiveRect: TRect; override;
    function GetMonthRect: TRect;
    function GetViewRect: TRect;
  	procedure CreateWnd; override;
    procedure AcquireValue(X, Y: Integer); override;
    procedure DoButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    	X, Y: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure PaintDay(Day: Integer; DayRect: TRect);
    procedure PaintDays(X, Y: Integer);
    procedure PaintMonth(MonthRect: TRect);
    procedure PaintMonths(X, Y: Integer);
    procedure PaintYears(X, Y: Integer);
  public
    procedure BeforePopup; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Properties }
    property DaySize: TSize read GetDaySize;
    property MonthSize: TSize read GetMonthSize;
    property NoneButton: TNxMiniButton read FNoneButton;
    property TodayButton: TNxMiniButton read FTodayButton;
    property ViewMode: TNxDateViewMode read FViewMode write SetViewMode;
  published
    property Date: TDate read FDate write SetDate;
		property Day: Word read FDay write SetDay;
    property DayNames: TStrings read FDayNames write SetDayNames;
    property StartDay: TStartDayOfWeek read FStartDay write FStartDay;
		property Month: Word read FMonth write SetMonth;
    property MonthNames: TStrings read FMonthNames write SetMonthNames;
    property SelectedDate: TDate read FSelectedDate write FSelectedDate;
    property Year: Word read FYear write SetYear;
  end;

  TNxColorPopup = class(TNxPopupControl)
  private
    FColorDialog: TColorDialog;
    FColorMatrix: TColorMatrix;
    FColorsButton: TNxMiniButton;
    FNoneButton: TNxMiniButton;
    FOnColorOver: TColorOverEvent;
    FOnDialogClose: TDialogCloseEvent;
    FSelectedColor: TColor;
    FUseColorNames: Boolean;
    FWebColorFormat: Boolean;
    procedure SetSelectedColor(const Value: TColor);
    procedure SetUseColorNames(const Value: Boolean);
    procedure SetWebColorFormat(const Value: Boolean);
  protected
    function GetValueText(AColor: TColor): string;
    procedure AcquireValue(X, Y: Integer); override;
  	procedure CreateWnd; override;
    procedure GenerateColorMatrix;
    { event handlers }
    procedure DoColorsButtonClick(Sender: TObject); dynamic;
    procedure DoColorOver(Value: TColor); dynamic;
    procedure DoDialogClose(Execute: Boolean); dynamic;
    procedure DoNoneButtonClick(Sender: TObject); dynamic;
    procedure DrawColorInfo(AColor: TColor);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ColorDialog: TColorDialog read FColorDialog;
    property ColorsButton: TNxMiniButton read FColorsButton;
    property NoneButton: TNxMiniButton read FNoneButton;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property UseColorNames: Boolean read FUseColorNames write SetUseColorNames;
    property WebColorFormat: Boolean read FWebColorFormat write SetWebColorFormat;
    property OnColorOver: TColorOverEvent read FOnColorOver write FOnColorOver;
    property OnDialogClose: TDialogCloseEvent read FOnDialogClose write FOnDialogClose;
  end;

  TNxTimePopup = class(TNxPopupControl)
  private
    FDown: Boolean;
    FHours: Word;
    FHoursString: string;
    FSelectedTime: TTime;
    FMinutes: Word;
    FMinutesString: string;
    FOldHourRect: TRect;
    FOldMinuteRect: TRect;
    FTime: TTime;
    procedure SelectHour(X, Y: Integer);
    procedure SelectMinute(X, Y: Integer);
    procedure SetHour(const Value: Word);
    procedure SetHourString(Value: string);
    procedure SetMinute(const Value: Word);
    procedure SetTime(const Value: TTime);
    procedure SetMinuteString(Value: string);
  protected
    procedure AcquireValue(X, Y: Integer); override;
    procedure CreateWnd; override;
    function GetHourRect: TRect;
    function GetMinuteRect: TRect;
    function GetTitleRect: TRect;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure PaintItem(Day: Integer; DayRect: TRect);
    procedure PaintTitle(TitleRect: TRect);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Hours: Word read FHours write SetHour;
    property HoursString: string read FHoursString write SetHourString;
    property Minutes: Word read FMinutes write SetMinute;
    property MinutesString: string read FMinutesString write SetMinuteString;
    property SelectedTime: TTime read FSelectedTime write FSelectedTime;
    property Time: TTime read FTime write SetTime;
  end;

function DelphiColorToString(const Value: TColor; UseNames: Boolean): string;
function WebColorToString(Value: TColor; UseNames: Boolean): string;

implementation

uses Types, MultiMon, NxEdit, Consts, Math, StrUtils;

function DelphiColorToString(const Value: TColor; UseNames: Boolean): string;
begin
  if UseNames then Result := ColorToString(Value)
    else FmtStr(Result, '%s%.8x', [HexDisplayPrefix, Value]);
end;

function WebColorToString(Value: TColor; UseNames: Boolean): string;
var
  i: Integer;
begin
  if Integer(Value) < 0 then Value := GetSysColor(Value and $000000FF);
  Result := '#' + IntToHex(((Value and $ff) shl 16) + (Value and $ff00) + (Value and $ff0000) shr 16, 6);
  if UseNames then
    for i := Low(WebColorNames) to High(WebColorNames) do
      if WebColorNames[i][cnColorValue] = Result then
      begin
        Result := WebColorNames[i][cnColorName];
        Break;
      end;
end;

{ TNxMiniButton }

constructor TNxMiniButton.Create(AOwner: TComponent);
begin
  inherited;
  FDown := False;
  FHover := False;
  FGlyph := TBitmap.Create;
  FKind := sbkNormal;
end;

procedure TNxMiniButton.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TNxMiniButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FDown := True;
  Invalidate;
end;

procedure TNxMiniButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TNxMiniButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FDown := False;
  Invalidate;
end;

procedure TNxMiniButton.Paint;
var
  R: TRect;
  State, X, Y: Integer;
begin
	inherited;
  if IsThemed then
  begin
    State := 1;
    case Kind of
      sbkNormal:
        begin
          if FDown then State := 3 else if FHover then State := 2 else State := 1;
          ThemeRect(Parent.Handle, Canvas.Handle, ClientRect, teButton, 1, State);
        end;
      sbkTool:
        begin
          if FHover then State := 2;
          if FDown then State := 3;
          ThemeRect(Parent.Handle, Canvas.Handle, ClientRect, teToolbar, tcToolbarButton, State);
        end;
    end;
  end else
  begin
    with Canvas do
    begin
      Brush.Color := clBtnFace;
      FillRect(ClientRect);
    end;
    R := ClientRect;
    if FDown then DrawEdge(Canvas.Handle, R, EDGE_SUNKEN, BF_RECT)
      else DrawEdge(Canvas.Handle, R, EDGE_RAISED, BF_RECT);
  end;
  if not(FGlyph.Empty) then
  begin
    X := Width div 2 - FGlyph.Width div 2;
    Y := Height div 2 - FGlyph.Height div 2;
    Canvas.Draw(X, Y, FGlyph);
  end;  
  Canvas.Font.Assign(Font);
  DrawTextRect(Canvas, ClientRect, taCenter, Text);
end;

procedure TNxMiniButton.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

procedure TNxMiniButton.SetKind(const Value: TSmallButtonKind);
begin
  FKind := Value;
  Invalidate;
end;

destructor TNxMiniButton.Destroy;
begin
  FGlyph.Free;
  FGlyph := nil;
  inherited;
end;

{ TNxPopupControl }

constructor TNxPopupControl.Create(AOwner: TComponent);
begin
  inherited;
  FOwner := AOwner;
  ControlStyle := ControlStyle + [csNoDesignVisible];
  BorderWidth := 0;
  FDropMode := dmClosed;
  FFullOpened := False;
  FMouseDown := False;
  Visible := False;
end;

procedure TNxPopupControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WndParent := GetDesktopWindow;
    Style := WS_CLIPSIBLINGS or WS_CHILD;
    ExStyle := WS_EX_TOPMOST or WS_EX_TOOLWINDOW;
    WindowClass.Style := CS_DBLCLKS or CS_SAVEBITS;
  end;
end;

procedure TNxPopupControl.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  VertScrollBar.LargeChange := 10;
end;

procedure TNxPopupControl.DoDeactivate;
begin
  if Assigned(FOnDeactivate) then FOnDeactivate(Self);
end;

procedure TNxPopupControl.DoInput(Value: string);
begin
  if Assigned(FOnInput) then FOnInput(Self, Value);
end;

procedure TNxPopupControl.Popup(X, Y: Integer);
var
  HCurrMonitor: HMONITOR;
  MonitorInf: MONITORINFO;
  R, MonRect: TRect;
  Ctrl: TControl;
  NX, NY: Integer;
  CursporPos: TPoint;
begin
  { Reset First }
  FMouseDown := False;
  FDropMode := dmClosed;

  NX := X;
  NY := Y;

  R := Rect(X, Y, X + Self.Width, Y + Self.Height);
  HCurrMonitor := MonitorFromRect(@R, MONITOR_DEFAULTTONEAREST);
  MonitorInf.cbSize := SizeOf(MonitorInf);
  GetMonitorInfo(HCurrMonitor, @MonitorInf);

  MonRect := MonitorInf.rcWork;

  if R.Left < MonRect.Left then NX := MonRect.Left;
  if R.Right > MonRect.Right then NX := MonRect.Right - Width;
  if R.Top < MonRect.Top then NY := MonRect.Top;
  if R.Bottom > MonRect.Bottom then NY := MonRect.Bottom - Height;

  Ctrl := TControl(FOwner);
  if (NY < Y) and (NY + Height > Y + Ctrl.Height) then
  begin
    NY := Y - Height;
    Ny := NY - Ctrl.Height;
  end;

  SetWindowPos(Handle, HWND_TOP, NX, NY, Width, Height, SWP_SHOWWINDOW);
  DropMode := dmOpen;
  Visible := True;
  
  Repaint;

  GetCursorPos(CursporPos);

  if not PtInRect(BoundsRect, CursporPos) then
  begin
    SetCaptureControl(Self);
  end;

  FFullOpened := True;

  { 3/1/07: focus probably not needed }
end;

procedure TNxPopupControl.CMMouseEnter(var Message: TMessage);
begin
  inherited;

end;

procedure TNxPopupControl.CMMouseLeave(var Message: TMessage);
var
  CursorPos: TPoint;
begin
  inherited;

  GetCursorPos(CursorPos);

  if not PtInRect(BoundsRect, CursorPos) then
  begin
	  if DropMode = dmOpen
      then SetCaptureControl(Self);
  end;

end;

procedure TNxPopupControl.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

procedure TNxPopupControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if FMouseDown then
  begin
    if PtInRect(GetActiveRect, Point(X, Y))
      then AcquireValue(X, Y)
      else if DropMode = dmOpen
        then ClosePopup;
  end else
    if not PtInRect(ClientRect, Point(X, Y))
      then SetCaptureControl(Self);
end;

procedure TNxPopupControl.ClosePopup;
begin
  ReleaseCapture;
  FFullOpened := False;
  with Owner as TNxPopupEdit do DroppedDown := False;
  DoDeactivate;
end;

procedure TNxPopupControl.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WM_ACTIVATEAPP: if TWMActivateApp(Message).Active = False
                      then ClosePopup;
  end;
end;

procedure TNxPopupControl.BeforePopup;
begin

end;

procedure TNxPopupControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseDown := True;
end;

procedure TNxPopupControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  CursorPos: TPoint;
begin
  inherited;

  GetCursorPos(CursorPos);
  if PtInRect(BoundsRect, CursorPos) then
  begin
    ReleaseCapture;
  end;
end;

function TNxPopupControl.GetActiveRect: TRect;
begin
  Result := ClientRect; 
end;

{ TNxPopupList }

constructor TNxPopupList.Create(AOwner: TComponent);
begin
  inherited;
  FDropDownCount := 8;
  FHoverIndex := -1;
  FItemIndex := -1;
  FItemHeight := sizListItemHeight;
  FOldHoverIndex := 0;

  { Set Prop. }
  BorderWidth := 2;
  ClientHeight := 50;
  ClientWidth := 80;
end;

procedure TNxPopupList.CreateWnd;
begin
  inherited;
  VertScrollBar.Visible := False;
end;

function TNxPopupList.GetVertOffset(FromPos, ToPos: Integer): Integer;
begin
  Result := (FromPos - ToPos) * FItemHeight;
end;

function TNxPopupList.GetVisibleCount: Integer;
begin
  Result := ClientHeight div FItemHeight; 
end;

procedure TNxPopupList.SetDropDownCount(const Value: Integer);
begin
  FDropDownCount := Value;
end;

procedure TNxPopupList.AcquireValue(X, Y: Integer);
begin
  if PtInRect(ClientRect, Point(X, Y)) then
  begin
    FItemIndex := (Y - 2) div FItemHeight + VertScrollBar.Position;
    if InRange(FItemIndex, 0, Pred(FItems.Count))
      then DoInput(Items[FItemIndex]);
  end;
end;

procedure TNxPopupList.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  { 3/1/07: *bug* MouseMove occur even if keyboard is used }
  if PtInRect(ClientRect, Point(X, Y)) then
    SetHoverIndex((Y - 2) div FItemHeight + VertScrollBar.Position);
end;

procedure TNxPopupList.Paint;
var
	i, t: Integer;
  r: TRect;
  Value: string;
  State: TNxItemState;
begin
  inherited;
  Canvas.Font := Font;
  VertScrollClipRect := ClientRect;
  t := ClientRect.Top;
 	for i := VertScrollBar.Position to GetItemCount - 1 do
  begin
    State := []; { Reset state for each item }
    r := Rect(ClientRect.Left, t, ClientRect.Left + ClientWidth, t + FItemHeight);
    with Canvas do
    begin
    	if i = FHoverIndex then
      begin
        Include(State, isSelected);
        Brush.Color := SelectionColor;
        Font.Color := HighlightTextColor;
      end else
      begin
	      Brush.Color := DropDownColor;
        Font.Color := clWindowText;
      end;
      Value := Items[i];

      { Custom Drawing? }
      case FDrawingOptions of
        doNormal:
        begin
          FillRect(r);
          DrawItem(i, r);
        end;
        doBackgroundOnly:
        begin
          FillRect(r);
          DoDrawItem(i, r, State); { event }
        end;
        doCustomOnly: DoDrawItem(i, r, State); { event }
        doCustom:
        begin
          FillRect(r);
          DrawItem(i, r);
          DoDrawItem(i, r, State); { event }
        end;
      end;
    end;
		inc(t, FItemHeight);
    if t > ClientHeight then Break;
  end;
  Canvas.Brush.Color := DropDownColor;
  Canvas.FillRect(Rect(ClientRect.Left, t, ClientRect.Right, ClientRect.Bottom));
end;

procedure TNxPopupList.RefreshItem(Index: Integer);
var
	ARect: TRect;
  Y: Integer;
begin
  Y := ItemHeight * (Index -  VertScrollBar.Position);
  ARect := Rect(0, Y, ClientWidth, Y + ItemHeight);
  InvalidateRect(Handle, @ARect, False);
end;

function TNxPopupList.GetVertScrollMax: Integer;
begin
  Result := Pred(Items.Count);
end;

procedure TNxPopupList.Popup(X, Y: Integer);
begin
  VertScrollBar.Max := GetVertScrollMax;
  VertScrollBar.Visible := Items.Count > DropDownCount;
  VertScrollBar.PageSize := 0;
  if Items.Count > 0 then
  begin
    VertScrollBar.PageSize := Min(DropDownCount, Items.Count);
  end;
  inherited Popup(X, Y);
  VertScrollBar.UpdateScrollBar;
end;

procedure TNxPopupList.SetItems(const Value: TNxStrings);
begin
  FItems := Value;
end;

procedure TNxPopupList.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
end;

procedure TNxPopupList.SetShowImages(const Value: Boolean);
begin
  FShowImages := Value;
end;

procedure TNxPopupList.SetItemIndex(const Value: Integer);
var
  Delta: Integer;
begin
  FItemIndex := Value;
  if FItemIndex >= VertScrollBar.Position + GetVisibleCount then
  begin
    Delta := FItemIndex - (VertScrollBar.Position + GetVisibleCount - 1);
    VertScrollBar.MoveBy(Delta);
  end
  else if FItemIndex < VertScrollBar.Position then
  begin
    Delta := FItemIndex - VertScrollBar.Position;
    VertScrollBar.MoveBy(Delta);
  end;
  SetHoverIndex(FItemIndex);
end;

procedure TNxPopupList.SetItemHeight(const Value: Integer);
begin
  FItemHeight := Value;
  if FItemHeight < 1 then FItemHeight := 1; 
end;

procedure TNxPopupList.SetSelectionColor(const Value: TColor);
begin
  FSelectionColor := Value;
end;

procedure TNxPopupList.DrawItem(Index: Integer; ARect: TRect);
var
  AText: TString;
  ALeft, ImageTop, Indent: Integer;
  TxtRect: TRect;
begin
  TxtRect := ARect;
  ALeft := ARect.Left;
  if (FShowImages) and (FImages <> nil) then
  begin
    ImageTop := ARect.Top + (FItemHeight div 2 - FImages.Height div 2);
   	if Index < FImages.Count then FImages.Draw(Canvas, ALeft, ImageTop, Index);
    ALeft := ARect.Left + FImages.Width + spaImageToText;
  end;
  TxtRect.Left := ALeft + 2;
  case FDisplayMode of
    dmDefault: AText := FItems[Index];
    dmNameList: AText := FItems.Names[Index];
    dmValueList: AText := GetValueFromIndex(Index);
    dmIndentList:
    begin
      Indent := StrToInt(FItems.Names[Index]) * 10;
      AText := GetValueFromIndex(Index);
      Inc(TxtRect.Left, Indent);
    end;
  end;
  TGraphicsProvider.DrawTextRect(Canvas, TxtRect, taLeftJustify, AText);
end;

function TNxPopupList.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TNxPopupList.GetValueFromIndex(const Index: Integer): string;
begin
  if Index >= 0 then
    Result := Copy(Items[Index], Length(FItems.Names[Index]) + 2, MaxInt) else
    Result := '';
end;

procedure TNxPopupList.SetHighlightTextColor(const Value: TColor);
begin
  FHighlightTextColor := Value;
end;

procedure TNxPopupList.SetHoverIndex(const Value: Integer);
begin
  FOldHoverIndex := FHoverIndex;
  FHoverIndex := Value;
  if FOldHoverIndex <> FHoverIndex then
  begin
    RefreshItem(FOldHoverIndex);
    RefreshItem(FHoverIndex);
  end;
end;

procedure TNxPopupList.SetDropDownColor(const Value: TColor);
begin
  FDropDownColor := Value;
end;

procedure TNxPopupList.DoDrawItem(Index: Integer; Rect: TRect;
  State: TNxItemState);
begin
  if Assigned(FOnDrawItem) then FOnDrawItem(Self, Index, Rect, State);
end;

procedure TNxPopupList.BeforePopup;
begin
  VertScrollBar.Max := GetVertScrollMax;
end;

procedure TNxPopupList.WMNCPaint(var Message: TMessage);
var
  DC: HDC;
  BRUSH: HBRUSH;
  R: TRect;
begin
  DC := GetWindowDC(Handle);
  GetWindowRect(Handle, R);
  OffsetRect(R, -R.Left, -R.Top);
  BRUSH := CreateSolidBrush(ColorToRGB(clGrayText));
  FrameRect(DC, R, BRUSH);
  DeleteObject(BRUSH);
  ReleaseDC(Handle, DC);
end;

procedure TNxPopupList.WndProc(var Message: TMessage);
begin
  inherited;
//  GetParentForm(Self, True).Caption := IntToStr(Message.Msg);
//  Clipboard.AsText := Clipboard.AsText + ' ' + IntToStr(Message.Msg);
end;

procedure TNxPopupList.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;

end;

procedure TNxPopupList.WMNCMouseHover(var Message: TMessage);
begin
  inherited;
//  ReleaseCapture;
end;

procedure TNxPopupList.WMNCMouseLeave(var Message: TMessage);
var
  CursorPos: TPoint;
begin
  inherited;

  GetCursorPos(CursorPos);
  if not PtInRect(BoundsRect, CursorPos) then
  begin
    SetCaptureControl(Self);
  end;
end;

{ TNxDatePopup }

constructor TNxDatePopup.Create(AOwner: TComponent);
begin
  inherited;
  FDay := 1;
  FDown := False;
  FMonth := 1;
  FYear := 1;

  FTodayButton := TNxMiniButton.Create(Self);
  FTodayButton.Parent := Self;
  FTodayButton.Visible := True;
  FTodayButton.SetBounds(20, 133, 47, 20);
  FTodayButton.Tag := 0;
  FTodayButton.OnMouseUp := DoButtonMouseUp;

  FNoneButton := TNxMiniButton.Create(Self);
  FNoneButton.Parent := Self;
  FNoneButton.Visible := True;
  FNoneButton.SetBounds(82, 133, 47, 20);
  FNoneButton.Tag := 1;
  FNoneButton.OnMouseUp := DoButtonMouseUp;

  DoubleBuffered := True;

  FDayNames := TStringList.Create;
  FMonthNames := TStringList.Create;

  FViewMode := dmDays;
end;

destructor TNxDatePopup.Destroy;
begin
  FTodayButton.Free;
  FNoneButton.Free;
  FDayNames.Free;
  FMonthNames.Free;
  inherited;
end;

procedure TNxDatePopup.CreateWnd;
begin
  inherited;
  VertScrollBar.Visible := False;
end;

function TNxDatePopup.GetMonthRect: TRect;
begin
  Result := Rect(5, 5, ClientWidth - 5, 5 + DaySize.cy);
end;

function TNxDatePopup.GetDayInWeekChar(DayInWeek: Integer): WideChar;
begin
  Result := ' ';
  if Assigned(FDayNames) and InRange(DayInWeek, 1, 7)
    and (FDayNames.Count >= 7) then Result := WideChar(FDayNames[DayInWeek - 1][1]) else
  case GetActualStartDay of
    dwSunday: Result := WideChar(LocalSettings.LongDayNames[DayInWeek][1]);
    dwMonday: if DayInWeek = 7 then Result := WideChar(LocalSettings.LongDayNames[1][1])
      else Result := WideChar(LocalSettings.LongDayNames[DayInWeek + 1][1]);
  end;
end;

function TNxDatePopup.GetDateAtPos(ACol, ARow: Integer): TDate;
var
	PastMonth, PastYear, StartingDay, LastDayInPastMonth, DaysInPastMonth, delta: Integer;
  p, ADay: Integer;
begin
	p := ARow * 7 + ACol;
  StartingDay := GetDayOfWeek(StartOfAMonth(Year, Month));
  if p < StartingDay - 1 then
  begin
    PastYear := Year;
    PastMonth := Month;
    SetPastMonth(PastYear, PastMonth);
    LastDayInPastMonth := GetDayOfWeek(EndOfAMonth(PastYear, PastMonth));
    DaysInPastMonth := DaysInAMonth(PastYear, PastMonth);
    delta := LastDayInPastMonth - (ACol + 1);
    Result := EncodeDate(PastYear, PastMonth, DaysInPastMonth - delta);
  end
  else if p + 2 > StartingDay + DaysInAMonth(Year, Month) then
  begin
    PastYear := Year;
    PastMonth := Month;
    SetPastMonth(PastYear, PastMonth, False);
    ADay := p - (StartingDay + DaysInAMonth(Year, Month)) + 2;
    Result := EncodeDate(PastYear, PastMonth, ADay);
  end
  else Result := EncodeDate(Year, Month, (ARow * 7) + (ACol - StartingDay + 2));
end;

function TNxDatePopup.GetMonthName(Month: Integer): WideString;
begin
  if Assigned(FMonthNames) and InRange(Month, 1, 12)
    and (FMonthNames.Count >= 12)
      then Result := FMonthNames[Month - 1]
        else Result := LocalSettings.LongMonthNames[Month];
end;

procedure TNxDatePopup.SetDate(const Value: TDate);
begin
  FDate := Value;
  FSelectedDate := Value;
  Year := YearOf(FDate);
  Month := MonthOf(FDate);
  Day := DayOf(FDate);
end;

procedure TNxDatePopup.SetDay(const Value: Word);
begin
  FDay := Value;
end;

procedure TNxDatePopup.SelectDate(X, Y: Integer);
var
	l, t, col, row, ys: Integer;
	r: TRect;
  sd: TDate;
begin
	col := (X - spDateStart) div DaySize.cx;

  ys := GetMonthRect.Bottom + DaySize.cy;
  row := (y - ys) div DaySize.cy;

  sd := GetDateAtPos(col, row);
  if sd = SelectedDate then Exit else SelectedDate := sd;

  l := spDateStart + (col * DaySize.cx);
  t := ys + (row * DaySize.cy);

  r := Bounds(l, t, DaySize.cx, DaySize.cy);

  InvalidateRect(Handle, @FOldRect, False);
  InvalidateRect(Handle, @r, False);
end;

procedure TNxDatePopup.SetMonth(const Value: Word);
begin
  FMonth := Value;
end;

procedure TNxDatePopup.SetPastMonth(var AYear, AMonth: Integer; Decrease: Boolean);
begin
	case Decrease of
  	True: if AMonth > 1 then AMonth := AMonth - 1 else
				  begin
				    AYear := AYear - 1;
				    AMonth := 12;
				  end;
    False:	if AMonth < 12 then AMonth := AMonth + 1 else
					  begin
					    AYear := AYear + 1;
					    AMonth := 1;
					  end;
  end;
end;

procedure TNxDatePopup.SetYear(const Value: Word);
begin
  FYear := Value;
end;

procedure TNxDatePopup.AcquireValue(X, Y: Integer);
var
  ys: Integer;
begin
  ys := GetMonthRect.Bottom + DaySize.cy;
  if PtInRect(Rect(spDateStart, ys, ClientWidth - spDateStart, ClientHeight - 20), Point(X, Y))
    then DoInput(DateTimeToStr(GetDateAtPos((X - spDateStart) div DaySize.cx, (Y - ys) div DaySize.cy)));
end;

procedure TNxDatePopup.DoButtonMouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
  if TNxMiniButton(Sender).Tag = 0 then DoInput(DateTimeToStr(Today));
  if TNxMiniButton(Sender).Tag = 1 then DoInput('');
end;

procedure TNxDatePopup.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  ys: Integer;
begin
  inherited;
  if PtInRect(Rect(GetMonthRect.Left, GetMonthRect.Top, 15, GetMonthRect.Bottom), Point(X, Y)) then
  begin
    if Month > 1 then Month := Month - 1 else
    begin
	    Year := Year - 1;
      Month := 12;
    end;
    Invalidate;
  end;
  if PtInRect(Rect(GetMonthRect.Right - 15, GetMonthRect.Top, ClientWidth, GetMonthRect.Bottom), Point(X, Y)) then
  begin
    if Month < 12 then Month := Month + 1 else
    begin
	    Year := Year + 1;
      Month := 1;
    end;
    Invalidate;
  end;

  case ViewMode of
    dmDays:
    begin
      ys := GetMonthRect.Bottom + 7 * DaySize.cy;
      if PtInRect(Rect(spDateStart, GetMonthRect.Bottom + DaySize.cy, ClientWidth - spDateStart, ys), Point(X, Y)) then
      begin
        FDown := True;
        SelectDate(X, Y);
      end;
    end;
    dmMonths:
    begin
      SelectMonth(X, Y);
    end;
    dmYears:
    begin
      SelectYear(X, Y);
    end;
  end;

end;

procedure TNxDatePopup.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (FDown) and (PtInRect(Rect(14, 38, ClientWidth - 14, ClientHeight - 20), Point(X, Y))) then
  begin
    SelectDate(X, Y);
  end;
end;

procedure TNxDatePopup.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin

  if PtInRect(GetMonthRect, Point(X, Y)) then
  begin

    if PtInRect(GetViewRect, Point(X, Y)) then
      if ViewMode = dmDays
        then ViewMode := dmMonths
          else if ViewMode = dmMonths then ViewMode := dmYears
            else ViewMode := dmDays;

  end else                                
    if (ViewMode = dmDays) or not PtInRect(ClientRect, Point(X, Y)) then inherited;

  FDown := False;
end;

procedure TNxDatePopup.Paint;
var
	X, Y: Integer;
begin
  inherited;
  X := 0;
  Y := GetMonthRect.Bottom;
  with Canvas do
  begin
    Font.Assign(Self.Font);
    Font.Color := clWindowText;
    Brush.Color := Self.Color;
    Pen.Color := clGrayText;
    Rectangle(0, 0, ClientWidth, ClientHeight);
    PaintMonth(GetMonthRect);

    case ViewMode of
      dmDays:
      begin
        X := spDateStart;
        PaintDays(X, Y);
      end;
      dmMonths: PaintMonths(X, Y);
      dmYears: PaintYears(X, Y);
    end;


  end;
end;

procedure TNxDatePopup.PaintDay(Day: Integer; DayRect: TRect);
var
  StringText: string;
begin
  with Canvas.Brush do
  begin
    Style := bsClear;
    StringText := IntToStr(Day);
    TGraphicsProvider.DrawWrapedTextRect(Canvas, DayRect, taRightJustify, taVerticalCenter,
      False, StringText, bdLeftToRight);
    Style := bsSolid;
  end;
end;

procedure TNxDatePopup.PaintMonth(MonthRect: TRect);
var
  PosY: Integer;
begin
  with Canvas do
  begin
    if not IsThemed	then Brush.Color := clBtnFace
      else Brush.Color := TGraphicsProvider.BlendColor(clGradientInactiveCaption, clWindow, 249);

    FillRect(MonthRect);
    Font.Color := clWindowText;
    DrawTextRect(Canvas, MonthRect, taCenter, GetMonthName(Month) + ' ' + IntToStr(Year));

    { draw left and right arrow }
    Brush.Color := clWindowText;
    Pen.Color := clWindowText;
    PosY := MonthRect.Top + (MonthRect.Bottom - MonthRect.Top) div 2 - 8 div 2;
    Polygon([
      Point(15, PosY),
      Point(15, PosY + 8),
      Point(11, PosY + 4)]);
    Polygon([
      Point(ClientWidth - 15, PosY),
      Point(ClientWidth - 15, PosY + 8),
      Point(ClientWidth - 11, PosY + 4)]);
  end;
end;

procedure TNxDatePopup.SetMonthNames(const Value: TStrings);
begin
  FMonthNames.Assign(Value);
end;

function TNxDatePopup.GetDayOfWeek(Day: TDateTime): Integer;
begin
  Result := 0;
  case GetActualStartDay of
    dwSunday: Result := DayOfWeek(Day);
    dwMonday: Result := DayOfTheWeek(Day);
  end;
end;

function TNxDatePopup.GetActualStartDay: TStartDayOfWeek;
var
  D: array[0..1] of char;
begin
  case FStartDay of
    dwAutomatic:
    begin
      GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IFIRSTDAYOFWEEK, D, SizeOf(D));
      case StrToInt(D) of
        0: Result := dwMonday;
        else Result := dwSunday;
      end;
    end
    else Result := FStartDay;
  end;
end;

procedure TNxDatePopup.SetDayNames(Value: TStrings);
begin
  FDayNames.Assign(Value);
end;

function TNxDatePopup.GetDaySize: TSize;
var
  Metrics: TTextMetric;
begin
  Canvas.Font.Assign(Font);
  GetTextMetrics(Canvas.Handle, Metrics);

  Result.cx := Metrics.tmHeight + 2;
  Result.cy := Metrics.tmHeight;

  Inc(Result.cx, 2);
  Inc(Result.cy, 2);
end;

procedure TNxDatePopup.BeforePopup;
begin
  inherited;
  FViewMode := dmDays;
  Width := DaySize.cx * 7 + spDateStart * 2;
  Height := GetMonthRect.Bottom + DaySize.cy * 7 + DaySize.cy + 20;
end;

function TNxDatePopup.GetActiveRect: TRect;
begin
  Result := Bounds(spDateStart, GetMonthRect.Bottom + DaySize.cy, DaySize.cx * 7, DaySize.cy * 6);
end;

procedure TNxDatePopup.PaintDays(X, Y: Integer);
var
  i, dy, dm, dd, diw, p: Integer;
  r: TRect;
begin
  with Canvas do
  begin
    p := 0;

    for i := 1 to 7 do
    begin
      TGraphicsProvider.DrawTextRect(Canvas, Bounds(X, Y, DaySize.cx, DaySize.cy), taRightJustify, GetDayInWeekChar(i));
      Inc(X, DaySize.cx);
    end;

	  X := spDateStart;
    Inc(Y, DaySize.cy);

    Pen.Color := clGrayText;
    MoveTo(spDateStart, Y - 1);
    LineTo(ClientWidth - spDateStart, Y - 1);

    dy := Year;
    dm := Month;
    diw := 1;
    if GetDayOfWeek(StartOfAMonth(Year, Month)) > 1 then
    begin
      SetPastMonth(dy, dm);
      dd := DaysInAMonth(dy, dm) - GetDayOfWeek(StartOfAMonth(Year, Month)) + 2;
    end else dd := 1;

    while p < 42 do
    begin
    	r := Rect(X, Y, X + DaySize.cy, Y + DaySize.cy);
      if dm = Month then Font.Color := clWindowText else Font.Color := clGrayText;
      if EncodeDate(dy, dm, dd) = SelectedDate then
      begin
      	if IsThemed then Brush.Color := RGB(251, 230, 148) else Brush.Color := clSilver;
        FillRect(Rect(X, Y, X + DaySize.cx, Y + DaySize.cy));
        FOldRect := Rect(X, Y, X + DaySize.cx, Y + DaySize.cy);
      end;
      if EncodeDate(dy, dm, dd) = Today then
      begin
      	if IsThemed then Brush.Color := clMaroon else Brush.Color := clHighlight;
        FrameRect(Rect(X, Y, X + DaySize.cx, Y + DaySize.cy));
      end;
      PaintDay(dd, r);
//      TGraphicsProvider.DrawTextRect(Canvas, r, taRightJustify, IntToStr(dd));
      Inc(p);
      Inc(dd);
      if dd > DaysInAMonth(dy, dm) then
			begin
	      SetPastMonth(dy, dm, False);
        dd := 1;
			end;
      Inc(X, DaySize.cx);
      Inc(diw);
      if diw > 7 then
      begin
        X := spDateStart;
        Inc(Y, DaySize.cy);
        diw := 1;
      end;
    end;

    Pen.Color := clGrayText;
    MoveTo(spDateStart, Y);
    LineTo(ClientWidth - spDateStart, Y);
  end;
end;

procedure TNxDatePopup.PaintMonths(X, Y: Integer);
var
  i: Integer;
  MonthRect: TRect;
  S: string;
begin
  X := 5;
  for i := 1 to 12 do
  begin
    MonthRect := Bounds(X, Y, MonthSize.cx, MonthSize.cy);

    S := GetMonthName(i);
    S := LeftStr(S, 3);

    if i = Month then
      with Canvas do
      begin
        if IsThemed then Brush.Color := clMaroon else Brush.Color := clHighlight;
        FrameRect(MonthRect);
      end;

    DrawTextRect(Canvas, MonthRect, taCenter, S);

    Inc(X, MonthSize.cx);
    if i mod 4 = 0 then
    begin
      X := 5;
      Inc(Y, MonthSize.cy);
    end;
  end;
end;

procedure TNxDatePopup.PaintYears(X, Y: Integer);
var
  i, yr: Integer;
  YearRect: TRect;
  S: string;
begin
  yr := Year - 1;
  X := 5;
  for i := 1 to 12 do
  begin
    YearRect := Bounds(X, Y, MonthSize.cx, MonthSize.cy);

    S := IntToStr(yr);

    if yr = Year then
      with Canvas do
      begin
        if IsThemed then Brush.Color := clMaroon else Brush.Color := clHighlight;
        FrameRect(YearRect);
      end;

    DrawTextRect(Canvas, YearRect, taCenter, S);

    Inc(yr);
    
    Inc(X, MonthSize.cx);
    if i mod 4 = 0 then
    begin
      X := 5;
      Inc(Y, MonthSize.cy);
    end;
  end;
end;

procedure TNxDatePopup.SetViewMode(const Value: TNxDateViewMode);
begin
  if Value <> FViewMode then
  begin
    FViewMode := Value;
    Invalidate;
  end;
end;

function TNxDatePopup.GetMonthSize: TSize;
begin
  with Result do
  begin
    cx := (ClientWidth - 10) div 4;
    cy := DaySize.cy * 7 div 3;
  end;
end;

function TNxDatePopup.GetViewRect: TRect;
begin
  Result := Rect(15, 5, ClientWidth - 15, 5 + DaySize.cy);
end;

procedure TNxDatePopup.SelectMonth(X, Y: Integer);
var
  Offset: TPoint;
  c, r: Integer;
begin
  if PtInRect(Bounds(5, GetMonthRect.Bottom, MonthSize.cx * 4, MonthSize.cy * 3), Point(X, Y)) then
  begin
    Offset := Point(5, GetMonthRect.Bottom);

    c := (X - Offset.X) div MonthSize.cx + 1;
    r := (Y - Offset.Y) div MonthSize.cy;

    Month := c + r * 4;

    Invalidate;
  end;
end;

procedure TNxDatePopup.SelectYear(X, Y: Integer);
var
  Offset: TPoint;
  c, r: Integer;
begin
  if PtInRect(Bounds(5, GetMonthRect.Bottom, MonthSize.cx * 4, MonthSize.cy * 3), Point(X, Y)) then
  begin
    Offset := Point(5, GetMonthRect.Bottom);

    c := (X - Offset.X) div MonthSize.cx;
    r := (Y - Offset.Y) div MonthSize.cy;

    Year := Year + (c + r * 4) - 1;

    Invalidate;
  end;
end;

{ TNxColorPopup }

constructor TNxColorPopup.Create(AOwner: TComponent);
begin
  inherited;
  FColorsButton := TNxMiniButton.Create(Self);
  FColorDialog := TColorDialog.Create(Self);
  FNoneButton := TNxMiniButton.Create(Self);
  with FColorsButton do
  begin
    Parent := Self;
    Left := 160;
    Top := 4;
    Width := 20;
    Height := 20;
    Visible := True;
//    Kind := sbkTool;
    OnClick := DoColorsButtonClick;
    Glyph.LoadFromResourceName(HInstance, 'MORECOLORSBUTTON');
    Glyph.TransparentColor := clLime;
    Glyph.Transparent := True;
  end;
  with FNoneButton do
  begin
    Parent := Self;
    Left := 180;
    Top := 4;
    Width := 20;
    Height := 20;
    Visible := True;
//    Kind := sbkTool;
    OnClick := DoNoneButtonClick;
    Glyph.LoadFromResourceName(HInstance, 'NONECOLOR');
  end;
  FUseColorNames := False;
  GenerateColorMatrix;
end;

destructor TNxColorPopup.Destroy;
begin
  FreeAndNil(FColorsButton);
  FreeAndNil(FColorDialog);
  FreeAndNil(FNoneButton);
  inherited;
end;

procedure TNxColorPopup.CreateWnd;
begin
  inherited;
  VertScrollBar.Visible := False;
  BorderWidth := 0;
end;

procedure TNxColorPopup.GenerateColorMatrix;
var
  i, j, r, g, b, c: Integer;
begin
  r := 0;
  g := 0;
  b := 0;
  c := 0;
  for i := 0 to 11 do
  begin
    FColorMatrix[0, i] := clBlack;
    FColorMatrix[2, i] := clBlack;
  end;
  for i := 0 to 5 do
  begin
    FColorMatrix[1, i] := RGB(c, c, c);
    c := c + 51;
  end;
  FColorMatrix[1, 6] := RGB(255, 0, 0);
  FColorMatrix[1, 7] := RGB(0, 255, 0);
  FColorMatrix[1, 8] := RGB(0, 0, 255);
  FColorMatrix[1, 9] := RGB(255, 255, 0);
  FColorMatrix[1, 10] := RGB(0, 255, 255);
  FColorMatrix[1, 11] := RGB(255, 0, 255);

  for i := 0 to 11 do
  begin
    for j := 3 to 20 do
    begin
      FColorMatrix[j, i] := RGB(r, g, b);
      Inc(g, 51);
      if g > 255 then
      begin
        g := 0;
        Inc(r, 51);
      end;
    end;
    Inc(b, 51);
    g := 0;
    if i = 5 then b := 0;
    if i < 5 then r := 0 else r := 153;
  end;
end;

procedure TNxColorPopup.SetSelectedColor(const Value: TColor);
begin
  FSelectedColor := Value;
end;

procedure TNxColorPopup.SetUseColorNames(const Value: Boolean);
begin
  FUseColorNames := Value;
end;

procedure TNxColorPopup.SetWebColorFormat(const Value: Boolean);
begin
  FWebColorFormat := Value;
end;

function TNxColorPopup.GetValueText(AColor: TColor): string;
begin
  case WebColorFormat of
    True: Result := WebColorToString(AColor, FUseColorNames);
    False: Result := DelphiColorToString(AColor, FUseColorNames);
  end;
end;

procedure TNxColorPopup.AcquireValue(X, Y: Integer);
var
  Col, Row: Integer;
begin
  Col := X div sizColorStep;
  Row := (Y - spaColorsTop) div sizColorStep;
  if (Col >= 0) and (Col <= 20) and (Row >= 0) and (Row <= 11) then
  case WebColorFormat of
    True: DoInput(WebColorToString(FColorMatrix[Col, Row], FUseColorNames));
    False: DoInput(DelphiColorToString(FColorMatrix[Col, Row], FUseColorNames));
  end else DoInput('');
end;

procedure TNxColorPopup.DrawColorInfo(AColor: TColor);
var
  TextInCorner: string;
  ARect: TRect;
begin
  with Canvas do
  begin
    Font.Assign(Self.Font);
    Brush.Color := clBtnFace;
    TextInCorner := GetValueText(AColor);
    Canvas.TextRect(Rect(45, 8, 105, 25), 45, 8, TextInCorner);
    Pen.Color := clBlack;
    if AColor <> clNone then
    begin
      Brush.Color := AColor;
      Rectangle(4, 4, 39, 24);
    end else
    begin
      ARect := Rect(4, 4, 39, 24);
      Brush.Color := clWhite;
      Rectangle(ARect);
      Pen.Color := clRed;
      InflateRect(ARect, -1, -1);
      MoveTo(ARect.Left, ARect.Top);
      LineTo(ARect.Right, ARect.Bottom);
      MoveTo(ARect.Left, ARect.Bottom - 1);
      LineTo(ARect.Right, ARect.Top - 1);
    end;
  end;
end;

procedure TNxColorPopup.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

end;

procedure TNxColorPopup.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
  Value: TColor;
begin
  inherited;
  if PtInRect(ClientRect, Point(X, Y)) then
  begin
    Col := X div sizColorStep;
    Row := (Y - spaColorsTop) div sizColorStep;
    if (Col >= 0) and (Col <= 20) and (Row >= 0) and (Row <= 11)
      then Value := FColorMatrix[Col, Row] else Value := SelectedColor;
    DrawColorInfo(Value);
    DoColorOver(Value);
  end;
end;

procedure TNxColorPopup.DoColorsButtonClick(Sender: TObject);
begin
  ClosePopup;
  if FColorDialog.Execute then
  begin
    case WebColorFormat of
      False:
        if UseColorNames then DoInput(ColorToString(FColorDialog.Color))
          else DoInput('$' + IntToHex(FColorDialog.Color, 8));
      True: DoInput(HTMLColorToString(FColorDialog.Color));
    end;
    DoDialogClose(True);
  end else DoDialogClose(False);
end;

procedure TNxColorPopup.DoColorOver(Value: TColor);
begin
  if Assigned(FOnColorOver) then FOnColorOver(Self, Value);
end;

procedure TNxColorPopup.DoDialogClose(Execute: Boolean);
begin
  if Assigned(FOnDialogClose) then FOnDialogClose(Self, Execute);
end;

procedure TNxColorPopup.DoNoneButtonClick(Sender: TObject);
begin
//  ClosePopup;
  DoInput('');
end;

procedure TNxColorPopup.Paint;
var
  i, j, x, y: Integer;
  ColorBoxRect, SelectionRect: TRect;
begin
  inherited;
  y := spaColorsTop;

  with Canvas do
  begin
    Brush.Color := clBtnFace;
    FillRect(ClientRect);
    Pen.Color := clBlack;

    for i := 0 to 11 do
    begin
      x := 0;
      for j := 0 to 20 do
      begin
        Brush.Color := FColorMatrix[j, i];
      	ColorBoxRect := Rect(x, y, x + sizColorBox, y + sizColorBox);
        if SelectedColor = FColorMatrix[j, i] then SelectionRect := ColorBoxRect;
        Rectangle(ColorBoxRect);
        x := x + sizColorStep;
      end;
      y := y + sizColorStep;
    end;
    if SelectedColor <> clNone then
    begin
      Brush.Color := clWhite;
      FrameRect(SelectionRect);
    end;
  end;
  DrawColorInfo(SelectedColor);
end;

{ TFontPopup }

procedure TFontPopup.DrawItem(Index: Integer; ARect: TRect);
var
  TextLeft: Integer;
  TxtRect: TRect;
begin
  TxtRect := ARect;
  TextLeft := 2;
  TxtRect.Left := TextLeft;
  with Canvas do
  begin
    Font.Size := ItemHeight - 8;
    Font.Name := Items[Index];
  end;
  TGraphicsProvider.DrawTextRect(Canvas, TxtRect, taLeftJustify, Items[Index]);
end;

{ TNxCalcPopup }

constructor TNxCalcPopup.Create(AOwner: TComponent);
begin
  inherited;
  CreateButtons;
end;

destructor TNxCalcPopup.Destroy;
var
  I: Integer;
  K: TCalcButtonKind;
begin
  K := ckBackspace;
  for I := 0 to Length(FButtons) - 1 do
  begin
    FButtons[K].Free;
    if K < High(TCalcButtonKind) then Inc(K);
  end;
  inherited;
end;

procedure TNxCalcPopup.CreateWnd;
begin
  inherited;
  BorderWidth := 0;
end;

function TNxCalcPopup.GetButtonCaption(
  ButtonKind: TCalcButtonKind): string;
begin
  case ButtonKind of
    ckBackspace: Result := 'Back';
    ckCe: Result := 'CE';
    ckC: Result := 'C';
    ckMC: Result := 'MC';
    ck7: Result := '7';
    ck8: Result := '8';
    ck9: Result := '9';
    ckDiv: Result := '/';
    ckSqrt: Result := 'sqrt';
    ckMR: Result := 'MR';
    ck4: Result := '4';
    ck5: Result := '5';
    ck6: Result := '6';
    ckMultiply: Result := '*';
    ckPercent: Result := '%';
    ckMS: Result := 'MS';
    ck1: Result := '1';
    ck2: Result := '2';
    ck3: Result := '3';
    ckMinus: Result := '-';
    ckRatio: Result := '1/x';
    ckM: Result := 'M+';
    ck0: Result := '0';
    ckSign: Result := '+/-';
    ckDot: Result := '.';
    ckPlus: Result := '+';
    ckEqual: Result := '=';
  end;
end;

function TNxCalcPopup.GetButtonFontColor(
  ButtonKind: TCalcButtonKind): TColor;
begin
  case ButtonKind of
    ck1, ck2, ck3, ck4, ck5, ck6, ck7, ck8, ck9, ck0, ckSign,
    ckDot, ckSqrt, ckPercent, ckRatio: Result := clBlue;
    else Result := clRed;
  end;
end;

function TNxCalcPopup.GetMemoryRect: TRect;
begin
  Result := Rect(6, 6, 28, 28);
end;

procedure TNxCalcPopup.CreateButtons;
var
  BtnKind: TCalcButtonKind;
  I, J, X, Y: Integer;
begin
  J := 0;

  { create top buttons }
  X := 36;
  Y := 4;
  BtnKind := ckBackspace;
  for I := 0 to 2 do
  begin
    FButtons[BtnKind] := TNxCalcButton.Create(Self);
    with FButtons[BtnKind] do
    begin
      Parent := Self;
      Kind := BtnKind;
      Caption := GetButtonCaption(Kind);
      Font.Color := GetButtonFontColor(Kind);
      OnClick := DoButtonClick;
      SetBounds(X, Y, sizMainButtonWidth, sizCalcButtonHeight);
    end;
    if BtnKind < High(TCalcButtonKind) then Inc(BtnKind);
    Inc(X, sizMainButtonWidth + 2);
  end;

  { create all buttons }
  X := 4;
  Y := 32;
  for I := 3 to Length(FButtons) - 1 do
  begin
    FButtons[BtnKind] := TNxCalcButton.Create(Self);
    with FButtons[BtnKind] do
    begin
      Parent := Self;
      Kind := BtnKind;
      Caption := GetButtonCaption(Kind);
      Font.Color := GetButtonFontColor(Kind);
      OnClick := DoButtonClick;
      SetBounds(X, Y, sizCalcButtonWidth, sizCalcButtonHeight);
      Inc(X, sizCalcButtonWidth + 2);
      Inc(J);
      if J > 5 then
      begin
        J := 0;
        X := 4;
        Inc(Y, sizCalcButtonHeight + 2);
      end;
      if BtnKind < High(TCalcButtonKind) then Inc(BtnKind);
    end;
  end;
end;

procedure TNxCalcPopup.AcquireValue(X, Y: Integer);
begin
  inherited;

end;

procedure TNxCalcPopup.DoButtonClick(Sender: TObject);
begin
  with Sender as TNxCalcButton, FOwner as TNxCalcEdit do
  case Kind of
    ckBackspace: Backspace;
    ckCe: Erase;
    ckC: ClearValue;
    ckMC: begin
            ClearMemory;
            RefreshMemoryRect;
          end;
    ck7: AddNumber(7);
    ck8: AddNumber(8);
    ck9: AddNumber(9);
    ckDiv: Divide;
    ckSqrt: Sqrt;
    ckMR: RestoreMemory;
    ck4: AddNumber(4);
    ck5: AddNumber(5);
    ck6: AddNumber(6);
    ckMultiply: Multiply;
    ckPercent: ;
    ckMS: begin
            Memory;
            RefreshMemoryRect;
          end;
    ck1: AddNumber(1);
    ck2: AddNumber(2);
    ck3: AddNumber(3);
    ckMinus: Minus;
    ckRatio: Ratio;
    ckM:  begin
            Memory;
            RefreshMemoryRect;
          end;
    ck0: AddNumber(0);
    ckSign: Sign;
    ckDot: AddDot;
    ckPlus: Plus;
    ckEqual: Equal;
  end;
end;

procedure TNxCalcPopup.RefreshMemoryRect;
var
  R: TRect;
begin
  R := GetMemoryRect;
  InvalidateRect(Handle, @R, False);
end;

procedure TNxCalcPopup.Paint;
var
  R: TRect;
begin
  inherited;
  if IsThemed then
  begin
    ThemeRect(Handle, Canvas.Handle, ClientRect, teTab, tcTabPage, 1);
    ThemeRect(Handle, Canvas.Handle, GetMemoryRect, teEdit, 1, 1);
  end else
  begin
    with Canvas do
    begin
      Brush.Color := clBtnFace;
      FillRect(ClientRect);
      R := ClientRect;
      DrawEdge(Canvas.Handle, R, EDGE_RAISED, BF_RECT);
      R := GetMemoryRect;
      DrawEdge(Canvas.Handle, R, EDGE_SUNKEN, BF_RECT);
    end;
  end;
  with FOwner as TNxCalcEdit do
  begin
    if not IsMemoryEmpty then
    begin
      DrawTextRect(Self.Canvas, GetMemoryRect, taCenter, 'M');
    end;
  end;
end;

{ TNxTimePopup }

constructor TNxTimePopup.Create(AOwner: TComponent);
begin
  inherited;
  FHours := 12;
  FMinutes := 0;

  DoubleBuffered := True;

  FHoursString := 'H';
  FMinutesString := 'M';
end;

destructor TNxTimePopup.Destroy;
begin
  inherited;
end;

procedure TNxTimePopup.CreateWnd;
begin
  inherited;
  VertScrollBar.Visible := False;
end;

function TNxTimePopup.GetTitleRect: TRect;
begin
  Result := Rect(5, 5, ClientWidth - 5, 5 + 16);
end;

function TNxTimePopup.GetHourRect: TRect;
begin
  Result := Rect(5, 5, 10 + (ClientWidth - 10) div 3 * 2, 5 + 16);
end;

function TNxTimePopup.GetMinuteRect: TRect;
begin
  Result := Rect(5 + (ClientWidth - 10) div 3 * 2, 5, ClientWidth - 5, 5 + 16);
end;

procedure TNxTimePopup.SetTime(const Value: TTime);
begin
  FTime := Value;
  FSelectedTime := Value;
  FHours := HourOf(FTime);
  FMinutes := MinuteOf(FTime);
end;

procedure TNxTimePopup.SelectHour(X, Y: Integer);
var
    l, t, col, row: Integer;
    r: TRect;
begin
  if (X - 5) < ((ClientWidth-10) div 3) then
    col := 1
  else
    col := 2;

  row := (y - 21) div 15 + 1;

  if col = 1 then
    begin
      if (row-1) = FHours then
        Exit;

      FHours := row-1;
    end
  else
    begin
      if (row+11) = FHours then
        Exit;

      FHours := row+11;
    end;

  if (FHours < 24) then
  SelectedTime := EncodeTime(FHours, FMinutes, 0, 0);

  l := 6 + col * 30;
  t := 22 + row * 15;
  r := Rect(l, t, l + 28, t + 13);
  InvalidateRect(Handle, @FOldHourRect, False);
  InvalidateRect(Handle, @r, False);
  Invalidate;
end;

procedure TNxTimePopup.SelectMinute(X, Y: Integer);
var
    l, t, col, row: Integer;
    r: TRect;
begin
    col := 3;
  row := (y - 21) div 15 + 1;

  if (row-1)*5 = FMinutes then
    Exit;

  FMinutes := (row-1)*5;

  if (FMinutes < 60) then SelectedTime := EncodeTime(FHours, FMinutes, 0, 0);

  l := 6 + col * 30;
  t := 22 + row * 15;
  r := Rect(l, t, l + 28, t + 13);
  InvalidateRect(Handle, @FOldMinuteRect, False);
  InvalidateRect(Handle, @r, False);
  Invalidate;
end;

procedure TNxTimePopup.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if PtInRect(Rect(5, 21, (ClientWidth div 3 * 2) + 5, ClientHeight - 5), Point(X, Y)) then
  begin
    FDown := True;
    SelectHour(X, Y);
  end;

  if PtInRect(Rect((ClientWidth div 3 * 2) + 5, 21, ClientWidth - 5, ClientHeight - 5), Point(X, Y)) then
  begin
    FDown := True;
    SelectMinute(X, Y);
  end;
end;

procedure TNxTimePopup.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (FDown) then
  begin
    if PtInRect(Rect(5, 21, (ClientWidth div 3 * 2) + 5, ClientHeight - 5), Point(X, Y)) then
    begin
      FDown := True;
      SelectHour(X, Y);
    end;

    if PtInRect(Rect((ClientWidth div 3 * 2) + 5, 21, ClientWidth - 5, ClientHeight - 5), Point(X, Y)) then
    begin
      FDown := True;
      SelectMinute(X, Y);
    end;
  end;
end;

procedure TNxTimePopup.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  //if not PtInRect(Rect(0, 0, ClientWidth, ClientHeight), Point(X, Y)) then inherited;
  FDown := False;
end;

procedure TNxTimePopup.Paint;
var
    X, Y, p, dx: Integer;
  r1, r2, r3: TRect;
  h, m, s, ms : Word;
begin
  inherited;
  X := 5;
  Y := 22;
  p := 0;

  dx := (ClientWidth - 10) div 3;

  with Canvas do
  begin
    Font.Assign(Self.Font);
    Font.Color := clWindowText;
    Brush.Color := Self.Color;
    Pen.Color := clGrayText;
    Rectangle(0, 0, ClientWidth, ClientHeight);
    PaintTitle(GetTitleRect);

    Font.Color := clWindowText;
    DrawTextRect(Canvas, GetHourRect, taCenter, FHoursString);
    Font.Color := clWindowText;
    DrawTextRect(Canvas, GetMinuteRect, taCenter, FMinutesString);

    Pen.Color := clGrayText;
    MoveTo(5, 21);
    LineTo(ClientWidth - 5, 21);
    Pen.Color := clGrayText;
    MoveTo(5 + ((ClientWidth - 10) div 3 * 2), 21);
    LineTo(5 + ((ClientWidth - 10) div 3 * 2), 202);
    Pen.Color := clGrayText;
    MoveTo(5 + ((ClientWidth - 10) div 3 * 1), 21);
    LineTo(5 + ((ClientWidth - 10) div 3 * 1), 202);
    Pen.Color := clGrayText;
    MoveTo(5, 202);
    LineTo(ClientWidth - 5, 202);

    DecodeTime(SelectedTime, h, m, s, ms);

    while p < 12 do
    begin
        r1 := Rect(X + 3, Y, X + dx - 3, Y + 15);
      r2 := Rect(X + dx + 3, Y, X + dx + dx - 3, Y + 15);
      r3 := Rect(X + 2*dx + 3, Y, X + 2*dx + dx - 3, Y + 15);

      Font.Color := clWindowText;

      if p = h then
      begin
          if IsThemed then
          Brush.Color := RGB(251, 230, 148)
        else
          Brush.Color := clSilver;

        FillRect(Rect(X + 1, Y + 1, X + dx - 1, Y + 15 - 1));
        FOldHourRect := Rect(X + 1, Y + 1, X + dx - 1, Y + 15 - 1);
      end
      else if (p+12) = h then
      begin
          if IsThemed then
          Brush.Color := RGB(251, 230, 148)
        else
          Brush.Color := clSilver;

        FillRect(Rect(X + dx + 2, Y + 1, X + dx + dx - 1, Y + 15 - 1));
        FOldHourRect := Rect(X + dx + 2, Y + 1, X + dx + dx - 1, Y + 15 - 1);
      end;

      PaintItem(p, r1);
      PaintItem(p + 12, r2);

      Font.Color := clGrayText;

      if p = (m div 5) then
      begin
          if IsThemed then
          Brush.Color := RGB(251, 230, 148)
        else
          Brush.Color := clSilver;
        FillRect(Rect(X + 2*dx + 2, Y + 1, X + 2*dx + dx - 1, Y + 15 - 1));
        FOldMinuteRect := Rect(X + 2*dx + 2, Y + 1, X + 2*dx + dx - 1, Y + 15 - 1);
      end;

      PaintItem(p*5, r3);

      Inc(Y, 15);
      Inc(p, 1)
    end;
  end;
end;

procedure TNxTimePopup.PaintItem(Day: Integer; DayRect: TRect);
var
  StringText: string;
begin
  with Canvas.Brush do
  begin
    Style := bsClear;
    StringText := IntToStr(Day);
    TGraphicsProvider.DrawWrapedTextRect(Canvas, DayRect, taRightJustify, taVerticalCenter,
      False, StringText, bdLeftToRight);
    Style := bsSolid;
  end;
end;

procedure TNxTimePopup.PaintTitle(TitleRect: TRect);
begin
  with Canvas do
  begin
    if not IsThemed    then Brush.Color := clBtnFace
      else Brush.Color := TGraphicsProvider.BlendColor(clGradientInactiveCaption, clWindow, 249);

    FillRect(TitleRect);
    Font.Color := clWindowText;
    DrawTextRect(Canvas, GetHourRect, taCenter, FHoursString);
      DrawTextRect(Canvas, GetMinuteRect, taCenter, FMinutesString);
  end;
end;

procedure TNxTimePopup.SetHourString(Value: string);
begin
  FHoursString := Value;
end;

procedure TNxTimePopup.SetMinuteString(Value: string);
begin
  FMinutesString := Value;
end;

procedure TNxTimePopup.SetHour(const Value: Word);
begin
  FHours := Value;
end;

procedure TNxTimePopup.SetMinute(const Value: Word);
begin
  FMinutes := Value;
end;

procedure TNxTimePopup.AcquireValue(X, Y: Integer);
begin
  inherited;
  DoInput(TimeToStr(SelectedTime));
end;

end.










