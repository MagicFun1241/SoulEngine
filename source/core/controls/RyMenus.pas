unit RyMenus;

interface

uses Windows, Classes, Graphics, ImgList, Controls, Commctrl,
  Menus, Forms;

type
  TRyMenu = class(TComponent)
  private
    FFont: TFont;
    FGutterColor: TColor;
    FMenuColor: TColor;
    FSelectedColor: TColor;
    FSelLightColor: TColor;
    FMinWidth: Integer;
    FMinHeight: Integer;
    procedure SetFont(const Value: TFont);
    procedure SetSelectedColor(const Value: TColor);
    procedure SetMinHeight(const Value: Integer);
    procedure InternalInitItem(Item: TMenuItem);
    procedure InternalInitItems(Item: TMenuItem);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(Menu: TMenu); overload;
    procedure Add(Item: TMenuItem); overload;
    procedure MeasureItem(Sender: TObject; ACanvas: TCanvas;
              var Width, Height: Integer);
    procedure AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
              ARect: TRect; State: TOwnerDrawState);
  public
    property  MenuColor: TColor read FMenuColor write FMenuColor;
    property  GutterColor: TColor read FGutterColor write FGutterColor;
    property  SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property  Font: TFont read FFont write SetFont; {������ �������� ���� � ����}
    property  MinHeight: Integer read FMinHeight write SetMinHeight;
    property  MinWidth: Integer read FMinWidth write FMinWidth;
  end;

procedure DrawItem(Sender: TObject; ACanvas: TCanvas;
          ARect: TRect; State: TOwnerDrawState; TopLevel, IsLine: Boolean;
          ImageList: TCustomImageList; ImageIndex: Integer; AFont: TFont;
          const Caption, CaptionEx: String; GutterWidth: Integer;
          SelectedColor, GutterColor, MenuColor, SelLightColor: TColor);

procedure MeasureItem(Sender: TObject; Canvas: TCanvas;
          ImageList: TCustomImageList; Font: TFont;
          IsLine: Boolean; const Caption, CaptionEx: String;
          MinHeight, MinWidth: Integer; var Width, Height: Integer);

var
  RyMenu: TRyMenu; {��� ����� ��� �� �������� - ��� ������� ���� ���������
  ���� TRyMenu. �� ���������������� ��� � ������ ��� � ����������� �����.}

implementation

uses Utils;

var
  BmpCheck: {array[Boolean] of} TBitmap; {��� bmp 12x12 ��� �������� ������� ����}

{ TRyMenuItem }

constructor TRyMenu.Create(AOwner: TComponent);
begin
  FGutterColor := clBtnFace; {����� �������}
  FMenuColor := GetLightColor(clBtnFace, 85);
  FSelectedColor := GetLightColor(clHighlight, 65);{���������� ����� ����}
  FSelLightColor := GetLightColor(clHighlight, 75);
  FMinWidth := 0;
  FMinHeight:= 20;
  FFont := TFont.Create;
  Font := Screen.MenuFont;{�������� ���� ������������ ����}
end;

destructor TRyMenu.Destroy;
begin
  FFont.Free;
  inherited;
end;

{
  ����� ������ �� ����������� ��� ����� ����, �������� ��� ���������,
  ������� � �������� ���������  ���� ���� ����  ���� ����� ���� ����
}

procedure TRyMenu.InternalInitItem(Item : TMenuItem);
begin
  {if not (Item.GetParentComponent is TMainMenu) then}
  Item.OnMeasureItem := Self.MeasureItem;
  Item.OnAdvancedDrawItem := Self.AdvancedDrawItem;
end;

procedure TRyMenu.InternalInitItems(Item : TMenuItem);
{����� �� ���� �������, ��� ������ ���������� � ���������}
var
  I: Integer;
begin
  for I := 0 to Item.Count - 1 do
  begin
    InternalInitItem(Item[I]);
    if Item[I].Count > 0 then InternalInitItems(Item[I]);
  end;
end;

procedure TRyMenu.Add(Menu: TMenu);
begin
  if Assigned(Menu) then
  begin
    InternalInitItems(Menu.Items);
    Menu.OwnerDraw := True; {����� ������� ����� �������� �� ���� ������ ��������
    � ����� �����������. �������� �������� � History �� 14.01.2002.}
  end;
end;

procedure TRyMenu.Add(Item: TMenuItem);
begin
  if Assigned(Item) then
  begin
    InternalInitItem(Item);
    InternalInitItems(Item);
  end
end;

procedure DrawItem(Sender: TObject; ACanvas: TCanvas;
          ARect: TRect; State: TOwnerDrawState; TopLevel, IsLine: Boolean;
          ImageList: TCustomImageList; ImageIndex: Integer; AFont: TFont;
          const Caption, CaptionEx: String; GutterWidth: Integer;
          SelectedColor, GutterColor, MenuColor, SelLightColor: TColor);
const
  {��������� �����}
  _Flags: LongInt = DT_NOCLIP or DT_VCENTER or DT_END_ELLIPSIS or DT_SINGLELINE;
  _FlagsTopLevel: array[Boolean] of Longint = (DT_LEFT, DT_CENTER);
  _FlagsShortCut: {array[Boolean] of} Longint = (DT_RIGHT);
  _RectEl: array[Boolean] of Byte = (0, 6);{������������ �������������}
begin
  with ACanvas do
  begin
    Font := AFont; {��������� ����� ����� ������ ������������}

    Pen.Color := clBlack;
    if (odSelected in State) then {���� ����� ���� �������}
    begin
      Brush.Color := SelectedColor;
      Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    end else
    if TopLevel then {���� ��� ������� ��������� ����}
    begin
      if (odHotLight in State) then {���� ���� ��� ������� ����}
      begin
        Pen.Color := clBtnShadow;
        Brush.Color := BtnHighLight;
        Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
      end else
      begin
        Brush.Color := clBtnFace;
        FillRect(ARect);
      end
    end else
      begin {����� �� �������������� ����� ����}
        Brush.Color := GutterColor; {�������}
        FillRect(Rect(ARect.Left, ARect.Top, GutterWidth, ARect.Bottom));
        Brush.Color := MenuColor;
        FillRect(Rect(GutterWidth, ARect.Top, ARect.Right, ARect.Bottom));
      end;

    if odChecked in State then
    begin {������������ �������� ����� ����}
      Pen.Color := clBtnShadow;
      Brush.Color := SelLightColor;
      Rectangle((ARect.Left + 1), (ARect.Top + 1),
        (GutterWidth - 1 - 1), (ARect.Bottom - 1));
      //if Assigned(ImageList) and (ImageIndex > -1) then
         {���� ������� �������� �� ������ ��������� ������ ���}
      //  RoundRect(ARect.Left + 1, ARect.Top + 1, Gutter - 1 - 1,
      //    ARect.Bottom - 1, _RectEl[{RadioItem}False], _RectEl[{RadioItem}False])
      //else {������ ������ �������}
      //begin
      //  Rectangle((ARect.Left + 1), (ARect.Top + 1),
      //    (Gutter - 1 - 1), (ARect.Bottom - 1));
      //  Draw((ARect.Left + 1 + Gutter - 1 - 1 - BmpCheck[{RadioItem}False].Width) div 2,
      //    (ARect.Top + ARect.Bottom - BmpCheck[{RadioItem}False].Height) div 2,
      //    BmpCheck[{RadioItem}False]);
      //end
    end;

    if Assigned(ImageList) and ((ImageIndex > -1) and (not TopLevel)) then
      if not (odDisabled in State) then
        ImageList.Draw(ACanvas,
          (ARect.Left + GutterWidth - 1 - ImageList.Width) div 2,
          (ARect.Top + ARect.Bottom - ImageList.Height) div 2,
          ImageIndex, True) {������ ������� ��������}
      else begin {������ �������� ��������}
        GetBmpFromImgList(FMonoBitmap, ImageList, ImageIndex);
        DoDrawMonoBmp(ACanvas, clBtnShadow,
          (ARect.Left + GutterWidth - 1 - ImageList.Width) div 2,
          (ARect.Top + ARect.Bottom - ImageList.Height) div 2);
      end
    else if not TopLevel and ((Sender as TMenuItem).Bitmap<>nil)
        and not TMenuItem(Sender).Bitmap.Empty then begin
      if not (odDisabled in State) then
        Draw((ARect.Left + GutterWidth - 1 - TMenuItem(Sender).Bitmap.Width) div 2,
          (ARect.Top + ARect.Bottom - TMenuItem(Sender).Bitmap.Height) div 2,
          TMenuItem(Sender).Bitmap)
      else //��������
        DrawState(ACanvas.Handle, ACanvas.Brush.Handle, nil,
          TMenuItem(Sender).Bitmap.Handle, 0,
          (ARect.Left + GutterWidth - 1 - TMenuItem(Sender).Bitmap.Width) div 2,
          (ARect.Top + ARect.Bottom - TMenuItem(Sender).Bitmap.Height) div 2,
          0, 0, DST_BITMAP or DSS_DISABLED);
    end
    else
    if odChecked in State then
      Draw((ARect.Left + GutterWidth - 1 - BmpCheck{[RadioItemFalse]}.Width) div 2,
          (ARect.Top + ARect.Bottom - BmpCheck{[RadioItemFalse]}.Height) div 2,
          BmpCheck{[RadioItemFalse]});

    with Font do
    begin
      if (odDefault in State) then Style := [fsBold];
      if (odDisabled in State) then Color := clGray
      else Color := clBlack;
    end;

    Brush.Style := bsClear;
    if TopLevel then {�����}
    else Inc(ARect.Left, GutterWidth + 5); {������ ��� ������}

    if IsLine then {���� �����������}
    begin
      Pen.Color := clBtnShadow;
      MoveTo(ARect.Left, ARect.Top + (ARect.Bottom - ARect.Top) div 2);
      LineTo(ARect.Right, ARect.Top + (ARect.Bottom - ARect.Top) div 2);
    end else
    begin {����� ����}
      DrawText(Handle, PChar(Caption), Length(Caption), ARect,
        _Flags or _FlagsTopLevel[TopLevel]);
      if CaptionEx <> '' then {������������}
      begin
        Dec(ARect.Right, 5);
        DrawText(Handle, PChar(CaptionEx), Length(CaptionEx),
          ARect, _Flags or _FlagsShortCut);
      end
    end
  end
end;

procedure MeasureItem(Sender: TObject; Canvas: TCanvas;
          ImageList: TCustomImageList; Font: TFont;
          IsLine: Boolean; const Caption, CaptionEx: String;
          MinHeight, MinWidth: Integer; var Width, Height: Integer);
begin
  Canvas.Font := Font; {��������� ����� �� ��� ����}
  if Assigned(ImageList) then
  begin
    if IsLine then
      if Max(MinHeight, ImageList.Height) > 20 then {��� ������� 20 ����� ������� ���������}
         Height := 11 else Height := 5
    else
      with Canvas do
      begin
        Height := Max(MinHeight,
          Max(Canvas.TextHeight('W'), ImageList.Height) + 4);

        Width := ImageList.Width;
        if Width < Height then Width := Height else Width := Width + 5;
        Width := Max(MinWidth,
          Width + TextWidth(Caption + CaptionEx) + 15);
      end
  end else
    with Canvas do
    begin
      Height := Max(TextHeight('W') + 4, MinHeight);

      Width := Max(MinWidth,
        Height + TextWidth(Caption + CaptionEx) + 15);

      if IsLine then
        if Height > 20 then {��� ������� 20 ����� ������� ���������}
           Height := 11 else Height := 5;
    end
end;

{���������� ���������-c}
procedure TRyMenu.AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
          ARect: TRect; State: TOwnerDrawState);
var
  ImageList: TCustomImageList;

  function GetGutterWidth(IsLine: Boolean): Integer;
  begin
    with ACanvas do
    begin
      Font := FFont; {��������� ����� ����� ������ ������������}
      if Assigned(ImageList) then
      begin
        Result := Max(ImageList.Width + 4,
            ARect.Bottom - ARect.Top); {������ ����� �� �������� + �������� + ���� �����}
        if IsLine then
          Result := Max(Result, TextHeight('W') + 4);
      end else
      if IsLine then
        Result := TextHeight('W') + 4
      else
        Result := ARect.Bottom - ARect.Top; {������ = ������ + 2 + 2 �����}
    end;
    Result := Max(Result, MinHeight) + 1;
  end;

begin
  with TMenuItem(Sender) do
  begin
    ImageList := GetImageList;
    DrawItem(Sender, ACanvas, ARect, State,
      GetParentComponent is TMainMenu, IsLine,
      ImageList, ImageIndex, FFont, Caption, ShortCutToText(ShortCut),
      GetGutterWidth(IsLine), SelectedColor, GutterColor, MenuColor, FSelLightColor);
  end
end;

{������� ����}
procedure TRyMenu.MeasureItem(Sender: TObject; ACanvas: TCanvas;
          var Width, Height: Integer);
begin
  with TMenuItem(Sender) do
    if GetParentComponent is TMainMenu then
    begin
      ACanvas.Font := FFont;
      Width := ACanvas.TextWidth(Caption);
    end else
      RyMenus.MeasureItem(Sender, ACanvas, GetImageList,
        FFont, IsLine, Caption, ShortCutToText(ShortCut),
        MinHeight, MinWidth, Width, Height)
end;

procedure TRyMenu.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TRyMenu.SetSelectedColor(const Value: TColor);
begin
  FSelectedColor := Value;
  FSelLightColor := GetLightColor(Value, 75);
end;

procedure InitBmp(Bmp: TBitmap{; Radio: Boolean});
const
  StrBmp: String =
    '000000000000' + '-' +
    '000000000000' + '-' +
    '000000000000' + '-' +
    '000000001000' + '-' +
    '000000011000' + '-' +
    '001000111000' + '-' +
    '001101110000' + '-' +
    '001111100000' + '-' +
    '000111000000' + '-' +
    '000010000000' + '-' +
    '000000000000' + '-' +
    '000000000000';

  {pr : array[0..17] of array[0..1] of Byte = (
    (2, 6), (3, 7), (4, 8), (5, 9), (6, 8), (7, 7),
    (3, 6), (4, 7), (5, 8), (6, 7), (7, 6), (8, 5), (9, 4), (10, 3), (11, 2),
    (3, 5), (4, 6), (5, 7)
  );
  pc : array[0..23] of array[0..1] of Byte = (
    (3, 5), (3, 6), (4, 7), (5, 8), (6, 8), (7, 7), (8, 6), (8, 5),
    (7, 4), (6, 3), (5, 3), (4, 4), (4, 5), (4, 6), (5, 7), (6, 7),
    (7, 6), (7, 5), (6, 4), (5, 4), (5, 5), (5, 6), (6, 6), (6, 5)
  );}
var
  I, X, Y: Byte;
  Len: Integer;
begin
  with Bmp, Canvas do
  begin
    Width := 12;
    Height := 12;
    Monochrome := True;
    Transparent := True;
    {Pen.Color := clBlack;}
    Brush.Color := clWhite;
    FillRect(Rect(0, 0, Width, Height));
    {if Radio then
      for I := Low(pc) to High(pc) do
        Pixels[pc[I, 0], pc[I, 1]] := clBlack
    else}
    X := 0; Y := 0;
    Len := Length(StrBmp);
    for I := 1 to Len do
      if StrBmp[I] = '-' then
      begin
        X := 0;
        Inc(Y);
      end else
      begin
        if StrBmp[I] = '1' then Pixels[X, Y] := clBlack;
        Inc(X);
      end
  end
end;

procedure TRyMenu.SetMinHeight(const Value: Integer);
begin
  FMinHeight := Max(20, Value);
end;

initialization
  BmpCheck{[False]}:= TBitmap.Create;
  {BmpCheck[True]:= TBitmap.Create;}
  InitBmp(BmpCheck{[False], False});
  {InitBmp(BmpCheck[True], True);}
  RyMenu := TRyMenu.Create(Application);

finalization
  BmpCheck{[False]}.Free;
  {BmpCheck[True].Free;}

end.
