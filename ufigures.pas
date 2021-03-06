unit UFigures;

{$mode objfpc}{$H+}

interface

 uses
   Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
   Dialogs, Menus,ExtCtrls, StdCtrls, Grids, LCLIntf,LCLType,
   Buttons, GraphMath, Math, Spin, FPCanvas, TypInfo, LCL, Windows, uscale;

 type
  TStringArray = array of String;

  TFigure = class
    Points: array of TDoublePoint;
    Selected: boolean;
    Region: HRGN;
    procedure Draw(Canvas: TCanvas); virtual; abstract;
    procedure CreateRegion; virtual; abstract;
    procedure DrawOutline(Point1,Point2: TDoublePoint; Canvas: TCanvas); virtual;
    function Save:TStringArray;virtual;
    procedure Load;virtual;
    function Copy: TFigure;virtual;
  end;

  TLines = class(TFigure)
    PenStyle: TPenStyle;
    PenColor: TColor;
    Width: integer;
    function Save: TStringArray; override;
    procedure Load; override;
    function Copy: TFigure; override;
  end;

  TFigures = class(TLines)
    BrushStyle: TBrushStyle;
    BrushColor: TColor;
    function Save: TStringArray; override;
    procedure Load; override;
    function Copy: TFigure; override;
  end;

  TPolyline = class(TLines)
    procedure Draw(Canvas: TCanvas); override;
    procedure CreateRegion; override;
  end;

  TRectangle = class(TFigures)
    procedure Draw(Canvas: TCanvas); override;
    procedure CreateRegion; override;
  end;

  TRoundRectangle = class(TFigures)
    RWidth,RHeight: integer;
    procedure Draw(Canvas: TCanvas); override;
    procedure CreateRegion; override;
    function Save: TStringArray; override;
    procedure Load; override;
    function Copy: TFigure; override;
  end;

  TEllipse = class(TFigures)
    procedure Draw(Canvas: TCanvas); override;
    procedure CreateRegion; override;
  end;

  TLine = class(TLines)
    procedure Draw(Canvas: TCanvas); override;
    procedure CreateRegion; override;
  end;

  TFrame = class(TLines)
    procedure Draw(Canvas: TCanvas); override;
  end;

  TRegularPolygon = class(TFigures)
    ConersCount: Integer;
    procedure Draw(Canvas: TCanvas); override;
    function Rotate(P1, P2: TDoublePoint; angle: double): TDoublePoint;
    procedure CreateRegion; override;
    function Save: TStringArray; override;
    procedure Load; override;
    function Copy: TFigure; override;
  end;

  procedure LineRegion(p1,p2:TPoint;var tempPoints: array of TPoint;Width:integer);
  function CasePenStyle(Index: integer):TPenStyle;
  function CaseBrushStyle(Index: integer):TBrushStyle;
  function CasePenStyleIndex(Style: TPenStyle): integer;
  function CaseBrushStyleIndex(BrushStyle: TBrushStyle): integer;
  procedure SaveInBuffer;
  procedure LoadFromBuffer;

var
  Figures: array of TFigure;
  RectR: TPoint;
  BufferPointer: integer;
  BufferBegin: integer;
  Buffer: array[0..99] of array of TFigure;

implementation

procedure TFigure.DrawOutline(Point1,Point2: TDoublePoint; Canvas: TCanvas);
var
  a:TDoublepoint;
begin
  if (Point1.X>Point2.X) then
    begin
      a.X:=Point1.X;
      Point1.X:=Point2.X;
      Point2.X:=a.X;
    end;
  if (Point1.Y>Point2.Y) then
    begin
      a.Y:=Point1.Y;
      Point1.Y:=Point2.Y;
      Point2.Y:=a.Y;
    end;
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psDash;
  Canvas.Frame  (WorldToScreen(Point1).x-5,WorldToScreen(Point1).y-5,
                 WorldToScreen(Point2).x+5,WorldToScreen(Point2).y+5);
end;

procedure TPolyline.Draw (Canvas: TCanvas);
var
  i: Integer;
  Max,Min:TDoublePoint;
begin
  Canvas.Pen.Style := PenStyle;
  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width := Width;
  Canvas.MoveTo(WorldToScreen(Points[0]));
  for i := 1 to High(Points) do
  Canvas.LineTo(WorldToScreen(Points[i]));
   if (Selected = true) then
  begin
    DeleteObject(Region);
    Min:=DoublePoint(MaxFloat,MaxFloat);
    Max := DoublePoint(MinFloat,MinFloat);
    for i := 1 to high(Points) do
      begin
        if Points[i].X > Max.X then Max.X:=Points[i].X;
        if Points[i].Y > Max.Y then Max.Y:=Points[i].Y;
        if Points[i].X < Min.X then Min.X:=Points[i].X;
        if Points[i].Y < Min.Y then Min.Y:=Points[i].Y;
      end;
      DrawOutline(Min,Max,Canvas);
  end;
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Style := PenStyle;
  Canvas.Brush.Style := BrushStyle;
  Canvas.Brush.Color := BrushColor;
  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width := Width;
  Canvas.Rectangle(WorldToScreen(DoubleRect(Points[0], Points[1])));
  if (Selected = true) then
  begin
    DeleteObject(Region);
    DrawOutline(Points[0],Points[1],Canvas);
  end;
end;

procedure TRoundRectangle.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Style := PenStyle;
  Canvas.Brush.Style := BrushStyle;
  Canvas.Brush.Color := BrushColor;
  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width := Width;
  Canvas.RoundRect(WorldToScreen(DoubleRect(Points[0], Points[1])), RWidth, RHeight);
   if (Selected = true) then
  begin
    DeleteObject(Region);
    DrawOutline(Points[0],Points[1],Canvas);
  end;
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Style := PenStyle;
  Canvas.Brush.Style := BrushStyle;
  Canvas.Brush.Color := BrushColor;
  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width := Width;
  Canvas.Ellipse(WorldToScreen(DoubleRect(Points[0], Points[1])));
   if (Selected = true) then
  begin
    DeleteObject(Region);
    DrawOutline(Points[0],Points[1],Canvas);
  end;
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Style:=PenStyle;
  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width := Width;
  Canvas.Line(WorldToScreen(DoubleRect(Points[0], Points[1])));
  if (Selected = true) then
  begin
    DeleteObject(Region);
    DrawOutline(Points[0],Points[1],Canvas);
  end;
end;

procedure TFrame.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Style := PenStyle;
  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width := Width;
  Canvas.Frame(WorldToScreen(DoubleRect(Points[0], Points[1])));
end;

procedure TRegularPolygon.Draw(Canvas: TCanvas);
var
  i, r, k: Integer;
  FAngle: Double;
  Max,Min:TPoint;
  APolygon: array of TDoublePoint;
  APolygonScreen: array of TPoint;
begin
  k := 0;
  Canvas.Pen.Style := PenStyle;
  Canvas.Pen.Color := PenColor;
  Canvas.Brush.Style := BrushStyle;
  Canvas.Brush.Color := BrushColor;
  Canvas.Pen.Width := Width;
  k := 360 div ConersCount;
  r := round(sqrt(sqr(abs(Points[0].X - Points[1].X))
    + sqr(abs(Points[0].Y - Points[1].Y))));
  FAngle := arctan2(Points[0].Y - Points[1].Y, Points[0].X - Points[1].X);
  SetLength(APolygon, ConersCount);
  SetLength(APolygonScreen, ConersCount);
  for i := 0 to ConersCount - 1 do
    begin
      APolygon[i].X := Points[0].X + cos(k * i / 180 * pi) * r;
      APolygon[i].Y := Points[0].Y + sin(k * i / 180 * pi) * r;
      APolygonScreen[i] := WorldToScreen(Rotate(Points[0], APolygon[i], FAngle))
    end;
  Canvas.Polygon(APolygonScreen);
  if (Selected = true) then
  begin
    Min.X := MaxInt;
    Min.Y := MaxInt;
    Max.X := Low(integer);
    Max.Y := Low(integer);
    for i := 0 to ConersCount - 1 do
      begin
        if APolygonScreen[i].X > Max.X then Max.X:=APolygonScreen[i].X;
        if APolygonScreen[i].Y > Max.Y then Max.Y:=APolygonScreen[i].Y;
        if APolygonScreen[i].X < Min.X then Min.X:=APolygonScreen[i].X;
        if APolygonScreen[i].Y < Min.Y then Min.Y:=APolygonScreen[i].Y;
      end;
      DrawOutline(Scrn2Wrld(Min), Scrn2Wrld(Max), Canvas);
  end;
end;

function TRegularPolygon.Rotate(P1,P2: TDoublePoint; angle: double): TDoublePoint;
begin
  Result.x := P1.x + (P2.x - P1.x) * cos(angle)
    - (P2.Y - P1.Y) * sin(angle);
  Result.Y := P1.y + (P2.x - P1.x) * sin(angle)
    + (P2.Y - P1.Y) * cos(angle);
end;

procedure TRectangle.CreateRegion;
var
  RegionRect: TRect;
begin
  RegionRect.TopLeft := WorldToScreen(Points[0]);
  RegionRect.BottomRight := WorldToScreen(Points[1]);
  Region := CreateRectRgn (RegionRect.Left,RegionRect.Top,
    RegionRect.Right,RegionRect.Bottom);
end;

procedure TEllipse.CreateRegion;
var
  RegionRect: TRect;
begin
  RegionRect.TopLeft := WorldToScreen(Points[0]);
  RegionRect.BottomRight := WorldToScreen(Points[1]);
  Region := CreateEllipticRgn (RegionRect.Left,RegionRect.Top,RegionRect.Right,RegionRect.Bottom);
end;

procedure TRoundRectangle.CreateRegion;
var
  RegionRect: TRect;
begin
  RegionRect.TopLeft := WorldToScreen(Points[0]);
  RegionRect.BottomRight := WorldToScreen(Points[1]);
  Region := CreateRoundRectRgn (RegionRect.Left,RegionRect.Top,RegionRect.Right,
    RegionRect.Bottom,RectR.x,RectR.y);
end;

procedure TLine.CreateRegion;
var
  RegionPoints: array[0..3] of TPoint;
  p1,p2: TPoint;
begin
  p1 := WorldToScreen(Points[0]);
  p2 := WorldToScreen(Points[1]);
  LineRegion(p1,p2,RegionPoints,Width);
  Region := CreatePolygonRgn(RegionPoints,3,2);
end;

procedure TPolyline.CreateRegion;
var
  RegionPoints: array[0..3] of TPoint;
  p1,p2: TPoint;
  curRgn: HRGN;
  i: integer;
begin
  for i := 0 to high(Points)-1 do
  begin
    p1 := WorldToScreen(Points[i]);
    p2 := WorldToScreen(Points[i+1]);
    LineRegion(p1,p2,RegionPoints,Width);
    if (i=low(Points)) then Region := CreatePolygonRgn (RegionPoints,3,2);
    curRgn := CreatePolygonRgn (RegionPoints,3,2);
    CombineRgn (Region,Region,curRgn,RGN_OR);
    DeleteObject(curRgn);
  end;
end;

procedure LineRegion(p1,p2:TPoint;var tempPoints: array of TPoint;Width:integer);
begin
      if (abs(p2.x-p1.x)>45) then
    begin
      tempPoints[0].x := p1.x-Width div 2;
      tempPoints[0].y := p1.y-5-Width;
      tempPoints[1].x := p2.x+Width div 2;
      tempPoints[1].y := p2.y-5-Width;
      tempPoints[2].x := p2.x+Width div 2;
      tempPoints[2].y := p2.y+5+Width;
      tempPoints[3].x := p1.x-Width div 2;
      tempPoints[3].y := p1.y+5+Width;
    end else
    begin
      tempPoints[0].x := p1.x-5-Width;
      tempPoints[0].y := p1.y-Width div 2;
      tempPoints[1].x := p2.x-5-Width;
      tempPoints[1].y := p2.y+Width div 2;
      tempPoints[2].x := p2.x+5+Width;
      tempPoints[2].y := p2.y+Width div 2;
      tempPoints[3].x := p1.x+5+Width;
      tempPoints[3].y := p1.y-Width div 2;
    end;
end;

procedure TRegularPolygon.CreateRegion;
var
  i, r, k: Integer;
  FAngle: Double;
  Max,Min:TDoublePoint;
  APolygon: array of TDoublePoint;
  APolygonScreen: array of TPoint;
begin
  k := 0;
  k := 360 div ConersCount;
  r := round(sqrt(sqr(abs(Points[0].X - Points[1].X))
    + sqr(abs(Points[0].Y - Points[1].Y))));
  FAngle := arctan2(Points[0].Y - Points[1].Y, Points[0].X - Points[1].X);
  SetLength(APolygon, ConersCount);
  SetLength(APolygonScreen, ConersCount);
  for i := 0 to ConersCount - 1 do
    begin
      APolygon[i].X := Points[0].X + cos(k * i / 180 * pi) * r;
      APolygon[i].Y := Points[0].Y + sin(k * i / 180 * pi) * r;
      APolygonScreen[i] := WorldToScreen(Rotate(Points[0], APolygon[i], FAngle))
    end;
  Region := CreatePolygonRgn (APolygonScreen[0],length(APolygonScreen),winding);
end;

function CasePenStyle(Index: integer): TPenStyle;
begin
  case Index of
    0:Result := psSolid;
    1:Result := psDash;
    2:Result := psDot;
    3:Result := psDashDot;
    4:Result := psDashDotDot;
  end;
end;

function CasePenStyleIndex(Style: TPenStyle): integer;
begin
    case Style of
    psSolid:Result := 0;
    psDash:Result := 1;
    psDot:Result := 2;
    psDashDot:Result := 3;
    psDashDotDot:Result := 4;
  end;
end;

function CaseBrushStyle(Index: integer): TBrushStyle;
begin
  case Index of
    0:Result := bsSolid;
    1:Result := bsBDiagonal;
    2:Result := bsDiagCross;
    3:Result := bsVertical;
    4:Result := bsCross;
    5:Result := bsFDiagonal;
    6:Result := bsHorizontal;
  end;
end;

function CaseBrushStyleIndex(BrushStyle: TBrushStyle): integer;
begin
  case BrushStyle of
    bsSolid: Result := 0;
    bsBDiagonal:Result := 1;
    bsDiagCross:Result := 2;
    bsVertical:Result := 3;
    bsCross:Result := 4;
    bsFDiagonal:Result := 5;
    bsHorizontal:Result := 6;
  end;
end;

function TFigure.Copy: TFigure;
var
  i: integer;
begin
  case Self.ClassName of
    'TPolyline': Result := TPolyline.Create;
    'TLine': Result := TLine.Create;
    'TRectangle': Result := TRectangle.Create;
    'TRoundRectangle': Result := TRoundRectangle.Create;
    'TEllipse': Result := TEllipse.Create;
    'TRegularPolygon': Result := TRegularPolygon.Create;
  end;
  SetLength(Result.Points, Length(Self.Points));
  for i:=0 to High(Self.Points) do
  Result.Points[i] := Self.Points[i];
end;

function TLines.Copy: TFigure;
begin
  Result := Inherited;
  (Result as TLines).PenColor := Self.PenColor;
  (Result as TLines).PenStyle := Self.PenStyle;
end;

function TFigures.Copy: TFigure;
begin
  Result := Inherited;
  (Result as TFigures).BrushColor := Self.BrushColor;
  (Result as TFigures).BrushStyle := Self.BrushStyle;
end;

function TRoundRectangle.Copy: TFigure;
begin
  Result := Inherited;
  (Result as TRoundRectangle).RHeight := Self.RHeight;
  (Result as TRoundRectangle).RWidth := Self.RWidth;
end;

function TRegularPolygon.Copy: TFigure;
begin
  Result := Inherited;
  (Result as TRegularPolygon).ConersCount := Self.ConersCount;
end;

function TFigure.Save:TStringArray;
var
  i: integer;
begin
  SetLength(Result, 2);
  Result[0] := ClassName;
  Result[1] := IntToStr(Length(Points)) + ' ';
  for i:=0 to High(Points) do
    begin
    Result[1] += FloatToStr(Points[i].X) + ' ';
    Result[1] += FloatToStr(Points[i].Y) + ' ';
    end;
end;

function TLines.Save:TStringArray;
begin
  Result := Inherited;
  SetLength(Result, Length(Result) + 3);
  Result[High(Result)-2] := IntToStr(Width);
  Result[High(Result)-1] := ColorToString(PenColor);
  Result[High(Result)] := IntToStr(CasePenStyleIndex(PenStyle));
end;

function TFigures.Save:TStringArray;
begin
  Result := Inherited;
  SetLength(Result, Length(Result) + 2);
  Result[High(Result)-1] := ColorToString(BrushColor);
  Result[High(Result)] := IntToStr(CaseBrushStyleIndex(BrushStyle));
end;

function TRoundRectangle.Save:TStringArray;
begin
  Result := Inherited;
  SetLength(Result, Length(Result) + 2);
  Result[High(Result)] := IntToStr(RWidth);
  Result[High(Result)] += ' ' + IntToStr(RHeight);
end;

function TRegularPolygon.Save:TStringArray;
begin
  Result := Inherited;
  SetLength(Result, Length(Result) + 1);
  Result[High(Result)] := IntToStr(ConersCount);
end;

procedure TFigure.Load;
var
  i, n: integer;
begin
  read(n);
  SetLength(Points, n);
  for i:=0 to n-1 do
    begin
      read(Points[i].X);
      read(Points[i].Y);
      FindMinMax(Points[i]);
    end;
  readln();
end;

procedure TLines.Load;
var
  a: integer;
  s: string;
begin
  Inherited;
  readln(a);
  Width := a;
  readln(s);
  PenColor := StringToColor(s);
  readln(a);
  PenStyle := CasePenStyle(a);
end;

procedure TFigures.Load;
var
  a: integer;
  s: string;
begin
  Inherited;
  readln(s);
  BrushColor := StringToColor(s);
  readln(a);
  BrushStyle := CaseBrushStyle(a);
end;

procedure TRoundRectangle.Load;
var
  a: integer;
begin
  Inherited;
  read(a);
  RWidth := a;
  readln(a);
  RHeight := a;
end;

procedure TRegularPolygon.Load;
var
  a: integer;
begin
  Inherited;
  readln(a);
  ConersCount := a;
end;

procedure SaveInBuffer;
var
  BufferCount, i, j: integer;
begin
  if BufferPointer >= BufferBegin then
  begin
    BufferCount := BufferPointer - BufferBegin + 1;
  end
  else
  begin
    BufferCount := 100 - BufferBegin + BufferPointer + 1;
  end;
  if BufferCount = 100 then
    BufferBegin := (BufferBegin + 1) mod 100;
  BufferPointer := (BufferPointer + 1) mod 100;
  SetLength(Buffer[BufferPointer], Length(Figures));
  for i:=0 to High(Figures) do
  begin
    //Figures[i].Selected := False;
    Buffer[BufferPointer][i] := Figures[i].Copy;
  end;
end;

procedure LoadFromBuffer;
var
  i: integer;
begin

  SetLength(Figures, Length(Buffer[BufferPointer]));
  for i:=0 to High(Figures) do
    Figures[i] := Buffer[BufferPointer][i].Copy;
end;

end.

