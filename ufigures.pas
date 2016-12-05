unit UFigures;

{$mode objfpc}{$H+}

interface

 uses
   Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
   Dialogs, Menus,ExtCtrls, StdCtrls, Grids, LCLIntf,LCLType,
   Buttons, GraphMath, Math, Spin, FPCanvas, TypInfo, LCL, Windows, uscale;

 type

  TFigure = class
    Points: array of TDoublePoint;
    Selected: boolean;
    Region: HRGN;
    procedure Draw(Canvas: TCanvas); virtual; abstract;
    procedure CreateRegion; virtual; abstract;
    procedure DrawOutline(Point1,Point2: TDoublePoint; Canvas: TCanvas); virtual;
  end;

  TPolyline = class(TFigure)
    PenStyle: TPenStyle;
    PenColor: TColor;
    Width: integer;
    procedure Draw(Canvas: TCanvas); override;
    procedure CreateRegion; override;
  end;

  TRectangle = class(TFigure)
    PenStyle: TPenStyle;
    BrushStyle: TBrushStyle;
    PenColor: TColor;
    BrushColor: TColor;
    Width: integer;
    procedure Draw(Canvas: TCanvas); override;
    procedure CreateRegion; override;
  end;

  TRoundRectangle = class(TRectangle)
    RWidth,RHeight: integer;
    procedure Draw(Canvas: TCanvas); override;
    procedure CreateRegion; override;
  end;

  TEllipse = class(TFigure)
    PenStyle: TPenStyle;
    BrushStyle: TBrushStyle;
    PenColor: TColor;
    BrushColor: TColor;
    Width: integer;
    procedure Draw(Canvas: TCanvas); override;
    procedure CreateRegion; override;
  end;

  TLine = class(TFigure)
    PenStyle: TPenStyle;
    PenColor: TColor;
    Width: integer;
    procedure Draw(Canvas: TCanvas); override;
    procedure CreateRegion; override;
  end;

  TFrame = class(TFigure)
    PenStyle: TPenStyle;
    PenColor: TColor;
    Width: integer;
    procedure Draw(Canvas: TCanvas); override;
  end;

  TRegularPolygon = class(TFigure)
    PenStyle: TPenStyle;
    BrushStyle: TBrushStyle;
    PenColor: TColor;
    BrushColor: TColor;
    Width: integer;
    ConersCount: Integer;
    procedure Draw(Canvas: TCanvas); override;
    function Rotate(P1, P2: TDoublePoint; angle: double): TDoublePoint;
    procedure CreateRegion; override;
  end;

  procedure LineRegion(p1,p2:TPoint;var tempPoints: array of TPoint;Width:integer);

var
  RectR: TPoint;

implementation

procedure TFigure.DrawOutline(Point1,Point2: TDoublePoint; Canvas: TCanvas);
var
  a:TDoublepoint;
begin
  if (Point1.X>Point2.X) then
    begin
      a:=Point1;
      Point1:=Point2;
      Point2:=a;
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
  Max,Min:TDoublePoint;
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
  for i := 0 to ConersCount - 1 do
    APolygonScreen[i] := WorldToScreen(DoublePoint(APolygonScreen[i].X, APolygonScreen[i].Y));
  Canvas.Polygon(APolygonScreen);
  if (Selected = true) then
  begin
    DeleteObject(Region);
    Min:=DoublePoint(MaxFloat,MaxFloat);
    for i := 0 to ConersCount - 1 do
      begin
        if APolygonScreen[i].X > Max.X then Max.X:=APolygonScreen[i].X;
        if APolygonScreen[i].Y > Max.Y then Max.Y:=APolygonScreen[i].Y;
        if APolygonScreen[i].X < Min.X then Min.X:=APolygonScreen[i].X;
        if APolygonScreen[i].Y < Min.Y then Min.Y:=APolygonScreen[i].Y;
      end;
      DrawOutline(Min, Max, Canvas);
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
  Region := CreatePolygonRgn (RegionPoints,3,2);
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
  r, FAngle: double;
  i,k: integer;
  PolygonPoints: array of TDoublePoint;
  PolygonPointsScr: array of TPoint;
begin
  r := sqrt(abs(sqr(Points[1].x-Points[0].x) + sqr(Points[1].y-Points[0].y)));
  k:=360 div ConersCount;
  FAngle := Arctan2(Points[0].Y - Points[1].Y, Points[0].X - Points[1].X);
  setlength (PolygonPoints, ConersCount);
  setlength (PolygonPointsScr, ConersCount);
  for i := low(PolygonPoints) to high(PolygonPoints) do
  begin
    PolygonPoints[i].x := Points[0].x + r*cos(i*k/180*Pi);
    PolygonPoints[i].y := Points[0].Y + r*sin(i*k/180*Pi);
    PolygonPointsScr[i] := WorldToScreen(Rotate(Points[0],PolygonPoints[i],FAngle));
  end;
  Region := CreatePolygonRgn (PolygonPointsScr[0],length(PolygonPointsScr),winding);
end;

end.

