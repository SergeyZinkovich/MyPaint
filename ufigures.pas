unit UFigures;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, Graphics, UScale, GraphMath, Math, Dialogs;

 type

  TFigure = class
  public
    procedure Draw(Canvas: TCanvas); virtual; abstract;
  public
    Points: array of TDoublePoint;
  end;

  TPolyline = class(TFigure)
    PenStyle: TPenStyle;
    PenColor: TColor;
    Width: integer;
    procedure Draw(Canvas: TCanvas); override;
  end;

  TRectangle = class(TFigure)
    PenStyle: TPenStyle;
    BrushStyle: TBrushStyle;
    PenColor: TColor;
    BrushColor: TColor;
    Width: integer;
    procedure Draw(Canvas: TCanvas); override;
  end;

  TRoundRectangle = class(TRectangle)
    RWidth,RHeight: integer;
    procedure Draw(Canvas: TCanvas); override;
  end;

  TEllipse = class(TFigure)
    PenStyle: TPenStyle;
    BrushStyle: TBrushStyle;
    PenColor: TColor;
    BrushColor: TColor;
    Width: integer;
    procedure Draw(Canvas: TCanvas); override;
  end;

  TLine = class(TFigure)
    PenStyle: TPenStyle;
    PenColor: TColor;
    Width: integer;
    procedure Draw(Canvas: TCanvas); override;
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
  end;

implementation

procedure TPolyline.Draw (Canvas: TCanvas);
var
  i: Integer;
begin
  Canvas.Pen.Style := PenStyle;
  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width := Width;
  Canvas.MoveTo(WorldToScreen(Points[0]));
  for i := 1 to High(Points) do
  Canvas.LineTo(WorldToScreen(Points[i]));
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Style := PenStyle;
  Canvas.Brush.Style := BrushStyle;
  Canvas.Brush.Color := BrushColor;
  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width := Width;
  Canvas.Rectangle(WorldToScreen(DoubleRect(Points[0], Points[1])));
end;

procedure TRoundRectangle.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Style := PenStyle;
  Canvas.Brush.Style := BrushStyle;
  Canvas.Brush.Color := BrushColor;
  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width := Width;
  Canvas.RoundRect(WorldToScreen(DoubleRect(Points[0], Points[1])), RWidth, RHeight);
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Style := PenStyle;
  Canvas.Brush.Style := BrushStyle;
  Canvas.Brush.Color := BrushColor;
  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width := Width;
  Canvas.Ellipse(WorldToScreen(DoubleRect(Points[0], Points[1])));
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Style:=PenStyle;
  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width := Width;
  Canvas.Line(WorldToScreen(DoubleRect(Points[0], Points[1])));
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
end;

function TRegularPolygon.Rotate(P1,P2: TDoublePoint; angle: double): TDoublePoint;
begin
  Result.x := P1.x + (P2.x - P1.x) * cos(angle)
    - (P2.Y - P1.Y) * sin(angle);
  Result.Y := P1.y + (P2.x - P1.x) * sin(angle)
    + (P2.Y - P1.Y) * cos(angle);
end;

end.

