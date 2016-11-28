unit UFigures;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, Graphics, UScale;

 type

  TFigure = class
  public
    procedure Draw(Canvas:TCanvas); virtual; abstract;
  public
    Points: array of TDoublePoint;
  end;

  TPolyline = class(TFigure)
    PenStyle:TPenStyle;
    PenColor:TColor;
    Width:integer;
    procedure Draw(Canvas:TCanvas); override;
  end;

  TRectangle = class(TFigure)
    PenStyle:TPenStyle;
    BrushStyle:TBrushStyle;
    PenColor:TColor;
    BrushColor:TColor;
    Width:integer;
    procedure Draw(Canvas:TCanvas); override;
  end;

  TEllipse = class(TFigure)
    PenStyle:TPenStyle;
    BrushStyle:TBrushStyle;
    PenColor:TColor;
    BrushColor:TColor;
    Width:integer;
    procedure Draw(Canvas:TCanvas); override;
  end;

  TLine = class(TFigure)
    PenStyle:TPenStyle;
    PenColor:TColor;
    Width:integer;
    procedure Draw(Canvas:TCanvas); override;
  end;

  TFrame = class(TFigure)
    PenStyle:TPenStyle;
    PenColor:TColor;
    Width:integer;
    procedure Draw(Canvas:TCanvas); override;
  end;

  TRegularPolygon = class(TFigure)
    PenStyle:TPenStyle;
    BrushStyle:TBrushStyle;
    PenColor:TColor;
    BrushColor:TColor;
    Width:integer;
    ConersCount:Integer;
    procedure Draw(Canvas:TCanvas); override;
  end;

implementation

procedure TPolyline.Draw (Canvas: TCanvas);
var
  i:Integer;
begin
  Canvas.Pen.Style:=PenStyle;
  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width:= Width;
  Canvas.MoveTo(WorldToScreen(Points[0]));
  for i:=1 to High(Points) do
  Canvas.LineTo(WorldToScreen(Points[i]));
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Style:=PenStyle;
  Canvas.Brush.Style:=BrushStyle;
  Canvas.Brush.Color:=BrushColor;
  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width:= Width;
  Canvas.Rectangle(WorldToScreen(DoubleRect(Points[0],Points[1])));
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Style:=PenStyle;
  Canvas.Brush.Style:=BrushStyle;
  Canvas.Brush.Color:=BrushColor;
  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width:= Width;
  Canvas.Ellipse(WorldToScreen(DoubleRect(Points[0],Points[1])));
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Style:=PenStyle;
  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width:= Width;
  Canvas.Line(WorldToScreen(DoubleRect(Points[0],Points[1])));
end;

procedure TFrame.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Style:=PenStyle;
  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width:= Width;
  Canvas.Frame(WorldToScreen(DoubleRect(Points[0],Points[1])));
end;

procedure TRegularPolygon.Draw(Canvas: TCanvas);
var
  i,r,k,z:Integer;
  APolygon:array of TPoint;
begin
  k:=0;
  z:=0;
  Canvas.Pen.Style:=PenStyle;
  Canvas.Pen.Color := PenColor;
  Canvas.Brush.Style:=BrushStyle;
  Canvas.Brush.Color:=BrushColor;
  Canvas.Pen.Width:= Width;
  k:=360 div ConersCount;
  r:=round(sqrt(sqr(abs(Points[0].X-Points[1].X))+sqr(abs(Points[0].Y-Points[1].Y))));
  SetLength(APolygon,ConersCount);
  for i:=0 to ConersCount-1 do
    begin
      APolygon[i].X:=Round(Points[0].X+cos(z/180*pi)*r);
      APolygon[i].Y:=Round(Points[0].Y-sin(z/180*pi)*r);
      z:=z+k;
    end;
  for i:=0 to ConersCount-1 do
    APolygon[i]:=WorldToScreen(DoublePoint(APolygon[i].X,APolygon[i].Y));
  Canvas.Polygon(APolygon);
end;

end.

