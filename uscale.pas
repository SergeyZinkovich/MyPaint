unit UScale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, Controls;

  type
  TDoublePoint = record
    X: Double;
    Y: Double;
  end;

 type
  TDoubleRect = record
    X1: Double;
    Y1: Double;
    X2: Double;
    Y2: Double;
  end;

  function DoublePoint(X, Y: Double): TDoublePoint;
  function DoubleRect(A, B: TDoublePoint): TDoubleRect;
  function WorldToScreen(ARect: TDoubleRect): TRect;
  function WorldToScreen(P: TDoublePoint): TPoint;
  function Scrn2Wrld(P: TPoint): TDoublePoint;
  procedure FindMinMax(APoint: TDoublePoint);
  procedure RectZoom(Height, Width: Integer;AMin, AMax: TDoublePoint);
  procedure CenterZoom(AWidth, AHeight: integer;OldZoom: Double);
  procedure Scroll(AVert, Ahorz: integer);
  procedure PointZoom(APoint: TDoublePoint;RButton: Boolean;Height, Width: Integer);
var
  Zoom: integer = 100;
  Offset: TPoint;
  MinPoint: TDoublePoint;
  MaxPoint: TDoublePoint;
  CanvasClean: boolean = true;
implementation

procedure CenterZoom(AWidth, AHeight: integer;OldZoom: Double);
begin
  if Zoom > oldzoom then
    begin
      Offset.x := Offset.x + round(AWidth * (Zoom - oldzoom) / 200);
      Offset.y := Offset.y + round(AHeight * (Zoom - oldzoom) / 200);
    end
  else
    begin
      Offset.x := Offset.x - round(AWidth * (oldzoom - Zoom) / 200);
      Offset.y := Offset.y - round(AHeight * (oldzoom-Zoom) / 200);
    end;
end;

function DoublePoint(X, Y: Double): TDoublePoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function DoubleRect(A, B: TDoublePoint): TDoubleRect;
begin
  Result.X1 := A.X;
  Result.X2 := B.X;
  Result.Y1 := A.Y;
  Result.Y2 := B.Y;
end;

procedure FindMinMax(APoint: TDoublePoint);
begin
  if (APoint.x > MaxPoint.x) then
     MaxPoint.x := APoint.x;
  if (APoint.y > MaxPoint.y) then
     MaxPoint.y := APoint.y;
  if (APoint.x < MinPoint.x) or CanvasClean then
     MinPoint.x := APoint.x;
  if (APoint.y < MinPoint.y) or CanvasClean then
     MinPoint.y := APoint.y;
  CanvasClean := false;
end;

function WorldToScreen(P: TDoublePoint): TPoint;
begin
  Result.X := round(P.X * Zoom / 100) - Offset.x;
  Result.y := round(P.Y * Zoom / 100) - Offset.y;
end;

function WorldToScreen(ARect: TDoubleRect): TRect;
begin
  Result.TopLeft := WorldToScreen(DoublePoint(ARect.X1, ARect.Y1));
  Result.BottomRight := WorldToScreen(DoublePoint(ARect.X2, ARect.Y2));
end;

function Scrn2Wrld(P: TPoint): TDoublePoint;
begin
  Result.X := (P.x + Offset.x) / Zoom * 100;
  Result.Y := (P.y + Offset.y) / Zoom * 100;
end;

procedure RectZoom(Height, Width: Integer;AMin, AMax: TDoublePoint);
var
  oldzoom: Double;
  DPoint: TDoublePoint;
begin
  if AMin.X > AMax.X then
    begin
      DPoint := AMax;
      AMax := AMin;
      AMin := DPoint;
    end;
  oldzoom := Zoom;
  if (AMin.X = AMax.X) and (AMin.Y = AMax.Y) then
    exit;
  Zoom := round(Min(Height / (AMax.Y - AMin.Y) * 100,
    Width / (AMax.X - AMin.X) * 100)) - 1;
  if Zoom > 1000 then
    Zoom := 1000;
  if (Zoom <> oldzoom) and (Zoom <> 100)  then
    begin
      Offset.x := round(AMin.X * Zoom / 100) - 5;
      Offset.y := round(AMin.Y * Zoom / 100) - 5;
    end;
end;

procedure PointZoom(APoint: TDoublePoint;RButton: Boolean;Height, Width: Integer);
begin
  if RButton then
    Zoom := round(Zoom / 2)
  else
    Zoom := Zoom * 2;
  if Zoom > 1000 then
    Zoom := 1000;
  if Zoom < 1 then
    Zoom := 1;
  Offset.x := Offset.x + (WorldToScreen(APoint).X - round(APoint.X));
  Offset.y := Offset.y + (WorldToScreen(APoint).y - round(APoint.Y));
end;

procedure Scroll(AVert, Ahorz: integer);
begin
  Offset.x := Ahorz;
  Offset.y := AVert;
end;

end.

