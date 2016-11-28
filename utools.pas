unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, UScale,Controls,StdCtrls,ExtCtrls,Spin;
  type

  TTool = class
    Icon: String;
    procedure FigureCreate(Point: TPoint); virtual; abstract;
    procedure AddPoint(Point: TPoint); virtual; abstract;
    procedure MouseUp(Point: TPoint;RButton: Boolean); virtual;
    procedure PropertiesCreate(APanel:TPanel); virtual;abstract;
  end;

  TLinesTool = class(TTool)
    Width:Integer;
    ToolPenStyle:TPenStyle;
    procedure WidthChange(Sender:TObject);
    procedure PenStyleChange(Sender:TObject);
    procedure CreatePenProperty(APanel:TPanel);
    procedure PenStylesBoxDrawItem(Control: TWinControl;
      Index: Integer; ARect: TRect; State: TOwnerDrawState);
  end;

  TFigureTool = class(TLinesTool)
    ToolBrushStyle:TBrushStyle;
    procedure BrushStyleChange(Sender:TObject);
    procedure CreateBrushProperty(APanel:TPanel);
    procedure BrushStylesBoxDrawItem(Control: TWinControl;
      Index: Integer; ARect: TRect; State: TOwnerDrawState);
  end;

  TPolylineTool = class(TLinesTool)
     public
     constructor Create;
     procedure FigureCreate(Point: TPoint); override;
     procedure AddPoint(Point: TPoint); override;
     procedure PropertiesCreate(APanel:TPanel);override;
  end;

  TRectangleTool = class(TFigureTool)
    public
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
    procedure PropertiesCreate(APanel:TPanel);override;
  end;

  TRoundRectangleTool = class(TRectangleTool)
  RoundingWidth,RoundingHeight:integer;
  public
  constructor Create;
  procedure FigureCreate(Point: TPoint); override;
  procedure AddPoint(Point: TPoint); override;
  procedure PropertiesCreate(APanel:TPanel);override;
  procedure RoundingWidthChange(Sender:TObject);
  procedure RoundingHeightChange(Sender:TObject);
  end;

  TEllipseTool = class(TFigureTool)
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
    procedure PropertiesCreate(APanel:TPanel);override;
  end;

  TLineTool = class(TLinesTool)
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
    procedure PropertiesCreate(APanel:TPanel);override;
  end;

  TRegularPolygonTool = class(TFigureTool)
    ToolConersCount:integer;
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
    procedure PropertiesCreate(APanel:TPanel);override;
    procedure ConersCountEditChange(Sender:TObject);
  end;

  TMoverTool = class(Ttool)
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
    procedure PropertiesCreate(APanel:TPanel);override;
  end;

  TRectZoomTool = class(Ttool)
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
    procedure MouseUp(Point: TPoint;RButton: Boolean); override;
    procedure PropertiesCreate(APanel:TPanel);override;
  end;


var
  Tools:array of TTool;
  OffsetFirstPoint:TPoint;

implementation

uses Main, UFigures;

procedure RegisterTool(Tool: TTool);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)] := Tool;
end;

procedure TLinesTool.CreatePenProperty(APanel:TPanel);
var
  WidthLabel:TLabel;
  ToolWidth:TSpinEdit;
  PenStylesLable:TLabel;
  PenStylesBox:TComboBox;
begin
  ToolWidth := TSpinEdit.Create(Mainform);
  ToolWidth.Top:=20;
  ToolWidth.MinValue:=1;
  ToolWidth.Parent := APanel;
  ToolWidth.OnChange:=@WidthChange;
  WidthLabel:=TLabel.Create(Mainform);
  WidthLabel.Caption:='Width';
  WidthLabel.Parent:=APanel;
  PenStylesLable:=TLabel.Create(Mainform);
  PenStylesLable.Top:=45;
  PenStylesLable.Caption:='Pen Style';
  PenStylesLable.Parent:=APanel;
  PenStylesBox:=TComboBox.Create(Mainform);
  PenStylesBox.ReadOnly:=True;
  PenStylesBox.Top:=60;
  PenStylesBox.Parent := APanel;
  PenStylesBox.Items.CommaText:='Solid,Dash,Dot,DashDot,DashDotDot';
  PenStylesBox.Style:=csOwnerDrawFixed;
  PenStylesBox.OnDrawItem:=@PenStylesBoxDrawItem;
  PenStylesBox.ItemIndex:=0;
  PenStylesBox.OnChange :=@PenStyleChange;
end;

procedure TLinesTool.PenStylesBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  Y:integer;
begin
  Y:=ARect.Top+7;
  case Index  of
    0 : (Control as TComboBox).Canvas.Pen.Style:= psSolid;
    1 : (Control as TComboBox).Canvas.Pen.Style:= psDash;
    2 : (Control as TComboBox).Canvas.Pen.Style:= psDot;
    3 : (Control as TComboBox).Canvas.Pen.Style:= psDashDot;
    4 : (Control as TComboBox).Canvas.Pen.Style:= psDashDotDot;
  end;
  (Control as TComboBox).Canvas.Line(0, Y, 100,Y);
end;

procedure TFigureTool.CreateBrushProperty(APanel:TPanel);
var
  BrushStylesLable:TLabel;
  BrushStylesBox:TComboBox;
begin
  CreatePenProperty(APanel);
  BrushStylesLable:=TLabel.Create(Mainform);
  BrushStylesLable.Top:=85;
  BrushStylesLable.Caption:='Brush Style';
  BrushStylesLable.Parent:=APanel;
  BrushStylesBox:=TComboBox.Create(Mainform);
  BrushStylesBox.Items.CommaText:=
    'Solid,Diagonal,DiagCross,Vertical,Cross,FDiagonal,Horizontal';
  BrushStylesBox.ItemIndex:=0;
  BrushStylesBox.ReadOnly:=True;
  BrushStylesBox.Top:=100;
  BrushStylesBox.Style:=csOwnerDrawFixed;
  BrushStylesBox.OnDrawItem:=@BrushStylesBoxDrawItem;
  BrushStylesBox.Parent := APanel;
  BrushStylesBox.OnChange:=@BrushStyleChange;
  end;

procedure TFigureTool.BrushStylesBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  PRect:TRect;
begin
  PRect.Left:= ARect.Left+8;
  PRect.Right:= ARect.Right-8;
  PRect.Top:= ARect.Top+4;
  PRect.Bottom:= ARect.Bottom-4;
  case Index  of
    0 : (Control as TComboBox).Canvas.Brush.Style:= bsSolid;
    1 : (Control as TComboBox).Canvas.Brush.Style:= bsBDiagonal;
    2 : (Control as TComboBox).Canvas.Brush.Style:= bsDiagCross;
    3 : (Control as TComboBox).Canvas.Brush.Style:= bsVertical;
    4 : (Control as TComboBox).Canvas.Brush.Style:= bsCross;
    5 : (Control as TComboBox).Canvas.Brush.Style:= bsFDiagonal;
    6 : (Control as TComboBox).Canvas.Brush.Style:= bsHorizontal;
  end;
  (Control as TComboBox).Canvas.Brush.Color:=clBlack;
  (Control as TComboBox).Canvas.Rectangle(PRect);
end;

procedure TLinesTool.WidthChange(Sender:TObject);
begin
  Width:=(Sender as TSpinEdit).Value;
end;

procedure TLinesTool.PenStyleChange(Sender:TObject);
begin
  case (Sender as TComboBox).ItemIndex of
    0:ToolPenStyle:=psSolid;
    1:ToolPenStyle:=psDash;
    2:ToolPenStyle:=psDot;
    3:ToolPenStyle:=psDashDot;
    4:ToolPenStyle:=psDashDotDot;
  end;
end;

procedure TFigureTool.BrushStyleChange(Sender:TObject);
begin
  case (Sender as TComboBox).ItemIndex of
    0:ToolBrushStyle:=bsSolid;
    1:ToolBrushStyle:=bsBDiagonal;
    2:ToolBrushStyle:=bsDiagCross;
    3:ToolBrushStyle:=bsVertical;
    4:ToolBrushStyle:=bsCross;
    5:ToolBrushStyle:=bsFDiagonal;
    6:ToolBrushStyle:=bsHorizontal;
  end;
end;

procedure TTool.MouseUp(Point:TPoint;RButton: Boolean);
begin

end;

procedure TPolylineTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures,Length(Figures)+1);
  Figures[High(Figures)]:=TPolyline.Create;
  Drawing:=true;
  with (Figures[High(Figures)] as TPolyline) do
    begin
      SetLength(Points,Length(Points)+1);
      Points[High(Points)]:=Scrn2Wrld(Point);
      PenStyle:=ToolPenStyle;
      PenColor:=Color1;
      Width:=Self.Width;
    end;
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TPolylineTool.AddPoint(Point: TPoint);
begin
  with Figures[high(Figures)] do
    begin
      SetLength(Points,length(Points)+1);
      Points[high(Points)] := Scrn2Wrld(Point);
    end;
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TPolylineTool.PropertiesCreate(APanel:TPanel);
begin
  CreatePenProperty(APanel);
end;

procedure TRectangleTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures,length(Figures)+1);
  Figures[high(Figures)] := TRectangle.Create;
  Drawing:=true;
  with (Figures[high(Figures)] as TRectangle) do
    begin
      SetLength(Points,2);
      Points[0] := Scrn2Wrld(Point);
      Points[1] := Scrn2Wrld(Point);
      BrushStyle:=ToolBrushStyle;
      PenStyle:=ToolPenStyle;
      PenColor:=Color1;
      BrushColor:=Color2;
      Width:=Self.Width;
    end;
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TRectangleTool.AddPoint(Point: TPoint);
begin
  with Figures[high(Figures)] do
    begin
      Points[1] := Scrn2Wrld(Point);
    end;
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TRectangleTool.PropertiesCreate(APanel:TPanel);
begin
  CreatePenProperty(APanel);
  CreateBrushProperty(APanel);
end;

procedure TRoundRectangleTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures,length(Figures)+1);
  Figures[high(Figures)] := TRoundRectangle.Create;
  Drawing:=true;
  with (Figures[high(Figures)] as TRoundRectangle) do
    begin
      SetLength(Points,2);
      Points[0] := Scrn2Wrld(Point);
      Points[1] := Scrn2Wrld(Point);
      BrushStyle:=ToolBrushStyle;
      PenStyle:=ToolPenStyle;
      PenColor:=Color1;
      BrushColor:=Color2;
      Width:=Self.Width;
      RHeight:=RoundingHeight;
      RWidth:=RoundingWidth;
    end;
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TRoundRectangleTool.AddPoint(Point: TPoint);
begin
  with Figures[high(Figures)] do
    begin
      Points[1] := Scrn2Wrld(Point);
    end;
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TRoundRectangleTool.PropertiesCreate(APanel:TPanel);
var
  RoundingLabel:TLabel;
  WidthRoundingEdit,HeightRoundingEdit:TSpinEdit;
begin
  CreatePenProperty(APanel);
  CreateBrushProperty(APanel);
  RoundingLabel:=TLabel.Create(Mainform);
  RoundingLabel.Caption:='Rounding';
  RoundingLabel.Top:=125;
  RoundingLabel.Parent:=APanel;
  WidthRoundingEdit := TSpinEdit.Create(Mainform);
  WidthRoundingEdit.Top:=140;
  WidthRoundingEdit.MinValue:=1;
  WidthRoundingEdit.MaxValue:=200;
  WidthRoundingEdit.Value:=15;
  WidthRoundingEdit.Parent := APanel;
  WidthRoundingEdit.OnChange:=@RoundingWidthChange;
  RoundingWidth:=15;
  HeightRoundingEdit := TSpinEdit.Create(Mainform);
  HeightRoundingEdit.Top:=165;
  HeightRoundingEdit.MinValue:=1;
  HeightRoundingEdit.MaxValue:=200;
  HeightRoundingEdit.Value:=15;
  HeightRoundingEdit.Parent := APanel;
  HeightRoundingEdit.OnChange:=@RoundingHeightChange;
  RoundingHeight:=15;
end;

procedure TRoundRectangleTool.RoundingWidthChange(Sender:TObject);
begin
  RoundingWidth:=(Sender as TSpinEdit).Value;
end;

procedure TRoundRectangleTool.RoundingHeightChange(Sender:TObject);
begin
  RoundingHeight:=(Sender as TSpinEdit).Value;
end;

procedure TEllipseTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures,length(Figures)+1);
  Figures[high(Figures)] := TEllipse.Create;
  Drawing:=true;
  with (Figures[high(Figures)] as TEllipse) do
    begin
      SetLength(Points,2);
      Points[0] := Scrn2Wrld(Point);
      Points[1] := Scrn2Wrld(Point);
      PenStyle:=ToolPenStyle;
      BrushStyle:=ToolBrushStyle;
      PenColor:=Color1;
      BrushColor:=Color2;
      Width:=Self.Width;
    end;
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TEllipseTool.AddPoint(Point: TPoint);
begin
  Figures[high(Figures)].Points[1] := Scrn2Wrld(Point);
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TEllipseTool.PropertiesCreate(APanel:TPanel);
begin
  CreatePenProperty(APanel);
  CreateBrushProperty(APanel);
end;

procedure TLineTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures,length(Figures)+1);
  Figures[high(Figures)] := TLine.Create;
  Drawing:=true;
  with (Figures[high(Figures)] as TLine) do
    begin
      SetLength(Points,2);
      Points[0] := Scrn2Wrld(Point);
      Points[1] := Scrn2Wrld(Point);
      PenStyle:=ToolPenStyle;
      PenColor:=Color1;
      Width:=Self.Width;
    end;
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TLineTool.AddPoint(Point: TPoint);
begin
  with Figures[high(Figures)] do begin
    Points[1] := Scrn2Wrld(Point);
  end;
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TLineTool.PropertiesCreate(APanel:TPanel);
begin
  CreatePenProperty(APanel);
end;

procedure TRegularPolygonTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures,length(Figures)+1);
  Figures[high(Figures)] := TRegularPolygon.Create;
  Drawing:=true;
  with (Figures[high(Figures)] as TRegularPolygon) do
    begin
      SetLength(Points,2);
      Points[0] := Scrn2Wrld(Point);
      Points[1] := Scrn2Wrld(Point);
      PenStyle:=ToolPenStyle;
      BrushStyle:=ToolBrushStyle;
      PenColor:=Color1;
      BrushColor:=Color2;
      ConersCount:=ToolConersCount;
      Width:=Self.Width;
    end;
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TRegularPolygonTool.AddPoint(Point: TPoint);
begin
  Figures[high(Figures)].Points[1] := Scrn2Wrld(Point);
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TRegularPolygonTool.PropertiesCreate(APanel:TPanel);
var
  ConersCountLabel:TLabel;
  ConersCountEdit:TSpinEdit;
begin
  ConersCountLabel:=TLabel.Create(Mainform);
  ConersCountLabel.Caption:='Coners Count';
  ConersCountLabel.Top:=125;
  ConersCountLabel.Parent:=APanel;
  ConersCountEdit := TSpinEdit.Create(Mainform);
  ConersCountEdit.Top:=140;
  ConersCountEdit.MinValue:=3;
  ConersCountEdit.MaxValue:=50;
  ConersCountEdit.Parent := APanel;
  ConersCountEdit.OnChange:=@ConersCountEditChange;
  ConersCountEditChange(ConersCountEdit);
  CreatePenProperty(APanel);
  CreateBrushProperty(APanel);
end;

procedure TRegularPolygonTool.ConersCountEditChange(Sender:TObject);
begin
  ToolConersCount:=(Sender as TSpinEdit).Value;
end;

procedure TMoverTool.FigureCreate(Point: TPoint);
begin
  OffsetFirstPoint.x:=Offset.x+Point.x;
  OffsetFirstPoint.y:=Offset.y+Point.y;
  Drawing:=true;
end;

procedure TMoverTool.AddPoint(Point: TPoint);
begin
  Offset.x:=OffsetFirstPoint.x-Point.x;
  Offset.y:=OffsetFirstPoint.y-Point.y;
end;

procedure TMoverTool.PropertiesCreate(APanel:TPanel);
begin

end;

procedure TRectZoomTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures,length(Figures)+1);
  Figures[high(Figures)] := TFrame.Create;
  Drawing:=true;
  with (Figures[high(Figures)] as TFrame) do
    begin
      SetLength(Points,2);
      Points[0] := Scrn2Wrld(Point);
      Points[1] := Scrn2Wrld(Point);
      PenStyle:=psSolid;
      PenColor:=clBlack;
      Width:=1;
    end;
end;

procedure TRectZoomTool.AddPoint(Point: TPoint);
begin
  with Figures[high(Figures)] do
    begin
      Points[1] := Scrn2Wrld(Point);
    end;
end;

procedure TRectZoomTool.MouseUp(Point: TPoint;RButton: Boolean);
begin
  with Figures[high(Figures)] do
    begin
      if (Points[0].x <> Points[1].x) and
        (Points[0].y <> Points[1].y)
      then
        RectZoom(Mainform.PaintBox1.Height,
          Mainform.PaintBox1.Width,
          Points[0],
          Points[1])
      else
        PointZoom(DoublePoint(Point.x,Point.y),RButton,Mainform.PaintBox1.Height,
          Mainform.PaintBox1.Width);
    end;
  SetLength(Figures,length(Figures)-1);
  Mainform.ZoomEdit.Text:=IntToStr(round(Zoom));
end;

procedure TRectZoomTool.PropertiesCreate(APanel:TPanel);
begin

end;

constructor TPolylineTool.Create;
begin
  Icon := 'ico/polyline.png';
end;

constructor TRectangleTool.Create;
begin
  Icon := 'ico/rectangle.png';
end;

constructor TRoundRectangleTool.Create;
begin
  Icon := 'ico/RoundRectangle.png';
end;

constructor TEllipseTool.Create;
begin
  Icon := 'ico/ellipse.png';
end;

constructor TLineTool.Create;
begin
  Icon := 'ico/line.png';
end;

constructor TRegularPolygonTool.Create;
begin
  Icon := 'ico/RegularPolygon.png';
end;

constructor TMoverTool.Create;
begin
  Icon:='ico/mover.png';
end;

constructor TRectZoomTool.Create;
begin
  Icon:='ico/rectzoom.png';
end;

initialization

RegisterTool(TPolylineTool.Create);
RegisterTool(TRectangleTool.Create);
RegisterTool(TRoundRectangleTool.Create);
RegisterTool(TEllipseTool.Create);
RegisterTool(TLineTool.Create);
RegisterTool(TRegularPolygonTool.Create);
RegisterTool(TMoverTool.Create);
RegisterTool(TRectZoomTool.Create);

end.

