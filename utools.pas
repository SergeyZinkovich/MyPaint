unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Grids, LCLIntf, LCLType, Buttons,
  GraphMath, Math, Spin, FPCanvas, TypInfo, LCL,
  ufigures,uscale;

  type
  TFigureClass = class of TFigure;
  TIntegerArray = array of integer;

  TProperty = class
    procedure ObjectsCreate(APanel: TPanel; Value: integer);virtual;abstract;
    function GetPropertyValue(F: TFigure): integer;virtual;abstract;
  end;

    type
  ArrayOfProperty = array of TProperty;

  TWidthProperty = class(TProperty)
    procedure ObjectsCreate(APanel: TPanel; Value: integer);override;
    function GetPropertyValue(F: TFigure): integer; override;
    procedure WidthChange(Sender: TObject);
  end;

  TPenStyleProperty = class(TProperty)
    procedure ObjectsCreate(APanel: TPanel; Value: integer);override;
    procedure PenStyleChange(Sender: TObject);
    function GetPropertyValue(F: TFigure): integer; override;
    procedure PenStylesBoxDrawItem(Control: TWinControl;
      Index: Integer; ARect: TRect; State: TOwnerDrawState);
  end;

  TBrushStyleProperty = class(TProperty)
    procedure ObjectsCreate(APanel: TPanel; Value: integer);override;
    procedure BrushStyleChange(Sender: TObject);
    function GetPropertyValue(F: TFigure): integer; override;
    procedure BrushStylesBoxDrawItem(Control: TWinControl;
      Index: Integer; ARect: TRect; State: TOwnerDrawState);
  end;

  TConersCountProperty = class(TProperty)
    procedure ObjectsCreate(APanel: TPanel; Value: integer);override;
    function GetPropertyValue(F: TFigure): integer; override;
    procedure ConersCountEditChange(Sender: TObject);
  end;

  TWidthRoundingProperty = class(TProperty)
    procedure ObjectsCreate(APanel: TPanel; Value: integer);override;
    function GetPropertyValue(F: TFigure): integer; override;
    procedure RoundingWidthChange(Sender: TObject);
  end;

  THeightRoundingProperty = class(TProperty)
    procedure ObjectsCreate(APanel: TPanel; Value: integer);override;
    function GetPropertyValue(F: TFigure): integer; override;
    procedure RoundingHeightChange(Sender: TObject);
  end;

  TTool = class
    FigureClass:TFigureClass;
    Properties: array of TProperty;
    Icon: String;
    procedure FigureCreate(Point: TPoint); virtual; abstract;
    procedure AddPoint(Point: TPoint); virtual; abstract;
    procedure MouseUp(Point: TPoint;RButton: Boolean;APanel: TPanel); virtual;
    procedure PropertiesListCreate; virtual; abstract;
    procedure PropertiesCreate(APanel: TPanel);virtual;
  end;

  TLinesTool = class(TTool)
    Width: Integer;
    ToolPenStyle: TPenStyle;
  end;

  TFigureTool = class(TLinesTool)
    ToolBrushStyle: TBrushStyle;
  end;

  TPolylineTool = class(TLinesTool)
     public
     constructor Create;
     procedure FigureCreate(Point: TPoint); override;
     procedure AddPoint(Point: TPoint); override;
     procedure PropertiesListCreate;override;
  end;

  TRectangleTool = class(TFigureTool)
    public
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
    procedure PropertiesListCreate;override;
  end;

  TRoundRectangleTool = class(TFigureTool)
  RoundingWidth,RoundingHeight:integer;
  public
  constructor Create;
  procedure FigureCreate(Point: TPoint); override;
  procedure AddPoint(Point: TPoint); override;
  procedure PropertiesListCreate;override;
  end;

  TEllipseTool = class(TFigureTool)
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
    procedure PropertiesListCreate;override;
  end;

  TLineTool = class(TLinesTool)
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
    procedure PropertiesListCreate;override;
  end;

  TRegularPolygonTool = class(TFigureTool)
    ToolConersCount: integer;
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
    procedure PropertiesListCreate;override;
  end;

  TScrollTool = class(Ttool)
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
    procedure PropertiesListCreate;override;
  end;

  TRectZoomTool = class(Ttool)
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
    procedure MouseUp(Point: TPoint;RButton: Boolean;APanel: TPanel); override;
    procedure PropertiesListCreate;override;
  end;

  TSelectTool = class(TTool)
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
    procedure MouseUp(Point: TPoint;RButton: Boolean;APanel: TPanel); override;
    procedure PropertiesListCreate;override;
    procedure PointSelectTool(Point: TPoint);
    procedure RectSelectTool(Point: TPoint);
    function SelectProperties:ArrayOfProperty;
    function SetValueArray(Prop: ArrayOfProperty): TIntegerArray;
  end;

  TMoverTool = class(TTool)
    APoint: TPoint;
    constructor Create;
    procedure FigureCreate(Point: TPoint); override;
    procedure AddPoint(Point: TPoint); override;
    procedure PropertiesListCreate; override;
  end;

  function CasePenStyle(Index: integer):TPenStyle;
  function CaseBrushStyle(Index: integer):TBrushStyle;
  function CasePenStyleIndex(Style: TPenStyle): integer;
  function CaseBrushStyleIndex(BrushStyle: TBrushStyle): integer;
  procedure ColorChange(Color1, Color2: TColor; Mode: Boolean);

var
  Tools: array of TTool;
  OffsetFirstPoint: TPoint;
  CtrlPressed: boolean;
  InvalidateHandler: procedure of Object;
  DeletePanelHandler: procedure of Object;
  CreatePanelHandler: procedure of Object;

implementation

uses Main;

procedure RegisterTool(Tool: TTool; AClass: TFigureClass );
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)] := Tool;
  Tools[High(Tools)].FigureClass := AClass;
end;

procedure TTool.PropertiesCreate(APanel: TPanel);
var
  i:integer;
begin
  ChoosenTool.PropertiesListCreate;
  for i := 0 to High(ChoosenTool.Properties) do
    ChoosenTool.Properties[i].ObjectsCreate(APanel, 0);
end;

procedure TWidthProperty.ObjectsCreate(APanel: TPanel; Value: integer);
var
  WidthLabel: TLabel;
  ToolWidth: TSpinEdit;
begin
  WidthLabel := TLabel.Create(Mainform);
  WidthLabel.Caption := 'Width';
  WidthLabel.Parent := APanel;

  ToolWidth := TSpinEdit.Create(Mainform);
  ToolWidth.Top := 20;
  ToolWidth.MinValue := 1;
  ToolWidth.Parent := APanel;
  ToolWidth.OnChange := @WidthChange;

  ToolWidth.Value:=Value;

  if ChoosenTool.ClassName <> TSelectTool.ClassName then
    WidthChange(ToolWidth);
end;

procedure TPenStyleProperty.ObjectsCreate(APanel: TPanel; Value: integer);
var
  PenStylesLable: TLabel;
  PenStylesBox: TComboBox;
begin
  PenStylesLable := TLabel.Create(Mainform);
  PenStylesLable.Top := 45;
  PenStylesLable.Caption := 'Pen Style';
  PenStylesLable.Parent := APanel;

  PenStylesBox := TComboBox.Create(Mainform);
  PenStylesBox.ReadOnly := True;
  PenStylesBox.Top := 60;
  PenStylesBox.Parent := APanel;
  PenStylesBox.Items.CommaText := ',,,,';
  PenStylesBox.Style := csOwnerDrawFixed;
  PenStylesBox.OnDrawItem := @PenStylesBoxDrawItem;
  PenStylesBox.OnChange := @PenStyleChange;

  PenStylesBox.ItemIndex := Value;

  if ChoosenTool.ClassName <> TSelectTool.ClassName then
    PenStyleChange(PenStylesBox);
end;

procedure TPenStyleProperty.PenStylesBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  Y: integer;
begin
  Y := ARect.Top + 7;
  (Control as TComboBox).Canvas.Pen.Style := CasePenStyle(Index);
  (Control as TComboBox).Canvas.Line(0, Y, 100, Y);
end;

procedure TBrushStyleProperty.ObjectsCreate(APanel: TPanel; Value: integer);
var
  BrushStylesLable: TLabel;
  BrushStylesBox: TComboBox;
begin
  BrushStylesLable := TLabel.Create(Mainform);
  BrushStylesLable.Top := 85;
  BrushStylesLable.Caption := 'Brush Style';
  BrushStylesLable.Parent := APanel;

  BrushStylesBox := TComboBox.Create(Mainform);
  BrushStylesBox.Items.CommaText := ',,,,,,';
  BrushStylesBox.ReadOnly := True;
  BrushStylesBox.Top := 100;
  BrushStylesBox.Style := csOwnerDrawFixed;
  BrushStylesBox.OnDrawItem := @BrushStylesBoxDrawItem;
  BrushStylesBox.Parent := APanel;
  BrushStylesBox.OnChange := @BrushStyleChange;

  BrushStylesBox.ItemIndex := Value;

  if ChoosenTool.ClassName <> TSelectTool.ClassName then
    BrushStyleChange(BrushStylesBox);
  end;

procedure TBrushStyleProperty.BrushStylesBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  PRect: TRect;
begin
  PRect.Left := ARect.Left+8;
  PRect.Right := ARect.Right-8;
  PRect.Top := ARect.Top+4;
  PRect.Bottom := ARect.Bottom-4;
  (Control as TComboBox).Canvas.Brush.Style := CaseBrushStyle(Index);
  (Control as TComboBox).Canvas.Brush.Color := clBlack;
  (Control as TComboBox).Canvas.Rectangle(PRect);
end;

procedure TWidthProperty.WidthChange(Sender: TObject);
var
  i: Integer;
begin
  if ChoosenTool.ClassName <> TSelectTool.ClassName then
    (ChoosenTool as TLinesTool).Width := (Sender as TSpinEdit).Value
  else
    for i:=0 to High(Figures) do
      if Figures[i].Selected then
        (Figures[i] as TLines).Width := (Sender as TSpinEdit).Value;
  InvalidateHandler;
end;

procedure TPenStyleProperty.PenStyleChange(Sender: TObject);
var
  i: Integer;
begin
  if ChoosenTool.ClassName <> TSelectTool.ClassName then
    (ChoosenTool as TLinesTool).ToolPenStyle :=
      CasePenStyle((Sender as TComboBox).ItemIndex)
  else
    for i:=0 to High(Figures) do
      if Figures[i].Selected then
        (Figures[i] as TLines).PenStyle :=
          CasePenStyle((Sender as TComboBox).ItemIndex);
  InvalidateHandler;
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

procedure TBrushStyleProperty.BrushStyleChange(Sender:TObject);
var
  i: Integer;
begin
  if ChoosenTool.ClassName <> TSelectTool.ClassName then
    (ChoosenTool as TFigureTool).ToolBrushStyle :=
      CaseBrushStyle((Sender as TComboBox).ItemIndex)
  else
    for i:=0 to High(Figures) do
      if Figures[i].Selected then
        (Figures[i] as TFigures).BrushStyle :=
          CaseBrushStyle((Sender as TComboBox).ItemIndex);
  InvalidateHandler;
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

procedure TWidthRoundingProperty.ObjectsCreate(APanel:TPanel; Value: integer);
var
  RoundingLabel: TLabel;
  WidthRoundingEdit: TSpinEdit;
begin
  RoundingLabel := TLabel.Create(Mainform);
  RoundingLabel.Caption := 'Rounding';
  RoundingLabel.Top := 125;
  RoundingLabel.Parent := APanel;

  WidthRoundingEdit := TSpinEdit.Create(Mainform);
  WidthRoundingEdit.Top := 140;
  WidthRoundingEdit.MinValue := 15;
  WidthRoundingEdit.MaxValue := 200;
  WidthRoundingEdit.Parent := APanel;
  WidthRoundingEdit.OnChange := @RoundingWidthChange;

  WidthRoundingEdit.Value := Value;

  if ChoosenTool.ClassName <> TSelectTool.ClassName then
    RoundingWidthChange(WidthRoundingEdit);
end;

procedure THeightRoundingProperty.ObjectsCreate(APanel:TPanel; Value: integer);
var
  HeightRoundingEdit: TSpinEdit;
begin
  HeightRoundingEdit := TSpinEdit.Create(Mainform);
  HeightRoundingEdit.Top := 165;
  HeightRoundingEdit.MinValue := 15;
  HeightRoundingEdit.MaxValue := 200;
  HeightRoundingEdit.Parent := APanel;
  HeightRoundingEdit.OnChange := @RoundingHeightChange;

  HeightRoundingEdit.Value := Value;

  if ChoosenTool.ClassName <> TSelectTool.ClassName then
    RoundingHeightChange(HeightRoundingEdit);
end;

procedure ColorChange(Color1, Color2: TColor; Mode: Boolean);
var
  i:integer;
begin
  for i:=0 to High(Figures) do
      if Figures[i].Selected then
        begin
          if Mode then
            (Figures[i] as TLines).PenColor := Color1
          else
            if Figures[i].ClassParent.ClassName = TFigures.ClassName then
            (Figures[i] as TFigures).BrushColor := Color2;
        end;
  InvalidateHandler;
end;

procedure TTool.MouseUp(Point: TPoint;RButton: Boolean;APanel: TPanel);
begin

end;

procedure TPolylineTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := TPolyline.Create;
  Drawing := true;
  with (Figures[High(Figures)] as TPolyline) do
    begin
      SetLength(Points, Length(Points) + 1);
      Points[High(Points)] := Scrn2Wrld(Point);
      PenStyle := ToolPenStyle;
      PenColor := Color1;
      Width := Self.Width;
    end;
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TPolylineTool.AddPoint(Point: TPoint);
begin
  with Figures[high(Figures)] do
    begin
      SetLength(Points, length(Points) + 1);
      Points[high(Points)] := Scrn2Wrld(Point);
    end;
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TPolylineTool.PropertiesListCreate;
begin
  SetLength(Properties,2);
  Properties[0] := TWidthProperty.Create;
  Properties[1] := TPenStyleProperty.Create;
end;

procedure TRectangleTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures, length(Figures) + 1);
  Figures[high(Figures)] := TRectangle.Create;
  Drawing:=true;
  with (Figures[high(Figures)] as TRectangle) do
    begin
      SetLength(Points, 2);
      Points[0] := Scrn2Wrld(Point);
      Points[1] := Scrn2Wrld(Point);
      BrushStyle := ToolBrushStyle;
      PenStyle := ToolPenStyle;
      PenColor := Color1;
      BrushColor := Color2;
      Width := Self.Width;
    end;
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TRectangleTool.AddPoint(Point: TPoint);
begin
  Figures[high(Figures)].Points[1] := Scrn2Wrld(Point);
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TRectangleTool.PropertiesListCreate;
begin
  SetLength(Properties,3);
  Properties[0] := TWidthProperty.Create;
  Properties[1] := TPenStyleProperty.Create;
  Properties[2] := TBrushStyleProperty.Create;
end;

procedure TRoundRectangleTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures, length(Figures) + 1);
  Figures[high(Figures)] := TRoundRectangle.Create;
  Drawing:=true;
  with (Figures[high(Figures)] as TRoundRectangle) do
    begin
      SetLength(Points, 2);
      Points[0] := Scrn2Wrld(Point);
      Points[1] := Scrn2Wrld(Point);
      BrushStyle := ToolBrushStyle;
      PenStyle := ToolPenStyle;
      PenColor := Color1;
      BrushColor := Color2;
      Width := Self.Width;
      RHeight := RoundingHeight;
      RWidth := RoundingWidth;
    end;
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TRoundRectangleTool.AddPoint(Point: TPoint);
begin
  Figures[high(Figures)].Points[1] := Scrn2Wrld(Point);
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TRoundRectangleTool.PropertiesListCreate;
begin
  SetLength(Properties,5);
  Properties[0] := TWidthProperty.Create;
  Properties[1] := TPenStyleProperty.Create;
  Properties[2] := TBrushStyleProperty.Create;
  Properties[3] := TWidthRoundingProperty.Create;
  Properties[4] := THeightRoundingProperty.Create;
end;

procedure TWidthRoundingProperty.RoundingWidthChange(Sender: TObject);
var
  i: Integer;
begin
  if ChoosenTool.ClassName <> TSelectTool.ClassName then
    (ChoosenTool as TRoundRectangleTool).RoundingWidth := (Sender as TSpinEdit).Value
  else
    for i:=0 to High(Figures) do
      if Figures[i].Selected then
        (Figures[i] as TRoundRectangle).RWidth := (Sender as TSpinEdit).Value;
  InvalidateHandler;
end;

procedure THeightRoundingProperty.RoundingHeightChange(Sender: TObject);
var
  i: Integer;
begin
  if ChoosenTool.ClassName <> TSelectTool.ClassName then
    (ChoosenTool as TRoundRectangleTool).RoundingHeight := (Sender as TSpinEdit).Value
  else
    for i:=0 to High(Figures) do
      if Figures[i].Selected then
        (Figures[i] as TRoundRectangle).RHeight := (Sender as TSpinEdit).Value;
  InvalidateHandler;
end;

procedure TEllipseTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures, length(Figures) + 1);
  Figures[high(Figures)] := TEllipse.Create;
  Drawing:=true;
  with (Figures[high(Figures)] as TEllipse) do
    begin
      SetLength(Points, 2);
      Points[0] := Scrn2Wrld(Point);
      Points[1] := Scrn2Wrld(Point);
      PenStyle := ToolPenStyle;
      BrushStyle := ToolBrushStyle;
      PenColor := Color1;
      BrushColor := Color2;
      Width := Self.Width;
    end;
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TEllipseTool.AddPoint(Point: TPoint);
begin
  Figures[high(Figures)].Points[1] := Scrn2Wrld(Point);
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TEllipseTool.PropertiesListCreate;
begin
  SetLength(Properties,3);
  Properties[0] := TWidthProperty.Create;
  Properties[1] := TPenStyleProperty.Create;
  Properties[2] := TBrushStyleProperty.Create;
end;

procedure TLineTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures, length(Figures) + 1);
  Figures[high(Figures)] := TLine.Create;
  Drawing := true;
  with (Figures[high(Figures)] as TLine) do
    begin
      SetLength(Points,2);
      Points[0] := Scrn2Wrld(Point);
      Points[1] := Scrn2Wrld(Point);
      PenStyle := ToolPenStyle;
      PenColor := Color1;
      Width := Self.Width;
    end;
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TLineTool.AddPoint(Point: TPoint);
begin
  Figures[high(Figures)].Points[1] := Scrn2Wrld(Point);
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TLineTool.PropertiesListCreate;
begin
  SetLength(Properties,2);
  Properties[0] := TWidthProperty.Create;
  Properties[1] := TPenStyleProperty.Create;
end;

procedure TRegularPolygonTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures, length(Figures) + 1);
  Figures[high(Figures)] := TRegularPolygon.Create;
  Drawing := true;
  with (Figures[high(Figures)] as TRegularPolygon) do
    begin
      SetLength(Points, 2);
      Points[0] := Scrn2Wrld(Point);
      Points[1] := Scrn2Wrld(Point);
      PenStyle := ToolPenStyle;
      BrushStyle := ToolBrushStyle;
      PenColor := Color1;
      BrushColor := Color2;
      ConersCount := ToolConersCount;
      Width := Self.Width;
    end;
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TRegularPolygonTool.AddPoint(Point: TPoint);
begin
  Figures[high(Figures)].Points[1] := Scrn2Wrld(Point);
  FindMinMax(Scrn2Wrld(Point));
end;

procedure TRegularPolygonTool.PropertiesListCreate;
begin
  SetLength(Properties,4);
  Properties[0] := TWidthProperty.Create;
  Properties[1] := TPenStyleProperty.Create;
  Properties[2] := TBrushStyleProperty.Create;
  Properties[3] := TConersCountProperty.Create;
end;

procedure TConersCountProperty.ObjectsCreate(APanel:TPanel; Value: integer);
  var
  ConersCountLabel: TLabel;
  ConersCountEdit: TSpinEdit;
begin
  ConersCountLabel := TLabel.Create(Mainform);
  ConersCountLabel.Caption := 'Coners Count';
  ConersCountLabel.Top := 125;
  ConersCountLabel.Parent := APanel;

  ConersCountEdit := TSpinEdit.Create(Mainform);
  ConersCountEdit.Top := 140;
  ConersCountEdit.MinValue := 3;
  ConersCountEdit.MaxValue := 50;
  ConersCountEdit.Parent := APanel;
  ConersCountEdit.OnChange := @ConersCountEditChange;

  ConersCountEdit.Value := Value;

  if ChoosenTool.ClassName <> TSelectTool.ClassName then
    ConersCountEditChange(ConersCountEdit);
end;

procedure TConersCountProperty.ConersCountEditChange(Sender: TObject);
var
  i: Integer;
begin
  if ChoosenTool.ClassName <> TSelectTool.ClassName then
    (ChoosenTool as TRegularPolygonTool).ToolConersCount := (Sender as TSpinEdit).Value
  else
    for i:=0 to High(Figures) do
      if Figures[i].Selected then
        (Figures[i] as TRegularPolygon).ConersCount := (Sender as TSpinEdit).Value;
  InvalidateHandler;
end;

procedure TScrollTool.FigureCreate(Point: TPoint);
begin
  OffsetFirstPoint.x := Offset.x + Point.x;
  OffsetFirstPoint.y := Offset.y + Point.y;
  Drawing := true;
end;

procedure TScrollTool.AddPoint(Point: TPoint);
begin
  Offset.x := OffsetFirstPoint.x - Point.x;
  Offset.y := OffsetFirstPoint.y - Point.y;
end;

procedure TScrollTool.PropertiesListCreate;
begin

end;

procedure TRectZoomTool.FigureCreate(Point: TPoint);
begin
  SetLength(Figures, length(Figures) + 1);
  Figures[high(Figures)] := TFrame.Create;
  Drawing := true;
  with (Figures[high(Figures)] as TFrame) do
    begin
      SetLength(Points, 2);
      Points[0] := Scrn2Wrld(Point);
      Points[1] := Scrn2Wrld(Point);
      PenStyle := psSolid;
      PenColor := clBlack;
      Width := 1;
    end;
end;

procedure TRectZoomTool.AddPoint(Point: TPoint);
begin
  Figures[high(Figures)].Points[1] := Scrn2Wrld(Point);
end;

procedure TRectZoomTool.MouseUp(Point: TPoint;RButton: Boolean;APanel: TPanel);
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
        PointZoom(DoublePoint(Point.x, Point.y),RButton, Mainform.PaintBox1.Height,
          Mainform.PaintBox1.Width);
    end;
  SetLength(Figures, length(Figures) - 1);
  Mainform.ZoomEdit.Text := IntToStr(round(Zoom));
end;

procedure TRectZoomTool.PropertiesListCreate;
begin

end;

procedure TSelectTool.FigureCreate(Point: TPoint);
begin
    SetLength(Figures, length(Figures) + 1);
  Figures[high(Figures)] := TFrame.Create;
  Drawing := true;
  with (Figures[high(Figures)] as TFrame) do
    begin
      SetLength(Points, 2);
      Points[0] := Scrn2Wrld(Point);
      Points[1] := Scrn2Wrld(Point);
      PenStyle := psSolid;
      PenColor := clBlack;
      Width := 1;
    end;
end;

procedure TSelectTool.AddPoint(Point: TPoint);
begin
  Figures[high(Figures)].Points[1] := Scrn2Wrld(Point);
end;

procedure TSelectTool.MouseUp(Point: TPoint;RButton: Boolean;APanel: TPanel);
var
  ToolRegion: HRGN;
  i: integer;
  Prop: array of TProperty;
  Values: array of integer;
begin
  with Figures[high(Figures)] do
    begin
      Region := CreateRectRgn(
      WorldToScreen(Points[0]).x,WorldToScreen(Points[0]).y,
      WorldToScreen(Points[1]).x,WorldToScreen(Points[1]).y);
    end;

  if (not CtrlPressed) then
    begin
      for i :=0 to high(Figures)-1 do
        begin
          if (CombineRgn(ToolRegion,Figures[i].Region,Figures[high(Figures)].Region,RGN_AND)
            <> NullRegion) then
            Figures[i].Selected := false;
        end;
    end;

  with Figures[high(Figures)] do
    begin
      If not((Points[0].X=Points[1].X) and (Points[0].Y=Points[1].Y)) then
        RectSelectTool(Point)
      else
        PointSelectTool(Point);
    end;
  SetLength(Figures, length(Figures) - 1);

  DeletePanelHandler;
  CreatePanelHandler;
  SetLength(Prop,0);
  Prop := SelectProperties;
  Values := SetValueArray(Prop);
  for i := 0 to High(Prop) do
    if Prop[i] <> Nil then
    Prop[i].ObjectsCreate(APanel, Values[i]);
end;

procedure TSelectTool.RectSelectTool(Point: TPoint);
var
  i:integer;
  ToolRegio: HRGN;
begin
    for i := 0 to high(Figures)-1 do
    begin
        DeleteObject(Figures[i].Region);
        Figures[i].CreateRegion;
        ToolRegio := CreateRectRgn(1,1,2,2);
        if (CombineRgn(ToolRegio,Figures[i].Region,Figures[high(Figures)].Region,RGN_AND)
          <> NULLREGION) then
            begin
              if Figures[i].Selected = false then
                Figures[i].Selected := true
              else
                Figures[i].Selected := false;
            end;
        DeleteObject(ToolRegio);
    end;
end;

procedure TSelectTool.PointSelectTool(Point: TPoint);
var
  i:integer;
begin
      for i := high(Figures)-1 downto low(Figures)  do
      begin
        with Figures[i] do
        begin
          DeleteObject(Region);
          CreateRegion;
          if PtInRegion(Region,Point.X,Point.Y)=true then
            begin
              if Selected = false then
                Selected := true
              else
                Selected := false;
            end;
        end;
      end;
end;

procedure TMoverTool.FigureCreate(Point: TPoint);
begin
  APoint := Point;
  Drawing := True;
end;

procedure TMoverTool.AddPoint(Point: TPoint);
var
  i, j: integer;
  P: TPoint;
begin
  P.x := Point.x - APoint.x;
  P.y := Point.y - APoint.y;
  for i:=0 to High(Figures) do
    if Figures[i].Selected then
      for j:=0 to High(Figures[i].Points) do
        begin
        Figures[i].Points[j].X := Figures[i].Points[j].X + Scrn2Wrld(P).X;
        Figures[i].Points[j].Y := Figures[i].Points[j].Y + Scrn2Wrld(P).Y;
        end;
  APoint := Point;
end;

procedure TMoverTool.PropertiesListCreate;
begin

end;

function TSelectTool.SelectProperties:ArrayOfProperty;
var
  i,j,h:integer;
  First,SerchBool:boolean;
  Prop:ArrayOfProperty;
  ATool:TTool;
begin
  First:=true;
  for i:=0 to High(Figures) do
    if Figures[i].Selected then
      begin
        for j:=0 to High(Tools) do
              if Tools[j].FigureClass.ClassName = Figures[i].ClassName then
                begin
                ATool:=Tools[j];
                Break;
                end;
        ATool.PropertiesListCreate;
        if First then
          begin
            SetLength(Prop,Length(ATool.Properties));
            for j:=0 to High(ATool.Properties) do
              Prop[j] := ATool.Properties[j];
            First:=False;
          end
        else
          begin
            for j:=0 to High(Prop) do
              begin
                SerchBool:=false;
                for h:=0 to High(ATool.Properties) do
                  if Prop[j] <> Nil then
                    if Prop[j].ClassName = ATool.Properties[h].ClassName then
                      SerchBool:=true;
                if SerchBool = False then
                  Prop[j] := Nil;
              end;
          end;
      end;
  Result := Prop;
end;

function TSelectTool.SetValueArray(Prop: ArrayOfProperty): TIntegerArray;
var
  i, j, h: integer;
  ATool: TTool;
  Values: array of integer;
begin
  SetLength(Values,Length(Prop));
  for i:=0 to High(Values) do
    Values[i] := -1;
  for i:=0 to High(Figures) do
    if Figures[i].Selected then
      begin
        for j:=0 to High(Tools) do
              if Tools[j].FigureClass.ClassName = Figures[i].ClassName then
                begin
                ATool:=Tools[j];
                Break;
                end;
        ATool.PropertiesListCreate;
        for j:=0 to High(Prop) do
          for h:=0 to High(ATool.Properties) do
            if  Prop[j] <> Nil then
              if Prop[j].ClassName = ATool.Properties[h].ClassName then
                if (ATool.Properties[h].GetPropertyValue(Figures[i]) < Values[j])
                  or ((Values[j] = -1) and (Prop[j] <> Nil)) then
                    Values[j] := ATool.Properties[h].GetPropertyValue(Figures[i]);
      end;
  Result := Values;
end;

function TWidthProperty.GetPropertyValue(F: TFigure): integer;
begin
  Result := (F as TLines).Width;
end;

function TPenStyleProperty.GetPropertyValue(F: TFigure): integer;
begin
  Result := CasePenStyleIndex((F as TLines).PenStyle);
end;

function TBrushStyleProperty.GetPropertyValue(F: TFigure): integer;
begin
  Result := CaseBrushStyleIndex((F as TFigures).BrushStyle);
end;

function TConersCountProperty.GetPropertyValue(F: TFigure): integer;
begin
  Result := (F as TRegularPolygon).ConersCount;
end;

function TWidthRoundingProperty.GetPropertyValue(F: TFigure): integer;
begin
  Result := (F as TRoundRectangle).RWidth;
end;

function THeightRoundingProperty.GetPropertyValue(F: TFigure): integer;
begin
  Result := (F as TRoundRectangle).RHeight;
end;

procedure TSelectTool.PropertiesListCreate;
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

constructor TScrollTool.Create;
begin
  Icon:='ico/Scroll.png';
end;

constructor TRectZoomTool.Create;
begin
  Icon:='ico/rectzoom.png';
end;

constructor TSelectTool.Create;
begin
  Icon:='ico/Select.png';
end;

constructor TMoverTool.Create;
begin
  Icon:='ico/Mover.png';
end;

initialization

RegisterTool(TPolylineTool.Create, TPolyline);
RegisterTool(TRectangleTool.Create, TRectangle);
RegisterTool(TRoundRectangleTool.Create, TRoundRectangle);
RegisterTool(TEllipseTool.Create, TEllipse);
RegisterTool(TLineTool.Create, TLine);
RegisterTool(TRegularPolygonTool.Create, TRegularPolygon);
RegisterTool(TScrollTool.Create, Nil);
RegisterTool(TRectZoomTool.Create, Nil);
RegisterTool(TSelectTool.Create, Nil);
RegisterTool(TMoverTool.Create, Nil);

end.

