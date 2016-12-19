unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Grids, LCLIntf, LCLType, Buttons, GraphMath, Math, FPCanvas, TypInfo,
  Spin, about, ufigures, utools, uscale, Types;

type

  { TMainform }

  TMainform = class(TForm)
    ColorDialog: TColorDialog;
    LoadButton: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveButton: TMenuItem;
    SaveAsButton: TMenuItem;
    SaveDialog: TSaveDialog;
    ShowAllButton: TMenuItem;
    VertScroll: TScrollBar;
    HorzScroll: TScrollBar;
    Palette: TDrawGrid;
    Menu1: TMainMenu;
    MenuItem1: TMenuItem;
    Exitbutton: TMenuItem;
    MenuItem3: TMenuItem;
    Aboutbutton: TMenuItem;
    Clearbutton: TMenuItem;
    PaintBox1: TPaintBox;
    ZoomEdit: TSpinEdit;
    ToolPanel: TPanel;
    panelColor1: TPanel;
    panelColor2: TPanel;
    procedure ClearbuttonClick(Sender: TObject);
    procedure ExitbuttonClick(Sender: TObject);
    procedure AboutbuttonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LoadButtonClick(Sender: TObject);
    procedure PaintBox1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SaveAsButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure ScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnPaint(Sender: TObject);
    procedure PaletteDblClick(Sender: TObject);
    procedure PaletteDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure PaletteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShowAllButtonClick(Sender: TObject);
    procedure ToolClick(Sender: TObject);
    procedure zoomeditChange(Sender: TObject);
    procedure DeletePropertyPanel;
    procedure CreatePropertyPanel;
    procedure SaveFile(APictureName: string);
    procedure WriteTitle;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  PalletColors: array of array of TColor;
  ChoosenTool: TTool;
  Figures: array of TFigure;
  Mainform: TMainform;
  Drawing: boolean;
  Color1: TColor = clBlack;
  Color2: TColor = clWhite;
  PositionProgramChange: boolean;
  ChooseFirstTool: boolean = true;
  PictureName:String = 'New Picture';
  PreviousPicture: String;
  PictureChanged: boolean = false;
implementation

{$R *.lfm}

{ TMainform }

procedure TMainform.ExitbuttonClick(Sender: TObject);
begin
  Mainform.Close;
end;


procedure TMainform.ClearbuttonClick(Sender: TObject);
begin
  SetLength(Figures, 0);
  Zoom := 100;
  VertScroll.Max := 578;
  VertScroll.Min := 0;
  VertScroll.Position := 0;
  HorzScroll.Max := 754;
  HorzScroll.Min := 0;
  HorzScroll.Position := 0;
  MinPoint := DoublePoint(0,0);
  MinPoint := DoublePoint(0,0);
  PaintBox1.Refresh;
end;

procedure TMainform.AboutbuttonClick(Sender: TObject);
begin
  Aboutform.ShowModal();
end;

procedure TMainform.FormCreate(Sender: TObject);
var
  i, j: integer;
  ToolBtn: TSpeedButton;
  ToolIcon: TBitmap;
begin
  PaintBox1.Canvas.Pen.Color := Color1;
  PaintBox1.Canvas.Brush.Color := Color2;
  InvalidateHandler:=@Invalidate;
  CreatePanelHandler:=@CreatePropertyPanel;
  DeletePanelHandler:=@DeletePropertyPanel;

  SetLength(PalletColors, Palette.ColCount);
  for i := 0 to Palette.ColCount - 1 do
    begin
      SetLength(PalletColors[i], Palette.RowCount);
        for j := 0 to Palette.RowCount - 1 do
          PalletColors[i][j] := RGBToColor(floor((i - 1) /
            ((Palette.ColCount - 1) - 1) * 255),
            floor(j / (Palette.RowCount - 1) * 255),
            floor((((Palette.ColCount - 1) - 1 - (i - 1)) *
            (Palette.RowCount - 1 - j))
            / ((Palette.RowCount - 1) * ((Palette.ColCount - 1) - 1)) * 255));
    end;
  PalletColors[0][0] := clBlack;
  PalletColors[Palette.RowCount - 1][Palette.ColCount - 1] := clWhite;

  for i := 0 to High(Tools) do
    begin
      ToolBtn := TSpeedButton.Create(Mainform);
      ToolIcon := TBitmap.Create;
      with TPicture.create do
        begin
          LoadFromFile(Tools[i].Icon);
          ToolIcon.Assign(Graphic);
        end;
      ToolBtn.Transparent := True;
      ToolIcon.Transparent := true;
      ToolBtn.Glyph := ToolIcon;
      ToolBtn.Width := 32;
      ToolBtn.Height := 32;
      ToolBtn.Top := (i div 4) * 32;
      ToolBtn.Left :=(i mod 4) * 32;
      ToolBtn.Tag := i;
      ToolBtn.GroupIndex := 1;
      ToolBtn.Down := i = 0;
      ToolBtn.OnClick := @ToolClick;
      ToolBtn.Parent := ToolPanel;
      if ChooseFirstTool then
        ToolBtn.Click;
      ChooseFirstTool := false;
  end;
end;

procedure TMainform.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_CONTROL then
    CtrlPressed:=true;
end;

procedure TMainform.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_CONTROL then
    CtrlPressed:=False;
end;

procedure TMainform.LoadButtonClick(Sender: TObject);
var
  i,n,j: integer;
  s: string;
begin
  OpenDialog.Filter := 'Picture|*.pnt|';
  OpenDialog.Title := 'Load';
  OpenDialog.Execute;

  PictureName := OpenDialog.FileName;

  AssignFile(input,PictureName);
  Reset(input);
  readln(s);
  if s = '&*_MyPaint_*&' then
    begin
      readln(n);
      SetLength(Figures, n);
      for i:=0 to n-1 do
        begin
          readln(s);
          for j:=0 to High(Tools) do
            if Tools[j].FigureClass <> Nil then
              if Tools[j].FigureClass.ClassName = s then
                begin
                  Figures[i] := Tools[j].FigureClass.Create;
                  Figures[i].Load;
                  Break;
                end;
        end;
    end;

  CloseFile(input);
  Invalidate;
end;

procedure TMainform.PaintBox1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  oldzoom:integer;
begin
  oldzoom := Zoom;
  if WheelDelta > 0 then
    Zoom := Zoom + 10
  else
    Zoom := Zoom - 10;
  if Zoom > 1000 then
    Zoom := 1000;
  if Zoom < 1 then
    Zoom := 1;
  CenterZoom(PaintBox1.Width, PaintBox1.Height, oldzoom);
  ZoomEdit.Text := IntToStr(Zoom);
end;

procedure TMainform.SaveAsButtonClick(Sender: TObject);
begin
    SaveDialog.InitialDir := GetCurrentDir;
    SaveDialog.Title := 'Save As';
    SaveDialog.DefaultExt := 'pnt';
    SaveDialog.Filter := 'Picture|*.pnt|';
    SaveDialog.FileName := PictureName;
    if SaveDialog.Execute then
      begin
      if FileExists(SaveDialog.FileName) then
        begin
          if (Application.MessageBox('Overwrite file?',
            '', MB_ICONQUESTION + MB_YESNO) = IDYES) then
            begin
              SaveFile(SaveDialog.FileName);
          end else
            begin
              SaveAsButton.Click;
              Exit;
            end;
        end else
          begin
            SaveFile(SaveDialog.FileName);
          end;
    end;
end;

procedure TMainform.SaveButtonClick(Sender: TObject);
begin
  if (PreviousPicture = PictureName) then
    SaveFile(PictureName)
  else
    SaveAsButtonClick(TObject.Create);
end;

procedure TMainform.SaveFile(APictureName: string);
var
  i,j: integer;
begin
  AssignFile(output,APictureName);
  rewrite(output);
  writeln('&*_MyPaint_*&');
  writeln(length(Figures));
  for i:=0 to high(Figures) do
    begin
      for j:=0 to high((Figures[i]).Save) do
        writeln((Figures[i]).Save[j]);
    end;
  CloseFile(output);
  PictureName := APictureName;
  PreviousPicture := APictureName;
  PictureChanged:=false;
  WriteTitle;
end;

procedure TMainform.WriteTitle;
begin
  Mainform.Caption := PictureName;
  if PictureChanged then
    Mainform.Caption := Mainform.Caption + '*';
  Mainform.Caption := Mainform.Caption + ' -- My Paint';
end;

procedure TMainform.ScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  if not PositionProgramChange then
    begin
      Scroll(VertScroll.Position, HorzScroll.Position);
      Invalidate;
    end;
  PositionProgramChange := false;
end;

procedure TMainform.ToolClick(Sender: TObject);
begin
  ChoosenTool := Tools[(Sender as TSpeedButton).Tag];
  DeletePropertyPanel;
  CreatePropertyPanel;
end;

procedure TMainform.DeletePropertyPanel;
begin
  if FindComponent('PropertyPanel') <> nil then
    FindComponent('PropertyPanel').Free;
end;

procedure TMainform.CreatePropertyPanel;
var
  PropertyPanel:TPanel;
begin
  PropertyPanel := TPanel.Create(Mainform);
  PropertyPanel.Parent := Mainform;
  PropertyPanel.Anchors := [akTop,akRight];
  PropertyPanel.Name := 'PropertyPanel';
  PropertyPanel.Caption := '';
  PropertyPanel.Width := 100;
  PropertyPanel.Height := 200;
  PropertyPanel.Top := 350;
  PropertyPanel.Left := 750;
  ChoosenTool.PropertiesCreate(PropertyPanel);
end;

procedure TMainform.zoomeditChange(Sender: TObject);
var
  oldzoom: double;
begin
  oldzoom := Zoom;
  Zoom := zoomedit.Value;
  CenterZoom(PaintBox1.Width, PaintBox1.Height, oldzoom);
  Invalidate;
end;

procedure TMainform.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PictureChanged := True;
  WriteTitle;
  PaintBox1.Canvas.Pen.Color := Color1;
  ChoosenTool.FigureCreate(Point(X, Y));
  Invalidate;
end;

procedure TMainform.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Drawing then
    begin
      ChoosenTool.AddPoint(Point(X, Y));
    end;
  Invalidate;
end;

procedure TMainform.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  RButton: Boolean;
begin
  Drawing := false;
  if Button = mbLeft then
    RButton := false;
  if Button = mbRight then
    RButton := true;
  ChoosenTool.MouseUp(Point(X, Y), RButton,(FindComponent('PropertyPanel') as TPanel));
  Invalidate;
end;

procedure TMainform.OnPaint(Sender: TObject);
var
  i: integer;
  MaxP, MinP: TDoublePoint;
begin
  for i:=0 to Length(Figures)-1 do
    Figures[i].Draw(PaintBox1.Canvas);

  if MaxPoint.X > PaintBox1.Width then
    MaxP.X := MaxPoint.X
  else
    MaxP.X := PaintBox1.Width;
  if MaxPoint.Y > PaintBox1.Height then
    MaxP.Y := MaxPoint.Y
  else
    MaxP.Y := PaintBox1.Height;
  if MinPoint.X < MinP.X then MinP.X := MinPoint.X;
  if MinPoint.Y < MinP.Y then MinP.Y := MinPoint.Y;

  if Offset.x + PaintBox1.Width > MaxP.X then
    MaxP.X := Offset.x + PaintBox1.Width;
  if Offset.x < MinP.X then
    MinP.X := Offset.x;
  if Offset.y + PaintBox1.Height > MaxP.Y then
    MaxP.Y := Offset.y + PaintBox1.Height;
  if Offset.y < MinP.Y then
    MinP.Y := Offset.y;

  HorzScroll.Min := round(MinP.X * Zoom / 100);
  HorzScroll.Max := round(MaxP.X * Zoom / 100);
  VertScroll.Min := round(MinP.Y * Zoom / 100);
  VertScroll.Max := round(MaxP.Y * Zoom / 100);

  PositionProgramChange := true;
  HorzScroll.Position := Offset.x;
  PositionProgramChange := true;
  VertScroll.Position := Offset.y;
end;

procedure TMainform.PaletteDblClick(Sender: TObject);
begin
  if ColorDialog.Execute then
    begin
      PalletColors[(Sender as TDrawGrid).Col, (Sender as TDrawGrid).Row] :=
        ColorDialog.Color;
    end;
end;

procedure TMainform.PaletteDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  Palette.Canvas.Brush.Color := PalletColors[aCol, aRow];
  Palette.Canvas.FillRect(aRect);
end;

procedure TMainform.PaletteMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
aCol, aRow: Integer;
begin
  Palette.MouseToCell(X, Y, aCol, aRow);
  if Button = mbLeft then
    begin
      Color1 := PalletColors[aCol, aRow];
      panelColor1.Color := Color1;
      if ChoosenTool.ClassName = TSelectTool.ClassName then
        ColorChange(Color1, Color2, True);
    end;
  if Button = mbRight then
    begin
      Color2 := PalletColors[aCol, aRow];
      panelColor2.Color := Color2;
      if ChoosenTool.ClassName = TSelectTool.ClassName then
        ColorChange(Color1, Color2, False);
    end;
end;

procedure TMainform.ShowAllButtonClick(Sender: TObject);
begin
  RectZoom(PaintBox1.Height, PaintBox1.Width, MinPoint, MaxPoint);
  zoomedit.Text := IntToStr(Zoom);
end;

end.

