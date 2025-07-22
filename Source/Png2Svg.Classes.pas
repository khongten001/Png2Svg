unit Png2Svg.Classes;

interface

uses
  System.Classes, System.SysUtils, Vcl.Imaging.pngimage, Vcl.Graphics;

type
  TPng2SvgOption = (psIgnoreAlpha, psNoPixelMerge, psMinify, psOutputSvg, psSaveSvg, psShowLog);
  TPng2SvgOptions = set of TPng2SvgOption;

  TPng2Svg = class(TObject)
  private type
    TColorRect = record
      Alpha: Byte;
      Color: TColor;
      Height: Integer;
      Width: Integer;
      X: Integer;
      Y: Integer;
    end;
  strict private
    FFilenamePng: string;
    FFilenameSvg: string;
    FOptions: TPng2SvgOptions;
    FSVGStrings: TStringList;
    function GetTimeStamp: string;
    function MinifyXML(const AXML: string): string;
    procedure GenerateSVGFromRects(const ARects: TArray<TColorRect>; const AOutput: TStrings);
    procedure MergePixels(var ARects: TArray<TColorRect>; const APngImage: TPngImage);
  public
    constructor Create(const AOptions: TPng2SvgOptions);
    destructor Destroy; override;
    function GetSVG: string;
    procedure Convert;
    property FilenamePng: string read FFilenamePng write FFilenamePng;
    property FilenameSvg: string read FFilenameSvg write FFilenameSvg;
    property Options: TPng2SvgOptions read FOptions write FOptions;
  end;

implementation

uses
  Winapi.Windows, System.Generics.Collections, System.Diagnostics, System.UITypes;

constructor TPng2Svg.Create(const AOptions: TPng2SvgOptions);
begin
  inherited Create;

  FSVGStrings := TStringList.Create;
  FOptions := AOptions;
end;

destructor TPng2Svg.Destroy;
begin
  FSVGStrings.Free;

  inherited Destroy;
end;

function TPng2Svg.GetTimeStamp: string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);
end;

function TPng2Svg.MinifyXML(const AXML: string): string;
begin
  Result := '';

  var LText := '';
  var LOutsideTag := False;
  var LPChar := PChar(AXML);

  while LPChar^ <> #0 do
  begin
    if LPChar^ = '<' then
    begin
      Result := Result + Trim(LText);
      LText := '';
      LOutsideTag := False;
    end;

    if LOutsideTag then
    begin
      if (LPChar^ <> #10) and (LPChar^ <> #13) then
        LText := LText + LPChar^
      else
        LText := LText + #32
    end
    else
    if (LPChar^ <> #10) and (LPChar^ <> #13) then
      Result := Result + LPChar^;

    if LPChar^ = '>' then
      LOutsideTag := True;

    Inc(LPChar);
  end;
end;

procedure TPng2Svg.GenerateSVGFromRects(const ARects: TArray<TColorRect>; const AOutput: TStrings);
type
  TGroup = record
    Color: TColor;
    Alpha: Byte;
    Rects: TList<TColorRect>;
  end;
var
  LGroups: TList<TGroup>;
  LRect: TColorRect;
  LIndex, LGroupIndex: Integer;
  LGroup: TGroup;
  LFound: Boolean;
  LColorStr: string;
  LOpacity: string;

  function ColorToHex(const AColor: TColor): string;
  begin
    Result := IntToHex(GetRValue(AColor), 2) + IntToHex(GetGValue(AColor), 2) + IntToHex(GetBValue(AColor), 2);
  end;

begin
  LGroups := TList<TGroup>.Create;
  try
    for LRect in ARects do
    begin
      LGroupIndex := -1;
      LFound := False;

      for LIndex := 0 to LGroups.Count - 1 do
      begin
        LGroup := LGroups[LIndex];

        if (LGroup.Color = LRect.Color) and (LGroup.Alpha = LRect.Alpha) then
        begin
          LFound := True;
          LGroupIndex := LIndex;

          Break;
        end;
      end;

      if not LFound then
      begin
        LGroup.Color := LRect.Color;
        LGroup.Alpha := LRect.Alpha;
        LGroup.Rects := TList<TColorRect>.Create;

        LGroups.Add(LGroup);

        LGroupIndex := LGroups.Count - 1;
      end;

      LGroups[LGroupIndex].Rects.Add(LRect);
    end;

    for LIndex := 0 to LGroups.Count - 1 do
    begin
      LGroup := LGroups[LIndex];

      if LGroup.Color = 0 then
        LColorStr := ''
      else
        LColorStr := ' fill="#' + ColorToHex(LGroup.Color) + '"';

      if LGroup.Alpha = 255 then
        LOpacity := ''
      else
        LOpacity := ' fill-opacity="' + FormatFloat('.###', LGroup.Alpha / 255, TFormatSettings.Invariant) + '"';

      AOutput.Add('<g' + LColorStr + LOpacity + '>');

      for LRect in LGroup.Rects do
        AOutput.Add(Format('  <rect x="%d" y="%d" width="%d" height="%d"/>', [LRect.X, LRect.Y, LRect.Width, LRect.Height]));

      AOutput.Add('</g>');

      LGroup.Rects.Free;
    end;
  finally
    LGroups.Free;
  end;
end;

procedure TPng2Svg.MergePixels(var ARects: TArray<TColorRect>; const APngImage: TPngImage);
var
  LVisited: array of array of Boolean;
  LPixelCache: array of array of TColor;
  LAlphaCache: array of PByteArray;
  LRectsCount: Integer;
  LPngHeight, LPngWidth: Integer;
  LX, LY, LExpandedX, LExpandedY: Integer;
  LAlpha: Byte;
  LColor: TColor;
  LWidth, LHeight: Integer;
  LCanExpand: Boolean;
begin
  LPngHeight := APngImage.Height;
  LPngWidth := APngImage.Width;

  SetLength(LVisited, LPngHeight, LPngWidth);
  SetLength(LPixelCache, LPngHeight);

  for LY := 0 to LPngHeight - 1 do
    SetLength(LPixelCache[LY], LPngWidth);

  SetLength(LAlphaCache, LPngHeight);

  SetLength(ARects, 1024);

  LRectsCount := 0;

  for LY := 0 to LPngHeight - 1 do
  begin
    LAlphaCache[LY] := APngImage.AlphaScanline[LY];

    for LX := 0 to LPngWidth - 1 do
      LPixelCache[LY][LX] := ColorToRGB(APngImage.Pixels[LX, LY]);
  end;

  for LY := 0 to LPngHeight - 1 do
    for LX := 0 to LPngWidth - 1 do
    begin
      if LVisited[LY, LX] then
        Continue;

      if Assigned(LAlphaCache[LY]) then
        LAlpha := LAlphaCache[LY][LX]
      else
        LAlpha := 255;

      if (LAlpha = 0) or (LAlpha <> 255) and (psIgnoreAlpha in FOptions) then
      begin
        LVisited[LY, LX] := True;
        Continue;
      end;

      LColor := LPixelCache[LY][LX];

      // Expand width
      LWidth := 1;

      if not (psNoPixelMerge in FOptions) then
      while (LX + LWidth < LPngWidth) and not LVisited[LY, LX + LWidth] do
      begin
        if Assigned(LAlphaCache[LY]) then
        begin
          if LAlphaCache[LY][LX + LWidth] <> LAlpha then
            Break;
        end
        else
        if LAlpha <> 255 then
          Break;

        if LPixelCache[LY][LX + LWidth] <> LColor then
          Break;

        Inc(LWidth);
      end;

      // Expand height
      LHeight := 1;

      if not (psNoPixelMerge in FOptions) then
      while LY + LHeight < LPngHeight do
      begin
        LCanExpand := True;

        for LExpandedX := 0 to LWidth - 1 do
        begin
          if LVisited[LY + LHeight, LX + LExpandedX] then
          begin
            LCanExpand := False;
            Break;
          end;

          if Assigned(LAlphaCache[LY + LHeight]) then
          begin
            if LAlphaCache[LY + LHeight][LX + LExpandedX] <> LAlpha then
            begin
              LCanExpand := False;
              Break;
            end;
          end
          else
          if LAlpha <> 255 then
          begin
            LCanExpand := False;
            Break;
          end;

          if LPixelCache[LY + LHeight][LX + LExpandedX] <> LColor then
          begin
            LCanExpand := False;
            Break;
          end;
        end;

        if not LCanExpand then
          Break;

        Inc(LHeight);
      end;

      for LExpandedY := 0 to LHeight - 1 do
        for LExpandedX := 0 to LWidth - 1 do
          LVisited[LY + LExpandedY, LX + LExpandedX] := True;

      if LRectsCount >= Length(ARects) then
        SetLength(ARects, Length(ARects) * 2);

      with ARects[LRectsCount] do
      begin
        X := LX;
        Y := LY;
        Width := LWidth;
        Height := LHeight;
        Color := LColor;
        Alpha := LAlpha;
      end;

      Inc(LRectsCount);
    end;

  SetLength(ARects, LRectsCount);
end;

function TPng2Svg.GetSVG: string;
begin
  Result := FSVGStrings.Text;
end;

procedure TPng2Svg.Convert;
const
  SVG_START_TAG = '<svg width="%d" height="%d" xmlns="http://www.w3.org/2000/svg" shape-rendering="crispEdges">';
  SVG_END_TAG   = '</svg>';
var
  LPngImage: TPngImage;
  LRects: TArray<TColorRect>;
  LStopwatch: TStopwatch;
begin
  if psShowLog in FOptions then
  begin
    WriteLn(GetTimeStamp + ': Starting conversion of ' + FFilenamePng);
    LStopwatch := TStopwatch.StartNew;
  end;

  with FSVGStrings do
  begin
    Clear;

    LPngImage := TPngImage.Create;
    try
      LPngImage.LoadFromFile(FFilenamePng);

      Add(Format(SVG_START_TAG, [LPngImage.Width, LPngImage.Height]));

      MergePixels(LRects, LPngImage);
      GenerateSVGFromRects(LRects, FSVGStrings);

      Add(SVG_END_TAG);
    finally
      LPngImage.Free;
    end;

    if psShowLog in FOptions then
    begin
      LStopwatch.Stop;
      WriteLn(GetTimeStamp + ': Conversion to ' + FFilenameSvg + ' done in ' + LStopwatch.ElapsedMilliseconds.ToString + 'ms');
    end;

    if psMinify in FOptions then
      Text := MinifyXML(Text);

    if psOutputSvg in FOptions then
      WriteLn(GetSVG)
    else
    if psSaveSvg in FOptions then
    begin
      SaveToFile(FFilenameSvg, TEncoding.UTF8);

      if psShowLog in FOptions then
        WriteLn(GetTimeStamp + ': Svg saved into ' + FFilenameSvg);
    end;
  end;
end;

end.
