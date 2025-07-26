unit Png2Svg.Classes;

interface

uses
  System.Classes, System.SysUtils, Vcl.Graphics, Vcl.Imaging.pngimage;

type
  TPng2SvgOption = (psIgnoreAlpha, psNoPixelMerge, psMinify, psOutputSvg, psSaveSvg, psShowLog, psUsePathElement);
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
    FHeight: Integer;
    FOptions: TPng2SvgOptions;
    FSVGStrings: TStringList;
    FWidth: Integer;
    function ColorToHex(const AColor: TColor): string;
    function GetTimeStamp: string;
    function MakeColorAlphaKey(const AColor: TColor; const AAlpha: Byte): Cardinal;
    function MinifyXML(const AXML: string): string;
    procedure GenerateSVGFromRects(const ARects: TArray<TColorRect>);
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
  Winapi.Windows, System.Diagnostics, System.Generics.Collections, System.UITypes;

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

function TPng2Svg.ColorToHex(const AColor: TColor): string;
var
  LRed, LGreen, LBlue: Byte;
begin
  LRed := GetRValue(AColor);
  LGreen := GetGValue(AColor);
  LBlue := GetBValue(AColor);

  if (LRed shr 4 = LRed and $F) and (LGreen shr 4 = LGreen and $F) and (LBlue shr 4 = LBlue and $F) then
    Result := Format('#%X%X%X', [LRed and $F, LGreen and $F, LBlue and $F])
  else
    Result := Format('#%.2X%.2X%.2X', [LRed, LGreen, LBlue]);
end;

function TPng2Svg.MakeColorAlphaKey(const AColor: TColor; const AAlpha: Byte): Cardinal;
var
  LRGB: Cardinal;
begin
  LRGB := Cardinal(ColorToRGB(AColor)) and $FFFFFF;

  Result := Cardinal(AAlpha) shl 24 or LRGB;
end;

procedure TPng2Svg.GenerateSVGFromRects(const ARects: TArray<TColorRect>);
const
  SVG_START_TAG = '<svg width="%d" height="%d" xmlns="http://www.w3.org/2000/svg" shape-rendering="crispEdges">';
  SVG_END_TAG   = '</svg>';
type
  TGroup = record
    Color: TColor;
    Alpha: Byte;
    Rects: TArray<TColorRect>;
    RectCount: Integer;
  end;
var
  LGroupMap: TDictionary<Cardinal, Integer>;
  LKey: Cardinal;
  LGroups: TArray<TGroup>;
  LGroupsCount: Integer;
  LRect: TColorRect;
  LIndex, LGroupIndex, LRectIndex: Integer;
  LGroup: TGroup;
  LColorStr: string;
  LOpacity: string;
  LStringBuilder: TStringBuilder;
  LLastX, LLastY, LX, LY: Integer;
begin
  FSVGStrings.Clear;
  FSVGStrings.Add(Format(SVG_START_TAG, [FWidth, FHeight]));

  SetLength(LGroups, 512);
  LGroupsCount := 0;

  LGroupMap := TDictionary<Cardinal, Integer>.Create;
  try
    for LRect in ARects do
    begin
      LKey := MakeColorAlphaKey(LRect.Color, LRect.Alpha);

      if not LGroupMap.TryGetValue(LKey, LGroupIndex) then
      begin
        if LGroupsCount >= Length(LGroups) then
          SetLength(LGroups, LGroupsCount * 2);

        with LGroups[LGroupsCount] do
        begin
          Color := LRect.Color;
          Alpha := LRect.Alpha;
          SetLength(Rects, 256);
          RectCount := 0;
        end;

        LGroupIndex := LGroupsCount;
        LGroupMap.Add(LKey, LGroupIndex);
        Inc(LGroupsCount);
      end;

      with LGroups[LGroupIndex] do
      begin
        if RectCount >= Length(Rects) then
          SetLength(Rects, RectCount * 2);

        Rects[RectCount] := LRect;
        Inc(RectCount);
      end;
    end;
  finally
    SetLength(LGroups, LGroupsCount);
    LGroupMap.Free;
  end;

  for LIndex := 0 to LGroupsCount - 1 do
  begin
    LGroup := LGroups[LIndex];

    SetLength(LGroup.Rects, LGroup.RectCount);

    if LGroup.Color = 0 then
      LColorStr := ''
    else
      LColorStr := ' fill="' + ColorToHex(LGroup.Color) + '"';

    if LGroup.Alpha = 255 then
      LOpacity := ''
    else
      LOpacity := ' fill-opacity="' + FormatFloat('.###', LGroup.Alpha / 255, TFormatSettings.Invariant) + '"';

    if psUsePathElement in FOptions then
    begin
      LStringBuilder := TStringBuilder.Create;
      try
        LStringBuilder.Append('  <path' + LColorStr + LOpacity + ' d="');

        LLastX := 0;
        LLastY := 0;

        for LRectIndex := 0 to High(LGroup.Rects) do
        with LGroup.Rects[LRectIndex] do
        begin
          if LRectIndex = 0 then
            LStringBuilder.Append(Format('M%d %dh%dv%dH%dZ', [X, Y, Width, Height, X]))
          else
          begin
            LX := X - LLastX;
            LY := Y - LLastY;

            LStringBuilder.Append(Format('m%d %dh%dv%dh%dZ', [LX, LY, Width, Height, -Width]));
          end;

          LLastX := X;
          LLastY := Y;
        end;
     
        FSVGStrings.Add(LStringBuilder.ToString + '"/>');
      finally
        LStringBuilder.Free;
      end;
    end
    else
    begin
      FSVGStrings.Add('<g' + LColorStr + LOpacity + '>');

      for LRect in LGroup.Rects do
        FSVGStrings.Add(Format('  <rect x="%d" y="%d" width="%d" height="%d"/>', [LRect.X, LRect.Y, LRect.Width, LRect.Height]));

      FSVGStrings.Add('</g>');
    end;
  end;

  FSVGStrings.Add(SVG_END_TAG);
end;

procedure TPng2Svg.MergePixels(var ARects: TArray<TColorRect>; const APngImage: TPngImage);
var
  LVisited: array of array of Boolean;
  LPixelCache: array of array of TColor;
  LAlphaCache: array of PByteArray;
  LRectsCount: Integer;
  LX, LY, LExpandedX, LExpandedY: Integer;
  LAlpha: Byte;
  LColor: TColor;
  LWidth, LHeight: Integer;
  LCanExpand: Boolean;
begin
  SetLength(LVisited, FHeight, FWidth);
  SetLength(LPixelCache, FHeight);

  for LY := 0 to FHeight - 1 do
    SetLength(LPixelCache[LY], FWidth);

  SetLength(LAlphaCache, FHeight);

  SetLength(ARects, 1024);

  LRectsCount := 0;

  for LY := 0 to FHeight - 1 do
  begin
    LAlphaCache[LY] := APngImage.AlphaScanline[LY];

    for LX := 0 to FWidth - 1 do
      LPixelCache[LY][LX] := ColorToRGB(APngImage.Pixels[LX, LY]);
  end;

  for LY := 0 to FHeight - 1 do
    for LX := 0 to FWidth - 1 do
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
      while (LX + LWidth < FWidth) and not LVisited[LY, LX + LWidth] do
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
      while LY + LHeight < FHeight do
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

  LPngImage := TPngImage.Create;
  try
    LPngImage.LoadFromFile(FFilenamePng);

    FWidth := LPngImage.Width;
    FHeight := LPngImage.Height;

    MergePixels(LRects, LPngImage);

    if psShowLog in FOptions then
      WriteLn(GetTimeStamp + ': Rect count ' + Length(LRects).ToString);

    GenerateSVGFromRects(LRects);
  finally
    LPngImage.Free;
  end;

  if psShowLog in FOptions then
  begin
    LStopwatch.Stop;
    WriteLn(GetTimeStamp + ': Conversion to ' + FFilenameSvg + ' done in ' + LStopwatch.ElapsedMilliseconds.ToString + 'ms');
  end;

  if psMinify in FOptions then
    FSVGStrings.Text := MinifyXML(FSVGStrings.Text);

  if psOutputSvg in FOptions then
    WriteLn(GetSVG)
  else
  if psSaveSvg in FOptions then
  begin
    FSVGStrings.SaveToFile(FFilenameSvg, TEncoding.UTF8);

    if psShowLog in FOptions then
      WriteLn(GetTimeStamp + ': Svg saved into ' + FFilenameSvg);
  end;
end;

end.
