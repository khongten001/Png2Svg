unit Png2Svg.Classes;

interface

uses
  System.Classes, System.SysUtils, Vcl.Imaging.pngimage;

type
  TPng2SvgOption = (psIgnoreAlpha, psNoPixelMerge, psMinify, psOutputSvg, psSaveSvg, psShowLog);
  TPng2SvgOptions = set of TPng2SvgOption;

  TPng2Svg = class(TObject)
  private type
    TColorRect = record
      Alpha: Byte;
      RValue: Byte;
      GValue: Byte;
      BValue: Byte;
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
    function MergePixels(const APngImage: TPngImage): TArray<TColorRect>;
    function MinifyXML(const AXML: string): string;
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
  Winapi.Windows, System.Diagnostics, System.UITypes, Vcl.Graphics;

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

function TPng2Svg.GetSVG: string;
begin
  Result := FSVGStrings.Text;
end;

function TPng2Svg.MergePixels(const APngImage: TPngImage): TArray<TColorRect>;
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

  SetLength(Result, 1024);
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

      if LRectsCount >= Length(Result) then
        SetLength(Result, Length(Result) * 2);

      with Result[LRectsCount] do
      begin
        X := LX;
        Y := LY;
        Width := LWidth;
        Height := LHeight;
        RValue := GetRValue(LColor);
        GValue := GetGValue(LColor);
        BValue := GetBValue(LColor);
        Alpha := LAlpha;
      end;

      Inc(LRectsCount);
    end;

  SetLength(Result, LRectsCount);
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

procedure TPng2Svg.Convert;
const
  SVG_START_TAG = '<svg width="%d" height="%d" xmlns="http://www.w3.org/2000/svg">';
  SVG_RECT_TAG  = '  <rect x="%d" y="%d" width="%d" height="%d" fill="rgb(%d,%d,%d)" fill-opacity="%s" />';
  SVG_END_TAG   = '</svg>';
var
  LPngImage: TPngImage;
  LFormatSettings: TFormatSettings;
  LRects: TArray<TColorRect>;
  LRect: TColorRect;
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

      LFormatSettings := TFormatSettings.Create;
      LFormatSettings.DecimalSeparator := '.';

      LRects := MergePixels(LPngImage);

      for LRect in LRects do
        Add(Format(SVG_RECT_TAG, [LRect.X, LRect.Y, LRect.Width, LRect.Height, LRect.RValue, LRect.GValue, LRect.BValue,
          FormatFloat('0.######', LRect.Alpha / 255, LFormatSettings)]));

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
