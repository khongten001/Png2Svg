program Png2Svg;

{$APPTYPE CONSOLE}

{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

uses
  System.Classes,
  System.SysUtils,
  Png2Svg.Classes in 'Png2Svg.Classes.pas';

var
  LOptions: TPng2SvgOptions;
  LSource, LDestination: string;

  procedure GetParams;
  var
    LIndex: Integer;
    LParam: string;
  begin
    Include(LOptions, psSaveSvg);

    for LIndex := 1 to ParamCount do
    begin
      LParam := ParamStr(LIndex);

      if LParam = '-ia' then
        Include(LOptions, psIgnoreAlpha)
      else
      if LParam = '-log' then
        Include(LOptions, psShowLog)
      else
      if LParam = '-nm' then
        Include(LOptions, psNoPixelMerge)
      else
      if LParam = '-m' then
        Include(LOptions, psMinify)
      else
      if LParam = '-o' then
        Include(LOptions, psOutputSvg)
      else
      if LParam = '-p' then
        Include(LOptions, psUsePathElement)
      else
      if LSource.IsEmpty then
        LSource := LParam
      else
      if LDestination.IsEmpty then
        LDestination := LParam
      else
        Break;
    end;
  end;

  function GetDestination: string;
  const
    SVG_EXT = '.svg';
  var
    LPath: string;
  begin
    Result := LDestination;

    if Result.IsEmpty then
      Result := ChangeFileExt(LSource, SVG_EXT)
    else
    if DirectoryExists(Result) then
      Result := IncludeTrailingPathDelimiter(Result) + ChangeFileExt(ExtractFilename(LSource), SVG_EXT)
    else
    begin
      LPath := IncludeTrailingPathDelimiter(ExtractFilepath(LSource)) + IncludeTrailingPathDelimiter(Result);

      if DirectoryExists(LPath) then
        Result := LPath + ChangeFileExt(ExtractFilename(LSource), SVG_EXT)
      else
      if not DirectoryExists(ExtractFilepath(Result)) and not ExtractFileExt(Result).IsEmpty then
        Result := IncludeTrailingPathDelimiter(ExtractFilepath(LSource)) + Result
      else
      if not FileExists(Result) then
        raise EDirectoryNotFoundException.Create(Format('The specified directory "%s" is not found', [Result]));
    end;
  end;

  function GetPngFiles: TStringList;
  var
    LSearchRec: TSearchRec;
    LPath: string;
  begin
    Result := TStringList.Create;

    LPath := IncludeTrailingPathDelimiter(LSource);

    if FindFirst(LPath + '*.png', faAnyFile, LSearchRec) = 0 then
    begin
      repeat
        Result.Add(LPath + LSearchRec.Name);
      until FindNext(LSearchRec) <> 0;

      FindClose(LSearchRec);
    end;
  end;

var
  LIndex: Integer;
  LPngFiles: TStringList;
begin
  try
    GetParams;

    if LSource.IsEmpty then
    begin
      Writeln('No png file or directory specified.');
      Exit;
    end;

    with TPng2Svg.Create(LOptions) do
    try
      if DirectoryExists(LSource) then
      begin
        LPngFiles := GetPngFiles;
        try
          for LIndex := 0 to LPngFiles.Count - 1 do
          begin
            LSource := LPngFiles[LIndex];

            FilenamePng := LSource;
            FilenameSvg := GetDestination;

            Convert;
          end;
        finally
          LPngFiles.Free;
        end;
      end
      else
      if FileExists(LSource) then
      begin
        FilenamePng := LSource;
        FilenameSvg := GetDestination;

        Convert;
      end
      else
        raise EFileNotFoundException.Create(Format('The specified file "%s" is not found', [LSource]));
    finally
      Free;
    end;
  except
    on E: Exception do
      Writeln(E.Message);
  end;
end.
